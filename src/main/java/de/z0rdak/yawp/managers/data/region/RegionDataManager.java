package de.z0rdak.yawp.managers.data.region;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.config.server.RegionConfig;
import de.z0rdak.yawp.core.flag.BooleanFlag;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.AbstractMarkableRegion;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.util.constants.*;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.server.MinecraftServer;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.registry.Registry;
import net.minecraft.util.text.TranslationTextComponent;
import net.minecraft.world.World;
import net.minecraft.world.server.ServerWorld;
import net.minecraft.world.storage.DimensionSavedDataManager;
import net.minecraft.world.storage.WorldSavedData;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.event.entity.player.PlayerEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.event.server.FMLServerStartingEvent;
import net.minecraftforge.fml.server.ServerLifecycleHooks;
import org.apache.commons.lang3.NotImplementedException;

import javax.annotation.Nullable;
import java.util.*;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.core.flag.FlagType.BOOLEAN_FLAG;

@EventBusSubscriber(modid = YetAnotherWorldProtector.MODID, value = Dist.DEDICATED_SERVER)
public class RegionDataManager extends WorldSavedData {

    private static final String DATA_NAME = YetAnotherWorldProtector.MODID + "-dimensions";
    private static Map<RegistryKey<World>, DimensionRegionCache> dimCacheMap = new HashMap<>();
    private List<String> dimensionDataNames;
    private static RegionDataManager regionDataCache = new RegionDataManager();

    private RegionDataManager() {
        super(DATA_NAME);
        this.dimensionDataNames = new ArrayList<>();
    }

    public static void save(){
        RegionDataManager.get().setDirty();
    }

    public List<String> getDimensionDataNames(){
        if (this.dimensionDataNames == null) {
            this.dimensionDataNames = new ArrayList<>();
        }
        return this.dimensionDataNames;
    }

    public static RegionDataManager get() {
        if (regionDataCache == null) {
            MinecraftServer server = ServerLifecycleHooks.getCurrentServer();
            if (server != null) {
                ServerWorld overworld = server.overworld();
                if (!overworld.isClientSide) {
                    DimensionSavedDataManager storage = overworld.getDataStorage();
                    regionDataCache = storage.computeIfAbsent(RegionDataManager::new, DATA_NAME);
                }
            }
        }
        return regionDataCache;
    }

    public static void loadRegionData(FMLServerStartingEvent event) {
        try {
            ServerWorld world = Objects.requireNonNull(event.getServer().overworld());
            if (!world.isClientSide) {
                DimensionSavedDataManager storage = world.getDataStorage();
                RegionDataManager data = storage.computeIfAbsent(RegionDataManager::new, DATA_NAME);
                storage.set(data);
                regionDataCache = data;
                YetAnotherWorldProtector.LOGGER.info(new TranslationTextComponent("data.nbt.dimensions.load.success", data.getAllRegionNames().size(), data.getDimensionList().size()).getString());
            }
        } catch (NullPointerException npe) {

            YetAnotherWorldProtector.LOGGER.error(new TranslationTextComponent("data.nbt.dimensions.load.failure").getString());
        }
    }

    @Override
    public void load(CompoundNBT nbt) {
        dimCacheMap.clear();
        CompoundNBT dimensionRegions = nbt.getCompound(NBTConstants.DIMENSIONS);
        YetAnotherWorldProtector.LOGGER.info(new TranslationTextComponent("data.nbt.dimensions.load.amount", dimensionRegions.getAllKeys().size()).getString());
        for (String dimKey : dimensionRegions.getAllKeys()) {
            this.dimensionDataNames.add(dimKey);
            RegistryKey<World> dimension = RegistryKey.create(Registry.DIMENSION_REGISTRY, new ResourceLocation(dimKey));
            dimCacheMap.put(dimension, new DimensionRegionCache(dimensionRegions.getCompound(dimKey)));
        }
    }

    @Override
    public CompoundNBT save(CompoundNBT compound) {
        YetAnotherWorldProtector.LOGGER.info(new TranslationTextComponent("data.nbt.dimensions.save.amount", dimCacheMap.entrySet().size()).getString());
        CompoundNBT dimRegionNbtData = new CompoundNBT();
        for (Map.Entry<RegistryKey<World>, DimensionRegionCache> entry : dimCacheMap.entrySet()) {
            String dimensionName = entry.getValue().getDimensionalRegion().getName();
            dimRegionNbtData.put(dimensionName, entry.getValue().serializeNBT());
        }
        compound.put(NBTConstants.DIMENSIONS, dimRegionNbtData);
        return compound;
    }

    @SubscribeEvent
    public static void addDimKeyOnDimensionChange(PlayerEvent.PlayerChangedDimensionEvent event){
        // if region map does not contain an entry for the dimension traveled to, add it to the map
        if (!event.getPlayer().getCommandSenderWorld().isClientSide) {
            if (dimCacheMap == null) {
                dimCacheMap = new HashMap<>();
            }
            if (!dimCacheMap.containsKey(event.getTo())) {
                DimensionRegionCache cache = RegionDataManager.get().newCacheFor(event.getTo());
                YetAnotherWorldProtector.LOGGER.info("Player traveling to dimension without region data. Init region data for dimension '" + cache.dimensionKey().location() + "'..");
                save();
            }
        }
    }

    @SubscribeEvent
    public static void addDimKeyOnPlayerLogin(PlayerEvent.PlayerLoggedInEvent event){
        // if region map does not contain an entry for the dimension traveled to, add it to the map
        if (!event.getPlayer().getCommandSenderWorld().isClientSide) {
            RegistryKey<World> dim = event.getPlayer().getCommandSenderWorld().dimension();
            if (dimCacheMap == null) {
                dimCacheMap = new HashMap<>();
            }
            if (!dimCacheMap.containsKey(dim)) {
                DimensionRegionCache cache = RegionDataManager.get().newCacheFor(dim);
                YetAnotherWorldProtector.LOGGER.info("Player joining to server in dimension without region data. This should only happen the first time a player is joining. Init region data for dimension '" + cache.dimensionKey().location() + "'..");
            }
        }
    }

    public Collection<String> getAllRegionNames() {
        return dimCacheMap.values().stream()
                .flatMap(regionCache -> regionCache.getRegionNames().stream())
                .collect(Collectors.toList());
    }

    public Collection<String> getRegionNamesFor(RegistryKey<World> dim) {
        return dimCacheMap.get(dim).getRegionNames();
    }

    public Collection<String> getDimensionList() {
        return dimCacheMap.keySet().stream()
                .map(entry -> entry.location().toString())
                .collect(Collectors.toList());
    }

    public Collection<IMarkableRegion> getRegionsFor(RegistryKey<World> dim) {
        return dimCacheMap.get(dim).getRegions();
    }

    @Nullable
    public IMarkableRegion getRegionIn(String regionName, RegistryKey<World> dim) {
        if (dimCacheMap.containsKey(dim)) {
            DimensionRegionCache cache = dimCacheMap.get(dim);
            if (cache.contains(regionName)) {
                return cache.get(regionName);
            }
        }
        return null;
    }

    public boolean containsRegion(RegistryKey<World> dim, IMarkableRegion region) {
        if (dimCacheMap.containsKey(dim)) {
            return dimCacheMap.get(dim).contains(region.getName());
        }
        return false;
    }

    public void setActiveState(Collection<IMarkableRegion> regionsToProcess, boolean activate) {

    }

    @Nullable
    public boolean containsCacheFor(RegistryKey<World> dim) {
        return dimCacheMap.containsKey(dim);
    }

    @Nullable
    public DimensionRegionCache cacheFor(RegistryKey<World> dim) {
        return dimCacheMap.get(dim);
    }

    public List<IFlag> getFlagsForDim(RegistryKey<World> dim){
        DimensionRegionCache dimCache = cacheFor(dim);
        if (dimCache != null) {
            return new ArrayList<>(dimCache.getDimensionalRegion().getFlags());
        }
        return new ArrayList<>();
    }

    public List<String> getFlagsIdsForDim(DimensionRegionCache dimCache){
        if (dimCache != null) {
            return dimCache.getDimensionalRegion().getFlags()
                    .stream()
                    .map(IFlag::getFlagIdentifier)
                    .collect(Collectors.toList());
        }
        return new ArrayList<>();
    }

    public DimensionRegionCache newCacheFor(RegistryKey<World> dim) {
        DimensionRegionCache cache = new DimensionRegionCache(dim);
        RegionConfig.getDefaultDimFlags()
                .stream() // get is fine here, because the config is validated beforehand
                .map(RegionFlag::fromId)
                .forEach(flag -> {
                    switch (flag.type) {
                        case BOOLEAN_FLAG:
                            cache.addFlag(new BooleanFlag(flag));
                            break;
                        case LIST_FLAG:
                            throw new NotImplementedException("");
                        case INT_FLAG:
                            throw new NotImplementedException("");
                    }
                });
        cache.setDimState(RegionConfig.shouldActivateNewDimRegion());
        RegionDataManager.dimCacheMap.put(cache.getDimensionalRegion().getDim(), cache);
        RegionDataManager.get().getDimensionDataNames().add(dimCacheMap.get(dim).getDimensionalRegion().getName());
        save();
        return cache;
    }

    /* hash for checking manipulated world data?*/
    public class DimensionDataEntry {
        private RegistryKey<World> dim;
        private boolean load;
        private String hash;
    }
}
