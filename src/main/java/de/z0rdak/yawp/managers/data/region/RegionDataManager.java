package de.z0rdak.yawp.managers.data.region;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.region.AbstractMarkableRegion;
import de.z0rdak.yawp.core.region.DimensionalRegion;
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
import net.minecraftforge.event.entity.player.PlayerEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.server.FMLServerStartingEvent;
import net.minecraftforge.fml.server.ServerLifecycleHooks;

import javax.annotation.Nullable;
import java.io.IOException;
import java.util.*;
import java.util.stream.Collectors;

@Mod.EventBusSubscriber(modid = YetAnotherWorldProtector.MODID)
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
                YetAnotherWorldProtector.LOGGER.info(new TranslationTextComponent("console.logger.info.data.load.success", data.getAllRegionNames().size(), data.getDimensionList().size()).getString());
            }
        } catch (NullPointerException npe) {
            YetAnotherWorldProtector.LOGGER.error(new TranslationTextComponent("console.logger.error.data.load.failure"));
        }
    }

    @Override
    public void load(CompoundNBT nbt) {
        dimCacheMap.clear();
        CompoundNBT dimensionRegions = nbt.getCompound(NBTConstants.DIMENSIONS);
        YetAnotherWorldProtector.LOGGER.info("Loading region data for " + dimensionRegions.getAllKeys().size() + " different dimensions");
        for (String dimKey : dimensionRegions.getAllKeys()) {
            this.dimensionDataNames.add(dimKey);
            RegistryKey<World> dimension = RegistryKey.create(Registry.DIMENSION_REGISTRY, new ResourceLocation(dimKey));
            dimCacheMap.put(dimension, new DimensionRegionCache(dimensionRegions.getCompound(dimKey)));
        }
    }

    @Override
    public CompoundNBT save(CompoundNBT compound) {
        YetAnotherWorldProtector.LOGGER.info("Saving region data for " + dimCacheMap.entrySet().size() + " different dimensions");
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
                DimensionRegionCache cache = new DimensionRegionCache(event.getTo());
                RegionDataManager.dimCacheMap.put(event.getTo(), cache);
                RegionDataManager.get().getDimensionDataNames().add(cache.getDimensionalRegion().getName());
                YetAnotherWorldProtector.LOGGER.info("Player traveling to dimension without region data. Init region data for dimension '" + event.getTo().location() + "'..");
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
                DimensionRegionCache cache = new DimensionRegionCache(dim);
                RegionDataManager.dimCacheMap.put(cache.getDimensionalRegion().getDimensionKey(), cache);
                RegionDataManager.get().getDimensionDataNames().add(dimCacheMap.get(dim).getDimensionalRegion().getName());
                save();
                YetAnotherWorldProtector.LOGGER.info("Player joining to server in dimension without region data. This should only happen the first time a player is joining. Init region data for dimension '" + event.getPlayer().getCommandSenderWorld().dimension().location() + "'..");
            }
        }
    }

    public Collection<String> getAllRegionNames() {
        return dimCacheMap.values().stream()
                .flatMap(regionCache -> regionCache.getRegionNames().stream())
                .collect(Collectors.toList());
    }

    public Collection<String> getDimensionList() {
        return dimCacheMap.keySet().stream()
                .map(entry -> entry.location().toString())
                .collect(Collectors.toList());
    }

    public Collection<IMarkableRegion> getRegionsFor(RegistryKey<World> dim) {
        return null;
    }

    public Optional<AbstractMarkableRegion> getRegion(String regionName) {
        return null;
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

    public boolean containsRegion(String regionName) {
        return false;
    }

    public void setActiveState(Collection<IMarkableRegion> regionsToProcess, boolean activate) {

    }

    public DimensionRegionCache cacheFor(RegistryKey<World> dim) {
        return dimCacheMap.get(dim);
    }

    // TODO: remove method to avoid not marking changes as dirty
    // OR: implement observable data structures to automatically mark changes as dirty
    public DimensionalRegion dimFor(RegistryKey<World> dim) {
        return dimCacheMap.get(dim).getDimensionalRegion();
    }

    public List<IFlag> getFlagsForDim(RegistryKey<World> dim){
        return new ArrayList<>(cacheFor(dim).getDimensionalRegion().getFlags());
    }

    /* hash for checking manipulated world data?*/
    public class DimensionDataEntry {
        private RegistryKey<World> dim;
        private boolean load;
        private String hash;
    }
}
