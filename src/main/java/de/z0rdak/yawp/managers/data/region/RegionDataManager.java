package de.z0rdak.yawp.managers.data.region;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.config.server.RegionConfig;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.AbstractMarkableRegion;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.util.constants.*;
import net.minecraft.core.Registry;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.network.chat.contents.TranslatableContents;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.saveddata.SavedData;
import net.minecraft.world.level.storage.DimensionDataStorage;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.event.entity.player.PlayerEvent;
import net.minecraftforge.event.server.ServerStartingEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.server.ServerLifecycleHooks;

import javax.annotation.Nullable;
import java.util.*;
import java.util.stream.Collectors;

@Mod.EventBusSubscriber(modid = YetAnotherWorldProtector.MODID, value = Dist.DEDICATED_SERVER)
public class RegionDataManager extends SavedData {

    private static final String DATA_NAME = YetAnotherWorldProtector.MODID + "-dimensions";
    private static Map<ResourceKey<Level>, DimensionRegionCache> dimCacheMap = new HashMap<>();
    private List<String> dimensionDataNames;
    private static RegionDataManager regionDataCache = new RegionDataManager();

    private RegionDataManager() {
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
                ServerLevel overworld = server.overworld();
                if (!overworld.isClientSide) {
                    DimensionDataStorage storage = overworld.getDataStorage();
                    regionDataCache = storage.computeIfAbsent(RegionDataManager::load, RegionDataManager::new, DATA_NAME);
                }
            }
        }
        return regionDataCache;
    }

    public static void loadRegionData(ServerStartingEvent event) {
        try {
            ServerLevel world = Objects.requireNonNull(event.getServer().overworld());
            if (!world.isClientSide) {
                DimensionDataStorage storage = world.getDataStorage();
                RegionDataManager data = storage.computeIfAbsent(RegionDataManager::load, RegionDataManager::new, DATA_NAME);
                storage.set(DATA_NAME, data);
                regionDataCache = data;
                YetAnotherWorldProtector.LOGGER.info(MutableComponent.create(new TranslatableContents("console.logger.info.data.load.success", data.getAllRegionNames().size(), data.getDimensionList().size())).getString());
            }
        } catch (NullPointerException npe) {
            YetAnotherWorldProtector.LOGGER.error(MutableComponent.create(new TranslatableContents("console.logger.error.data.load.failure")));
        }
    }

    // TODO: Check
    public static RegionDataManager load(CompoundTag nbt) {
        dimCacheMap.clear();
        RegionDataManager rdm = new RegionDataManager();
        CompoundTag dimensionRegions = nbt.getCompound(NBTConstants.DIMENSIONS);
        YetAnotherWorldProtector.LOGGER.info("Loading region data for " + dimensionRegions.getAllKeys().size() + " different dimensions");
        for (String dimKey : dimensionRegions.getAllKeys()) {
            rdm.dimensionDataNames.add(dimKey);
            ResourceKey<Level> dimension = ResourceKey.create(Registry.DIMENSION_REGISTRY, new ResourceLocation(dimKey));
            dimCacheMap.put(dimension, new DimensionRegionCache(dimensionRegions.getCompound(dimKey)));
        }
        return rdm;
    }

    @Override
    public CompoundTag save(CompoundTag compound) {
        YetAnotherWorldProtector.LOGGER.info("Saving region data for " + dimCacheMap.entrySet().size() + " different dimensions");
        CompoundTag dimRegionNbtData = new CompoundTag();
        for (Map.Entry<ResourceKey<Level>, DimensionRegionCache> entry : dimCacheMap.entrySet()) {
            String dimensionName = entry.getValue().getDimensionalRegion().getName();
            dimRegionNbtData.put(dimensionName, entry.getValue().serializeNBT());
        }
        compound.put(NBTConstants.DIMENSIONS, dimRegionNbtData);
        return compound;
    }

    @SubscribeEvent
    public static void addDimKeyOnDimensionChange(PlayerEvent.PlayerChangedDimensionEvent event){
        // if region map does not contain an entry for the dimension traveled to, add it to the map
        if (!event.getEntity().getCommandSenderWorld().isClientSide) {
            if (dimCacheMap == null) {
                dimCacheMap = new HashMap<>();
            }
            if (!dimCacheMap.containsKey(event.getTo())) {
                DimensionRegionCache cache = new DimensionRegionCache(event.getTo());
                RegionConfig.getDefaultDimFlags()
                        .stream() // get is fine here, because the config is validated beforehand
                        .map(flag -> RegionFlag.fromString(flag).get().flag)
                        .forEach(cache::addFlag);
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
        if (!event.getEntity().getCommandSenderWorld().isClientSide) {
            ResourceKey<Level> dim = event.getEntity().getCommandSenderWorld().dimension();
            if (dimCacheMap == null) {
                dimCacheMap = new HashMap<>();
            }
            if (!dimCacheMap.containsKey(dim)) {
                DimensionRegionCache cache = new DimensionRegionCache(dim);
                RegionConfig.getDefaultDimFlags()
                        .stream() // get is fine here, because the config is validated beforehand
                        .map(flag -> RegionFlag.fromString(flag).get().flag)
                        .forEach(cache::addFlag);
                RegionDataManager.dimCacheMap.put(cache.getDimensionalRegion().getDimensionKey(), cache);
                RegionDataManager.get().getDimensionDataNames().add(dimCacheMap.get(dim).getDimensionalRegion().getName());
                save();
                YetAnotherWorldProtector.LOGGER.info("Player joining to server in dimension without region data. This should only happen the first time a player is joining. Init region data for dimension '" + event.getEntity().getCommandSenderWorld().dimension().location() + "'..");
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

    public Collection<IMarkableRegion> getRegionsFor(ResourceKey<Level> dim) {
        return dimCacheMap.get(dim).getRegions();
    }

    @Nullable
    public IMarkableRegion getRegionIn(String regionName, ResourceKey<Level> dim) {
        if (dimCacheMap.containsKey(dim)) {
            DimensionRegionCache cache = dimCacheMap.get(dim);
            if (cache.contains(regionName)) {
                return cache.get(regionName);
            }
        }
        return null;
    }

    public boolean containsRegion(ResourceKey<Level> dim, IMarkableRegion region) {
        if (dimCacheMap.containsKey(dim)) {
            return dimCacheMap.get(dim).contains(region.getName());
        }
        return false;
    }

    public void setActiveState(Collection<IMarkableRegion> regionsToProcess, boolean activate) {

    }

    @Nullable
    public DimensionRegionCache cacheFor(ResourceKey<Level> dim) {
        return dimCacheMap.get(dim);
    }

    public List<IFlag> getFlagsForDim(ResourceKey<Level> dim){
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

    @Nullable
    public boolean containsCacheFor(ResourceKey<Level> dim) {
        return dimCacheMap.containsKey(dim);
    }

    public DimensionRegionCache newCacheFor(ResourceKey<Level> dim) {
        DimensionRegionCache dimCache =  dimCacheMap.put(dim, new DimensionRegionCache(dim));
        save();
        return dimCache;
    }
    /* hash for checking manipulated world data?*/
    public class DimensionDataEntry {
        private ResourceKey<Level> dim;
        private boolean load;
        private String hash;
    }
}
