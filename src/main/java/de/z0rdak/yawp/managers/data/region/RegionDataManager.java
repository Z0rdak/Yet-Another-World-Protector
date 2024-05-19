package de.z0rdak.yawp.managers.data.region;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.config.server.RegionConfig;
import de.z0rdak.yawp.core.flag.BooleanFlag;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.core.region.GlobalRegion;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import net.minecraft.core.registries.Registries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.Tag;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.saveddata.SavedData;
import net.minecraft.world.level.storage.DimensionDataStorage;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.event.entity.EntityTravelToDimensionEvent;
import net.minecraftforge.event.entity.player.PlayerEvent;
import net.minecraftforge.event.server.ServerStartingEvent;
import net.minecraftforge.eventbus.api.EventPriority;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.server.ServerLifecycleHooks;
import org.apache.commons.lang3.NotImplementedException;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.*;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.util.constants.RegionNBT.DIMENSIONS;
import static de.z0rdak.yawp.util.constants.RegionNBT.REGIONS;

@Mod.EventBusSubscriber(modid = YetAnotherWorldProtector.MODID, value = Dist.DEDICATED_SERVER)
public class RegionDataManager extends SavedData {

    /**
     * Name which is used for the file to store the NBT data: yawp-dimensions.dat.old
     */
    private static final String DATA_NAME = YetAnotherWorldProtector.MODID + "-dimensions";
    /**
     * The global region of this mod which all sets common rules/flags for all regions.
     */
    private final static GlobalRegion globalRegion = new GlobalRegion();
    /**
     * Singleton used to access methods to manage region data.
     */

    private static RegionDataManager regionDataCache = new RegionDataManager();
    /**
     * Map which holds the mod region information. Each dimension has its on DimensionRegionCache.
     */
    private final Map<ResourceKey<Level>, DimensionRegionCache> dimCacheMap = new HashMap<>();
    private final Set<String> dimensionDataNames = new HashSet<>();

    private RegionDataManager() {
    }

    public static void save() {
        YetAnotherWorldProtector.LOGGER.debug(Component.translatableWithFallback("data.nbt.dimensions.save", "Save for RegionDataManager called. Attempting to save region data...").getString());
        regionDataCache.setDirty();
    }

    /**
     * Returns the name of all dimension tracked by the region data manager.
     *
     * @return the set of dimension names which are tracked by the region data manager.
     */
    @SuppressWarnings("unused")
    public static Set<String> getDimensionDataNames() {
        return Collections.unmodifiableSet(regionDataCache.dimensionDataNames);
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

    /**
     * Server startup hook for loading the region data from the yawp-dimension.dat file by creating an instance of RegionDataManager.
     *
     * @param event which is fired upon server start and acts as trigger to load region data from disk.
     */
    @SubscribeEvent
    public static void loadRegionData(ServerStartingEvent event) {
        try {
            ServerLevel world = Objects.requireNonNull(event.getServer().overworld());
            if (!world.isClientSide) {
                DimensionDataStorage storage = world.getDataStorage();
                RegionDataManager data = storage.computeIfAbsent(RegionDataManager::load, RegionDataManager::new, DATA_NAME);
                storage.set(DATA_NAME, data);
                regionDataCache = data;
                YetAnotherWorldProtector.LOGGER.info(Component.translatableWithFallback("data.nbt.dimensions.load.success", "Loaded %s region(s) for %s dimension(s)", data.getTotalRegionAmount(), data.getDimensionAmount()).getString());
            }
        } catch (Exception npe) {
            YetAnotherWorldProtector.LOGGER.error(Component.translatableWithFallback("data.nbt.dimensions.load.failure", "Loading regions failed!").getString());
            YetAnotherWorldProtector.LOGGER.error(Component.literal(npe.getLocalizedMessage()).getString());
        }
    }

    public static RegionDataManager load(CompoundTag nbt) {
        RegionDataManager rdm = new RegionDataManager();
        rdm.dimCacheMap.clear();
        CompoundTag dimensionRegions = nbt.getCompound(DIMENSIONS);
        YetAnotherWorldProtector.LOGGER.info(Component.translatableWithFallback("data.nbt.dimensions.load.amount", "Loading region(s) for %s dimension(s)", dimensionRegions.getAllKeys().size()).getString());
        // deserialize all region without parent and child references
        for (String dimKey : dimensionRegions.getAllKeys()) {
            rdm.dimensionDataNames.add(dimKey);
            ResourceKey<Level> dimension = ResourceKey.create(Registries.DIMENSION, new ResourceLocation(dimKey));
            if (dimensionRegions.contains(dimKey, Tag.TAG_COMPOUND)) {
                CompoundTag dimCacheNbt = dimensionRegions.getCompound(dimKey);
                if (dimCacheNbt.contains(REGIONS, Tag.TAG_COMPOUND)) {
                    YetAnotherWorldProtector.LOGGER.info(Component.translatableWithFallback("data.nbt.dimensions.load.dim.amount", "Loading %s region(s) for dimension %s", dimCacheNbt.getCompound(REGIONS).size(), dimKey).getString());
                } else {
                    YetAnotherWorldProtector.LOGGER.info(Component.translatableWithFallback("data.nbt.dimensions.load.dim.empty", "No region data for dimension %s found", dimKey).getString());
                }
                rdm.dimCacheMap.put(dimension, new DimensionRegionCache(dimCacheNbt));
            }
        }
        rdm.dimCacheMap.forEach((dimKey, cache) -> {
            YetAnotherWorldProtector.LOGGER.info(Component.translatableWithFallback("data.nbt.dimensions.loaded.dim.amount", "Loaded %s region(s) for dimension %s", cache.getRegions().size(), dimKey.location().toString()).getString());
        });

        // set parent and child references
        for (String dimKey : dimensionRegions.getAllKeys()) {
            ResourceKey<Level> dimension = ResourceKey.create(Registries.DIMENSION, new ResourceLocation(dimKey));
            DimensionRegionCache dimCache = rdm.dimCacheMap.get(dimension);
            if (dimCache.getRegions().size() > 0) {
                YetAnotherWorldProtector.LOGGER.info(Component.translatableWithFallback("data.nbt.dimensions.load.dim.restore", "Restoring region hierarchy for regions in dimension %s", dimKey).getString());
            }
            DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
            dimCache.getRegionsInDimension().values().forEach(region -> {
                // set child references
                region.getChildrenNames().forEach(childName -> {
                    IMarkableRegion childRegion = dimCache.getRegion(childName);
                    region.addChild(childRegion);
                });
                // TODO: Replace this and put it into addChild?
                // set parent reference
                String parentName = region.getParentName();
                boolean hasValidParent = parentName != null && !parentName.equals("");
                if (hasValidParent) {
                    boolean hasDimRegionAsParent = parentName.contains(":");
                    if (hasDimRegionAsParent) { // colons are not allowed in normal region names so this should work fine
                        region.setParent(dimRegion);
                    } else {
                        region.setParent(dimCache.getRegion(parentName));
                    }
                }
            });
        }
        return rdm;
    }

    /**
     * Event handler which creates a new DimensionRegionCache when a dimension is created the first time, by a player loading the dimension.     *
     *
     * @param event the EntityTravelToDimensionEvent which serves as a trigger and provides the information which dimension the player is traveling to.
     */
    @SubscribeEvent(priority = EventPriority.HIGHEST, receiveCanceled = true)
    public static void addDimKeyOnDimensionChange(EntityTravelToDimensionEvent event) {
        if (!event.getEntity().getCommandSenderWorld().isClientSide) {
            if (event.getEntity() instanceof Player) {
                if (!regionDataCache.dimCacheMap.containsKey(event.getDimension())) {
                    DimensionRegionCache cache = regionDataCache.newCacheFor(event.getDimension());
                    YetAnotherWorldProtector.LOGGER.info("Init region data for dimension '" + cache.dimensionKey().location() + "'..");
                    save();
                }
            }
        }
    }

    /**
     * Event handler which is used to initialize the dimension cache with first dimension entry when a player logs in.
     *
     * @param event PlayerLoggedInEvent which serves as a trigger and provides the information about the dimension the player logged in to.
     */
    @SubscribeEvent
    public static void addDimKeyOnPlayerLogin(PlayerEvent.PlayerLoggedInEvent event) {
        if (!event.getEntity().getCommandSenderWorld().isClientSide && event.getEntity() != null) {
            ResourceKey<Level> dim = event.getEntity().getCommandSenderWorld().dimension();
            if (!regionDataCache.dimCacheMap.containsKey(dim)) {
                DimensionRegionCache cache = regionDataCache.newCacheFor(dim);
                YetAnotherWorldProtector.LOGGER.info("Player joining to server in dimension without region data. This should only happen the first time a player is joining.");
                YetAnotherWorldProtector.LOGGER.info("Init region data for dimension '" + cache.dimensionKey().location() + "'..");
                save();
            }
        }
    }

    public static void addFlags(Set<String> flags, IProtectedRegion region) {
        flags.stream()
                .map(RegionFlag::fromId)
                .forEach(flag -> {
                    switch (flag.type) {
                        case BOOLEAN_FLAG:
                            region.addFlag(new BooleanFlag(flag));
                            break;
                        case LIST_FLAG:
                        case INT_FLAG:
                            throw new NotImplementedException("");
                    }
                });
    }

    /**
     * Method which gets called the region data is marked as dirty via the save/markDirty method.
     *
     * @param compound nbt data to be filled with the region information.
     * @return the compound region nbt data to be saved to disk.
     */
    @Nonnull
    @Override
    public CompoundTag save(@Nonnull CompoundTag compound) {
        CompoundTag dimRegionNbtData = new CompoundTag();
        YetAnotherWorldProtector.LOGGER.info(Component.translatableWithFallback("data.nbt.dimensions.save.amount", "Saving %s region(s) for %s dimensions", this.getTotalRegionAmount(), dimCacheMap.keySet().size()).getString());
        for (Map.Entry<ResourceKey<Level>, DimensionRegionCache> entry : dimCacheMap.entrySet()) {
            YetAnotherWorldProtector.LOGGER.info(Component.translatableWithFallback("data.nbt.dimensions.save.dim.amount", "Saving %s region(s) for dimension %s", this.getRegionAmount(entry.getKey()), entry.getKey().location().toString()).getString());
            String dimensionName = entry.getValue().getDimensionalRegion().getName();
            dimRegionNbtData.put(dimensionName, entry.getValue().serializeNBT());
        }
        compound.put(DIMENSIONS, dimRegionNbtData);
        return compound;
    }

    public int getTotalRegionAmount() {
        return (int) dimCacheMap.values().stream()
                .mapToLong(regionCache -> regionCache.getRegionNames().size()).sum();
    }

    public int getRegionAmount(ResourceKey<Level> dim) {
        return cacheFor(dim).getRegions().size();
    }

    public Collection<String> getDimensionList() {
        return dimCacheMap.keySet().stream()
                .map(entry -> entry.location().toString())
                .collect(Collectors.toList());
    }

    public int getDimensionAmount() {
        return dimCacheMap.keySet().size();
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

    public Collection<IMarkableRegion> getRegionsFor(ResourceKey<Level> dim) {
        return cacheFor(dim).getRegions();
    }

    public DimensionRegionCache cacheFor(ResourceKey<Level> dim) {
        if (!dimCacheMap.containsKey(dim)) {
            newCacheFor(dim);
            save();
        }
        return dimCacheMap.get(dim);
    }

    public boolean containsCacheFor(ResourceKey<Level> dim) {
        return dimCacheMap.containsKey(dim);
    }

    public List<String> getFlagsIdsForDim(DimensionRegionCache dimCache) {
        if (dimCache != null) {
            return dimCache.getDimensionalRegion().getFlags()
                    .stream()
                    .map(IFlag::getFlagIdentifier)
                    .collect(Collectors.toList());
        }
        return new ArrayList<>();
    }

    public DimensionRegionCache newCacheFor(ResourceKey<Level> dim) {
        DimensionRegionCache cache = new DimensionRegionCache(dim);
        addFlags(RegionConfig.getDefaultDimFlags(), cache.getDimensionalRegion());
        cache.getDimensionalRegion().setIsActive(RegionConfig.shouldActivateNewDimRegion());
        cache.getDimensionalRegion().setParent(globalRegion);
        dimCacheMap.put(dim, cache);
        dimensionDataNames.add(cache.getDimensionalRegion().getName());
        return cache;
    }
}
