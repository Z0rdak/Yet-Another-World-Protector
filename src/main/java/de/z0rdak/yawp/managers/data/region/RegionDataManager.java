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
import net.minecraft.datafixer.DataFixTypes;
import net.minecraft.entity.Entity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.NbtCompound;
import net.minecraft.nbt.NbtElement;
import net.minecraft.registry.RegistryKey;
import net.minecraft.registry.RegistryKeys;
import net.minecraft.registry.RegistryWrapper;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.world.ServerWorld;
import net.minecraft.text.Text;
import net.minecraft.util.Identifier;
import net.minecraft.world.PersistentState;
import net.minecraft.world.PersistentStateManager;
import net.minecraft.world.World;
import org.apache.commons.lang3.NotImplementedException;

import java.util.*;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.util.constants.RegionNBT.DIMENSIONS;
import static de.z0rdak.yawp.util.constants.RegionNBT.REGIONS;

public class RegionDataManager extends PersistentState {

    /**
     * Name which is used for the file to store the NBT data: yawp-dimensions.dat
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
    public static MinecraftServer serverInstance;
    /**
     * Map which holds the mod region information. Each dimension has its on DimensionRegionCache.
     */
    private final Map<RegistryKey<World>, DimensionRegionCache> dimCacheMap = new HashMap<>();
    private final Set<String> dimensionDataNames = new HashSet<>();

    private RegionDataManager() {
    }

    public static void save() {
        YetAnotherWorldProtector.LOGGER.debug(Text.translatableWithFallback("data.nbt.dimensions.save", "Save for RegionDataManager called. Attempting to save region data...").getString());
        regionDataCache.markDirty();
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
            if (serverInstance != null) {
                ServerWorld overworld = serverInstance.getOverworld();
                if (!overworld.isClient) {
                    PersistentStateManager storage = overworld.getPersistentStateManager();
                    Type<RegionDataManager> rdmt = new Type<>(RegionDataManager::new, RegionDataManager::load, DataFixTypes.SAVED_DATA_MAP_DATA);
                    regionDataCache = storage.getOrCreate(rdmt, DATA_NAME);
                }
            }
        }
        return regionDataCache;
    }

    /**
     * Server startup hook for loading the region data from the yawp-dimension.dat file by creating an instance of RegionDataManager.
     *
     * @param server which is fired upon server start and acts as trigger to load region data from disk.
     */
    public static void initServerInstance(MinecraftServer server) {
        serverInstance = server;
    }

    /**
     * Server startup hook for loading the region data from the yawp-dimension.dat file by creating an instance of RegionDataManager.
     *
     * @param minecraftServer
     * @param serverWorld
     */
    public static void loadRegionDataForWorld(MinecraftServer minecraftServer, ServerWorld serverWorld) {
        try {
            if (serverInstance == null) {
                serverInstance = minecraftServer;
            }
            if (!serverWorld.isClient && serverWorld.getRegistryKey().getValue().equals(Identifier.of("minecraft:overworld"))) {
                PersistentStateManager storage = serverWorld.getPersistentStateManager();
                Type<RegionDataManager> rdmt = new Type<>(RegionDataManager::new, RegionDataManager::load, DataFixTypes.SAVED_DATA_MAP_DATA);
                RegionDataManager data = storage.getOrCreate(rdmt, DATA_NAME);
                storage.set(DATA_NAME, data);
                regionDataCache = data;
                YetAnotherWorldProtector.LOGGER.info(Text.translatableWithFallback("data.nbt.dimensions.load.success", "Loaded %s region(s) for %s dimension(s)", data.getTotalRegionAmount(), data.getDimensionAmount()).getString());
            }
        } catch (NullPointerException npe) {
            YetAnotherWorldProtector.LOGGER.error(Text.translatableWithFallback("data.nbt.dimensions.load.failure", "Loading regions failed!").getString());
        }
    }

    private static RegionDataManager load(NbtCompound nbt, RegistryWrapper.WrapperLookup wrapperLookup) {
        RegionDataManager rdm = new RegionDataManager();
        rdm.dimCacheMap.clear();
        NbtCompound dimensionRegions = nbt.getCompound(DIMENSIONS);
        YetAnotherWorldProtector.LOGGER.info(Text.translatableWithFallback("data.nbt.dimensions.load.amount", "Loading region(s) for %s dimension(s)", dimensionRegions.getKeys().size()).getString());
        // deserialize all region without parent and child references
        for (String dimKey : dimensionRegions.getKeys()) {
            rdm.dimensionDataNames.add(dimKey);
            RegistryKey<World> dimension = RegistryKey.of(RegistryKeys.WORLD, Identifier.of(dimKey));
            if (dimensionRegions.contains(dimKey, NbtElement.COMPOUND_TYPE)) {
                NbtCompound dimCacheNbt = dimensionRegions.getCompound(dimKey);
                if (dimCacheNbt.contains(REGIONS, NbtElement.COMPOUND_TYPE)) {
                    YetAnotherWorldProtector.LOGGER.info(Text.translatableWithFallback("data.nbt.dimensions.load.dim.amount", "Loading %s region(s) for dimension %s", dimCacheNbt.getCompound(REGIONS).getSize(), dimKey).getString());
                } else {
                    YetAnotherWorldProtector.LOGGER.info(Text.translatableWithFallback("data.nbt.dimensions.load.dim.empty", "No region data for dimension %s found", dimKey).getString());
                }
                rdm.dimCacheMap.put(dimension, new DimensionRegionCache(dimCacheNbt));
            }
        }
        rdm.dimCacheMap.forEach((dimKey, cache) -> {
            YetAnotherWorldProtector.LOGGER.info(Text.translatableWithFallback("data.nbt.dimensions.loaded.dim.amount", "Loaded %s region(s) for dimension %s", cache.getRegions().size(), dimKey.getValue().toString()).getString());
        });

        // set parent and child references
        for (String dimKey : dimensionRegions.getKeys()) {
            RegistryKey<World> dimension = RegistryKey.of(RegistryKeys.WORLD, Identifier.of(dimKey));
            DimensionRegionCache dimCache = rdm.dimCacheMap.get(dimension);
            if (dimCache.getRegions().size() > 0) {
                YetAnotherWorldProtector.LOGGER.info(Text.translatableWithFallback("data.nbt.dimensions.load.dim.restore", "Restoring region hierarchy for regions in dimension %s", dimKey).getString());
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
     * An event which is called after a player has been moved to a different world.
     * Event handler which creates a new DimensionRegionCache when a dimension is created the first time, by a player loading the dimension.
     *
     * @param playerEntity
     * @param origin
     * @param destination
     */
    public static void onPlayerChangeWorldAddDimKey(PlayerEntity playerEntity, ServerWorld origin, ServerWorld destination) {
        if (!destination.isClient) {
            if (!regionDataCache.dimCacheMap.containsKey(destination.getRegistryKey())) {
                DimensionRegionCache cache = RegionDataManager.get().newCacheFor(destination.getRegistryKey());
                YetAnotherWorldProtector.LOGGER.info("Init region data for dimension '" + cache.dimensionKey().getValue() + "'..");
                save();
            }
        }
    }

    /**
     * Event handler which is used to initialize the dimension cache with first dimension entry when a player logs in.
     *
     * @param entity
     * @param serverWorld
     */
    public static void onPlayerLoadAddDimKey(Entity entity, ServerWorld serverWorld) {
        if (!serverWorld.isClient && entity instanceof PlayerEntity) {
            RegistryKey<World> dim = serverWorld.getRegistryKey();
            if (!regionDataCache.dimCacheMap.containsKey(dim)) {
                DimensionRegionCache cache = RegionDataManager.get().newCacheFor(dim);
                YetAnotherWorldProtector.LOGGER.info("Player joining to server in dimension without region data. This should only happen the first time a player is joining.");
                YetAnotherWorldProtector.LOGGER.info("Init region data for dimension '" + cache.dimensionKey().getValue() + "'..");
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
    @Override
    public NbtCompound writeNbt(NbtCompound compound, RegistryWrapper.WrapperLookup registryLookup) {
        NbtCompound dimRegionNbtData = new NbtCompound();
        YetAnotherWorldProtector.LOGGER.info(Text.translatableWithFallback("data.nbt.dimensions.save.amount", "Saving %s region(s) for %s dimensions", this.getTotalRegionAmount(), dimCacheMap.keySet().size()).getString());
        for (Map.Entry<RegistryKey<World>, DimensionRegionCache> entry : dimCacheMap.entrySet()) {
            YetAnotherWorldProtector.LOGGER.info(Text.translatableWithFallback("data.nbt.dimensions.save.dim.amount", "Saving %s region(s) for dimension %s", this.getRegionAmount(entry.getKey()), entry.getKey().getValue().toString()).getString());
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

    public int getRegionAmount(RegistryKey<World> dim) {
        return cacheFor(dim).getRegions().size();
    }

    public Collection<String> getDimensionList() {
        return dimCacheMap.keySet().stream()
                .map(entry -> entry.getValue().toString())
                .collect(Collectors.toList());
    }

    public int getDimensionAmount() {
        return dimCacheMap.keySet().size();
    }

    public IMarkableRegion getRegionIn(String regionName, RegistryKey<World> dim) {
        if (dimCacheMap.containsKey(dim)) {
            DimensionRegionCache cache = dimCacheMap.get(dim);
            if (cache.contains(regionName)) {
                return cache.get(regionName);
            }
        }
        return null;
    }

    public Collection<IMarkableRegion> getRegionsFor(RegistryKey<World> dim) {
        return cacheFor(dim).getRegions();
    }

    public DimensionRegionCache cacheFor(RegistryKey<World> dim) {
        if (!dimCacheMap.containsKey(dim)) {
            newCacheFor(dim);
            save();
        }
        return dimCacheMap.get(dim);
    }

    public boolean containsCacheFor(RegistryKey<World> dim) {
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

    public DimensionRegionCache newCacheFor(RegistryKey<World> dim) {
        DimensionRegionCache cache = new DimensionRegionCache(dim);
        addFlags(RegionConfig.getDefaultDimFlags(), cache.getDimensionalRegion());
        cache.getDimensionalRegion().setIsActive(RegionConfig.shouldActivateNewDimRegion());
        cache.getDimensionalRegion().setParent(globalRegion);
        dimCacheMap.put(dim, cache);
        dimensionDataNames.add(cache.getDimensionalRegion().getName());
        return cache;
    }

}
