package de.z0rdak.yawp.data.region;

import com.mojang.datafixers.types.Type;
import de.z0rdak.yawp.api.commands.CommandConstants;
import de.z0rdak.yawp.commands.arguments.region.RegionArgumentType;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.flag.BooleanFlag;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.GlobalRegion;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.core.HolderLookup;
import net.minecraft.core.registries.Registries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.Tag;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.util.datafix.DataFixTypes;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.saveddata.SavedData;
import net.minecraft.world.level.storage.DimensionDataStorage;
import org.apache.commons.lang3.NotImplementedException;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.api.commands.CommandConstants.values;
import static de.z0rdak.yawp.constants.serialization.RegionNbtKeys.*;
import static de.z0rdak.yawp.handler.HandlerUtil.isServerSide;

public class RegionDataManager extends SavedData {

    /**
     * Name which is used for the file to store the NBT data: yawp-dimensions.dat
     */
    private static final String DATA_NAME = Constants.MOD_ID + "-dimensions";
    public static MinecraftServer serverInstance;
    /**
     * The global region of this mod which all sets common rules/flags for all regions.
     */
    private static GlobalRegion globalRegion = new GlobalRegion();
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
        Constants.LOGGER.debug(Component.translatableWithFallback("data.nbt.dimensions.save", "Save for RegionDataManager called. Attempting to save region data...").getString());
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

    public static List<DimensionRegionCache> getDimensionCaches() {
        return Collections.unmodifiableList(new ArrayList<>(regionDataCache.dimCacheMap.values()));
    }

    public static RegionDataManager get() {
        if (regionDataCache == null) {
            if (serverInstance != null) {
                ServerLevel overworld = serverInstance.overworld();
                if (!overworld.isClientSide) {
                    DimensionDataStorage storage = overworld.getDataStorage();
                    Factory<RegionDataManager> rdmt = new Factory<>(RegionDataManager::new, RegionDataManager::load, DataFixTypes.SAVED_DATA_MAP_DATA);
                    regionDataCache = storage.get(rdmt, DATA_NAME);
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
    public static void loadRegionDataForWorld(MinecraftServer minecraftServer, ServerLevel serverWorld) {
        try {
            if (serverInstance == null) {
                serverInstance = minecraftServer;
            }
            var isOverworld = serverWorld.dimension().location().equals(ServerLevel.OVERWORLD.location());
            if (isServerSide(serverWorld) && isOverworld) {
                DimensionDataStorage storage = serverWorld.getDataStorage();
                Factory<RegionDataManager> rdmt = new Factory<>(RegionDataManager::new, RegionDataManager::load, DataFixTypes.SAVED_DATA_MAP_DATA);
                RegionDataManager data = storage.computeIfAbsent(rdmt, DATA_NAME);
                storage.set(DATA_NAME, data);
                regionDataCache = data;
                Constants.LOGGER.info(Component.translatableWithFallback("data.nbt.dimensions.load.success", "Loaded %s region(s) for %s dimension(s)", data.getTotalRegionAmount(), data.getDimensionAmount()).getString());
            }
        } catch (NullPointerException npe) {
            Constants.LOGGER.error(Component.translatableWithFallback("data.nbt.dimensions.load.failure", "Loading regions failed!").getString());
        }
    }

    /**
     * Method which gets called when a new RegionDataManager instance is created by loadRegionData.
     *
     * @param nbt compound region data read from disk to be deserialized for the region cache.
     */
    public static RegionDataManager load(CompoundTag nbt, HolderLookup.Provider registries) {
        RegionDataManager rdm = new RegionDataManager();
        rdm.dimCacheMap.clear();
        if (!nbt.contains(GLOBAL) || nbt.getCompound(GLOBAL).isEmpty()) {
            Constants.LOGGER.info(Component.translatable("Missing global region data. Initializing new data. (Ignore this for the first server start)").getString());
            globalRegion = new GlobalRegion();
        } else {
            CompoundTag globalNbt = nbt.getCompound(GLOBAL);
            globalRegion = new GlobalRegion();
            globalRegion.deserializeNBT(globalNbt);
            Constants.LOGGER.info(Component.translatable("Loaded global region data").getString());
        }

        if (!nbt.contains(DIMENSIONS) || nbt.getCompound(DIMENSIONS).isEmpty()) {
            Constants.LOGGER.info(Component.translatable("No region data found for dimensions. Initializing new data...").getString());
            rdm.dimCacheMap.clear();
        } else {
            CompoundTag dimensionRegions = nbt.getCompound(DIMENSIONS);
            Constants.LOGGER.info(Component.translatable("Loading region(s) for " + dimensionRegions.size() + " dimension(s)").getString());
            rdm.dimCacheMap.clear();
            // deserialize all region without parent and child references
            for (String dimKey : dimensionRegions.getAllKeys()) {
                if (dimensionRegions.contains(dimKey, Tag.TAG_COMPOUND)) {
                    ResourceKey<Level> dimension = ResourceKey.create(Registries.DIMENSION, ResourceLocation.parse(dimKey));
                    CompoundTag dimCacheNbt = dimensionRegions.getCompound(dimKey);
                    if (dimCacheNbt.contains(REGIONS, Tag.TAG_COMPOUND)) {
                        Constants.LOGGER.info(Component.translatable("Loading " + dimCacheNbt.getCompound(REGIONS).size() + " region(s) for dimension '" + dimKey + "'").getString());
                    } else {
                        Constants.LOGGER.info(Component.translatable("No region data for dimension '" + dimKey + "' found").getString());
                    }
                    DimensionRegionCache dimCache = new DimensionRegionCache(dimCacheNbt);
                    globalRegion.addChild(dimCache.getDimensionalRegion());
                    rdm.dimCacheMap.put(dimension, dimCache);
                    rdm.dimensionDataNames.add(dimKey);
                    Constants.LOGGER.info(Component.translatableWithFallback("data.nbt.dimensions.loaded.dim.amount", "Loaded %s region(s) for dimension '%s'", dimCache.getRegionCount(), dimCache.getDimensionalRegion().getName()).getString());
                }
            }
        }
        // restore parent/child hierarchy
        rdm.dimCacheMap.forEach((dimKey, cache) -> {
            if (cache.getRegionCount() > 0) {
                Constants.LOGGER.info(Component.translatable("Restoring region hierarchy for regions in dimension '" + dimKey.location() + "'").getString());
                ArrayList<IMarkableRegion> regions = new ArrayList<>(cache.getRegionsInDimension().values());
                regions.forEach(region -> {
                    // set child reference
                    region.getChildrenNames().forEach(childName -> {
                        if (!cache.contains(childName)) {
                            Constants.LOGGER.error(Component.translatable("Corrupt save data. Child region '" + childName + "' not found in dimension '" + dimKey + "'!").getString());
                        } else {
                            IMarkableRegion child = cache.getRegion(childName);
                            if (child != null) {
                                cache.getDimensionalRegion().removeChild(child);
                                region.addChild(child);
                            }
                        }
                    });
                });
            }
        });
        return rdm;
    }

    /**
     * An event which is called after a player has been moved to a different world.
     * Event handler which creates a new DimensionRegionCache when a dimension is created the first time, by a player loading the dimension.
     */
    public static void addDimKeyOnDimensionChange(Player Player, Level origin, Level destination) {
        if (isServerSide(destination)) {
            if (!regionDataCache.dimCacheMap.containsKey(destination.dimension())) {
                DimensionRegionCache cache = RegionDataManager.get().newCacheFor(destination.dimension());
                Constants.LOGGER.info("Init region data for dimension '{}'..", cache.dimensionKey().location());
                save();
            }
        }
    }

    /**
     * Event handler which is used to initialize the dimension cache with first dimension entry when a player logs in.
     */
    public static void addDimKeyOnPlayerLogin(Entity entity, Level serverWorld) {
        if (isServerSide(serverWorld) && entity instanceof Player) {
            ResourceKey<Level> dim = serverWorld.dimension();
            if (!regionDataCache.dimCacheMap.containsKey(dim)) {
                DimensionRegionCache cache = regionDataCache.newCacheFor(dim);
                Constants.LOGGER.info("Player joining to server in dimension without region data. This should only happen the first time a player is joining.");
                Constants.LOGGER.info("Init region data for dimension '{}'..", cache.dimensionKey().location());
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
    public CompoundTag save(CompoundTag compound, HolderLookup.Provider var2) {
        compound.put(GLOBAL, globalRegion.serializeNBT());
        CompoundTag dimRegionNbtData = new CompoundTag();
        // Constants.LOGGER.info(new TranslationTextComponent("data.nbt.dimensions.save.amount", this.getTotalRegionAmount(), dimCacheMap.keySet().size()).getString());
        Constants.LOGGER.info(Component.translatable("Saving " + this.getTotalRegionAmount() + " region(s) for " + dimCacheMap.keySet().size() + " dimensions").getString());
        for (Map.Entry<ResourceKey<Level>, DimensionRegionCache> entry : dimCacheMap.entrySet()) {
            // Constants.LOGGER.info(new TranslationTextComponent("data.nbt.dimensions.save.dim.amount", this.getRegionAmount(entry.getKey()), entry.getKey().location().toString()).getString());
            Constants.LOGGER.info(Component.translatable("Saving " + this.getRegionAmount(entry.getKey()) + " region(s) for dimension '" + entry.getKey().location() + "'").getString());
            String dimensionName = entry.getValue().getDimensionalRegion().getName();
            dimRegionNbtData.put(dimensionName, entry.getValue().serializeNBT());
        }
        compound.put(DIMENSIONS, dimRegionNbtData);
        return compound;
    }

    public int getTotalRegionAmount() {
        return dimCacheMap.values().stream()
                .mapToInt(DimensionRegionCache::getRegionCount)
                .sum();
    }

    public int getRegionAmount(ResourceKey<Level> dim) {
        return cacheFor(dim).getRegionCount();
    }

    public Set<String> getDimensionList() {
        return dimCacheMap.keySet().stream()
                .map(entry -> entry.location().toString())
                .collect(Collectors.toSet());
    }

    public Set<ResourceKey<Level>> getDimKeys() {
        return new HashSet<>(dimCacheMap.keySet());
    }

    public GlobalRegion getGlobalRegion() {
        return globalRegion;
    }

    public void resetDimensionCache(ResourceKey<Level> dim) {
        dimCacheMap.remove(dim);
    }

    public void resetGlobalRegion() {
        List<IProtectedRegion> collect = new ArrayList<>(globalRegion.getChildren().values());
        globalRegion = new GlobalRegion();
        collect.forEach(dr -> {
            globalRegion.addChild(dr);
        });
        save();
    }

    public int getDimensionAmount() {
        return dimCacheMap.keySet().size();
    }

    @Nullable
    public Optional<IMarkableRegion> getRegionIn(ResourceKey<Level> dim, String regionName) {
        if (dimCacheMap.containsKey(dim)) {
            DimensionRegionCache cache = dimCacheMap.get(dim);
            if (cache.contains(regionName)) {
                IMarkableRegion region = cache.getRegion(regionName);
                return region == null ? Optional.empty() : Optional.of(region);
            }
        }
        return Optional.empty();
    }

    public Collection<IMarkableRegion> getRegionsFor(ResourceKey<Level> dim) {
        return cacheFor(dim).getAllLocal();
    }

    public DimensionRegionCache cacheFor(ResourceKey<Level> dim) {
        if (!dimCacheMap.containsKey(dim)) {
            newCacheFor(dim);
            save();
        }
        return dimCacheMap.get(dim);
    }

    public Optional<DimensionRegionCache> getCache(ResourceKey<Level> dim) {
        if (!dimCacheMap.containsKey(dim)) {
            return Optional.empty();
        }
        return Optional.of(dimCacheMap.get(dim));
    }

    /**
     * Method to check if a region name is valid for a given dimension. <br>
     * A region name is valid if it matches the pattern and is not already used in the dimension.
     *
     * @param dim        the dimension to be checked.
     * @param regionName the name of the region to be checked.
     * @return -1 if the region name is invalid, 0 if the region name is valid, 1 if the region name is already used in the dimension.
     */
    public int isValidRegionName(ResourceKey<Level> dim, String regionName) {
        List<String> commandStrings = Arrays.stream(values()).map(CommandConstants::toString).collect(Collectors.toList());
        if (!regionName.matches(RegionArgumentType.VALID_NAME_PATTERN.pattern())
                || commandStrings.contains(regionName.toLowerCase())) {
            return -1;
        }
        if (cacheFor(dim).contains(regionName)) {
            return 1;
        }
        return 0;
    }

    /**
     * Method to check if a region name is valid for a given dimension. <br>
     * A region name is valid if it matches the pattern and is not already used in the dimension.
     *
     * @param dim        the dimension to be checked.
     * @param regionName the name of the region to be checked.
     * @return -1 if the region name is invalid, 0 if the region name is valid, 1 if the region name is already used in the dimension.
     */
    public boolean isAvailableForLocal(ResourceKey<Level> dim, String regionName) {
        if (cacheFor(dim).contains(regionName)) {
            return false;
        }
        return isValidRegionName(dim, regionName) == 0;
    }

    public boolean containsCacheFor(ResourceKey<Level> dim) {
        return dimCacheMap.containsKey(dim);
    }

    public List<String> getFlagsIdsForDim(DimensionRegionCache dimCache) {
        if (dimCache != null) {
            return dimCache.getDimensionalRegion().getFlags().flags()
                    .stream()
                    .map(IFlag::getName)
                    .collect(Collectors.toList());
        }
        return new ArrayList<>();
    }

    public DimensionRegionCache newCacheFor(ResourceKey<Level> dim) {
        DimensionRegionCache cache = new DimensionRegionCache(dim);
        Set<String> defaultDimFlags = Services.REGION_CONFIG.getDefaultDimFlags();
        addFlags(defaultDimFlags, cache.getDimensionalRegion());
        cache.getDimensionalRegion().setIsActive(Services.REGION_CONFIG.shouldActivateNewDimRegion());
        globalRegion.addChild(cache.getDimensionalRegion());
        dimCacheMap.put(dim, cache);
        dimensionDataNames.add(cache.getDimensionalRegion().getName());
        return cache;
    }
}
