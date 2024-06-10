package de.z0rdak.yawp.managers.data.region;

import com.mojang.datafixers.types.Type;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.commands.CommandConstants;
import de.z0rdak.yawp.commands.arguments.region.RegionArgumentType;
import de.z0rdak.yawp.config.server.RegionConfig;
import de.z0rdak.yawp.core.flag.BooleanFlag;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.core.region.GlobalRegion;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.handler.flags.HandlerUtil;
import net.minecraft.datafixer.DataFixTypes;
import net.minecraft.entity.Entity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.NbtCompound;
import net.minecraft.nbt.NbtElement;
import net.minecraft.registry.RegistryKey;
import net.minecraft.registry.RegistryKeys;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.world.ServerWorld;
import net.minecraft.text.Text;
import net.minecraft.util.Identifier;
import net.minecraft.world.PersistentState;
import net.minecraft.world.PersistentStateManager;
import net.minecraft.world.World;
import org.apache.commons.lang3.NotImplementedException;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.commands.CommandConstants.values;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;
import static de.z0rdak.yawp.util.constants.RegionNBT.*;
import static de.z0rdak.yawp.util.constants.RegionNBT.GLOBAL;

public class RegionDataManager extends PersistentState {

    public static MinecraftServer serverInstance;
    /**
     * Name which is used for the file to store the NBT data: yawp-dimensions.dat
     */
    private static final String DATA_NAME = YetAnotherWorldProtector.MODID + "-dimensions";
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

    public static List<DimensionRegionCache> getDimensionCaches() {
        return Collections.unmodifiableList(new ArrayList<>(regionDataCache.dimCacheMap.values()));
    }

    public static RegionDataManager get() {
        if (regionDataCache == null) {
            if (serverInstance != null) {
                ServerWorld overworld = serverInstance.getOverworld();
                if (!overworld.isClient) {
                    PersistentStateManager storage = overworld.getPersistentStateManager();
                    regionDataCache = storage.getOrCreate(RegionDataManager::load, RegionDataManager::new, DATA_NAME);
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
            if (!serverWorld.isClient && serverWorld.getRegistryKey().getValue().equals(new Identifier("minecraft:overworld"))) {
                PersistentStateManager storage = serverWorld.getPersistentStateManager();
                RegionDataManager data = storage.getOrCreate(RegionDataManager::load, RegionDataManager::new, DATA_NAME);
                storage.set(DATA_NAME, data);
                regionDataCache = data;
                YetAnotherWorldProtector.LOGGER.info(Text.translatableWithFallback("data.nbt.dimensions.load.success", "Loaded %s region(s) for %s dimension(s)", data.getTotalRegionAmount(), data.getDimensionAmount()).getString());
            }
        } catch (NullPointerException npe) {
            YetAnotherWorldProtector.LOGGER.error(Text.translatableWithFallback("data.nbt.dimensions.load.failure", "Loading regions failed!").getString());
        }
    }

    /**
     * Method which gets called when a new RegionDataManager instance is created by loadRegionData.
     *
     * @param nbt compound region data read from disk to be deserialized for the region cache.
     */
    public static RegionDataManager load(NbtCompound nbt) {
        RegionDataManager rdm = new RegionDataManager();
        rdm.dimCacheMap.clear();
        if (!nbt.contains(GLOBAL) || nbt.getCompound(GLOBAL).isEmpty()) {
            YetAnotherWorldProtector.LOGGER.info(Text.translatable("Missing global region data. Initializing new data. (Ignore this for the first server start)").getString());
            globalRegion = new GlobalRegion();
        } else {
            NbtCompound globalNbt = nbt.getCompound(GLOBAL);
            globalRegion = new GlobalRegion(globalNbt);
            globalRegion.setParent(globalRegion);
            YetAnotherWorldProtector.LOGGER.info(Text.translatable("Loaded global region data").getString());
        }

        if (!nbt.contains(DIMENSIONS) || nbt.getCompound(DIMENSIONS).isEmpty()) {
            YetAnotherWorldProtector.LOGGER.info(Text.translatable("No region data found for dimensions. Initializing new data...").getString());
            rdm.dimCacheMap.clear();
        } else {
            NbtCompound dimensionRegions = nbt.getCompound(DIMENSIONS);
            YetAnotherWorldProtector.LOGGER.info(Text.translatable("Loading region(s) for " + dimensionRegions.getKeys().size() + " dimension(s)").getString());
            rdm.dimCacheMap.clear();
            // deserialize all region without parent and child references
            for (String dimKey : dimensionRegions.getKeys()) {
                if (dimensionRegions.contains(dimKey, NbtElement.COMPOUND_TYPE)) {
                    RegistryKey<World> dimension = RegistryKey.of(RegistryKeys.WORLD, new Identifier(dimKey));
                    NbtCompound dimCacheNbt = dimensionRegions.getCompound(dimKey);
                    if (dimCacheNbt.contains(REGIONS, NbtElement.COMPOUND_TYPE)) {
                        YetAnotherWorldProtector.LOGGER.info(Text.translatable("Loading " + dimCacheNbt.getCompound(REGIONS).getSize() + " region(s) for dimension '" + dimKey + "'").getString());
                    } else {
                        YetAnotherWorldProtector.LOGGER.info(Text.translatable("No region data for dimension '" + dimKey + "' found").getString());
                    }
                    DimensionRegionCache dimCache = new DimensionRegionCache(dimCacheNbt);
                    globalRegion.addChild(dimCache.getDimensionalRegion());
                    rdm.dimCacheMap.put(dimension, dimCache);
                    rdm.dimensionDataNames.add(dimKey);
                    YetAnotherWorldProtector.LOGGER.info(Text.translatableWithFallback("data.nbt.dimensions.loaded.dim.amount", "Loaded %s region(s) for dimension %s", dimCache.getRegions().size(), dimCache.getDimensionalRegion().getName()).getString());
                }
            }
        }
        // restore parent/child hierarchy
        rdm.dimCacheMap.forEach((dimKey, cache) -> {
            if (!cache.getRegions().isEmpty()) {
                YetAnotherWorldProtector.LOGGER.info(Text.translatable("Restoring region hierarchy for regions in dimension '" + dimKey + "'").getString());
                ArrayList<IMarkableRegion> regions = new ArrayList<>(cache.getRegionsInDimension().values());
                regions.forEach(region -> {
                    // set child reference
                    region.getChildrenNames().forEach(childName -> {
                        if (!cache.contains(childName)) {
                            YetAnotherWorldProtector.LOGGER.error(Text.translatable("Corrupt save data. Child region '" + childName + "' not found in dimension '" + dimKey + "'!").getString());
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
    public static void onPlayerChangeWorldAddDimKey(PlayerEntity playerEntity, ServerWorld origin, ServerWorld destination) {
        if (isServerSide(destination)) {
            if (!regionDataCache.dimCacheMap.containsKey(destination.getRegistryKey())) {
                DimensionRegionCache cache = RegionDataManager.get().newCacheFor(destination.getRegistryKey());
                YetAnotherWorldProtector.LOGGER.info("Init region data for dimension '" + cache.dimensionKey().getValue() + "'..");
                save();
            }
        }
    }

    /**
     * Event handler which is used to initialize the dimension cache with first dimension entry when a player logs in.
     */
    public static void onPlayerLoadAddDimKey(Entity entity, ServerWorld serverWorld) {
        if (isServerSide(serverWorld) && entity instanceof PlayerEntity) {
            RegistryKey<World> dim = serverWorld.getRegistryKey();
            if (!regionDataCache.dimCacheMap.containsKey(dim)) {
                DimensionRegionCache cache = regionDataCache.newCacheFor(dim);
                YetAnotherWorldProtector.LOGGER.info("Player joining to server in dimension without region data. This should only happen the first time a player is joining.");
                YetAnotherWorldProtector.LOGGER.info("Init region data for dimension '" + cache.dimensionKey().getValue() + "'..");
                save();
            }
        }
    }

    /**
     * Method which gets called the region data is marked as dirty via the save/markDirty method.
     *
     * @param compound nbt data to be filled with the region information.
     * @return the compound region nbt data to be saved to disk.
     */
    @Override
    public NbtCompound writeNbt(@NotNull NbtCompound compound) {
        compound.put(GLOBAL, globalRegion.serializeNBT());
        NbtCompound dimRegionNbtData = new NbtCompound();
        // YetAnotherWorldProtector.LOGGER.info(new TranslationTextComponent("data.nbt.dimensions.save.amount", this.getTotalRegionAmount(), dimCacheMap.keySet().size()).getString());
        YetAnotherWorldProtector.LOGGER.info(Text.translatable("Saving " + this.getTotalRegionAmount() + " region(s) for " + dimCacheMap.keySet().size() + " dimensions").getString());
        for (Map.Entry<RegistryKey<World>, DimensionRegionCache> entry : dimCacheMap.entrySet()) {
            // YetAnotherWorldProtector.LOGGER.info(new TranslationTextComponent("data.nbt.dimensions.save.dim.amount", this.getRegionAmount(entry.getKey()), entry.getKey().location().toString()).getString());
            YetAnotherWorldProtector.LOGGER.info(Text.translatable("Saving " + this.getRegionAmount(entry.getKey()) + " region(s) for dimension '" + entry.getKey().getValue() + "'").getString());
            String dimensionName = entry.getValue().getDimensionalRegion().getName();
            dimRegionNbtData.put(dimensionName, entry.getValue().serializeNBT());
        }
        compound.put(DIMENSIONS, dimRegionNbtData);
        return compound;
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

    public int getTotalRegionAmount() {
        return dimCacheMap.values().stream()
                .mapToInt(regionCache -> regionCache.getRegionNames().size())
                .sum();
    }

    public int getRegionAmount(RegistryKey<World> dim) {
        return cacheFor(dim).getRegions().size();
    }

    public Collection<String> getDimensionList() {
        return dimCacheMap.keySet().stream()
                .map(entry -> entry.getValue().toString())
                .collect(Collectors.toList());
    }

    public GlobalRegion getGlobalRegion() {
        return globalRegion;
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

    /**
     * Method to check if a region name is valid for a given dimension. <br></br>
     * A region name is valid if it matches the pattern and is not already used in the dimension.
     *
     * @param dim        the dimension to be checked.
     * @param regionName the name of the region to be checked.
     * @return -1 if the region name is invalid, 0 if the region name is valid, 1 if the region name is already used in the dimension.
     */
    public int isValidRegionName(RegistryKey<World> dim, String regionName) {
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

    public boolean containsCacheFor(RegistryKey<World> dim) {
        return dimCacheMap.containsKey(dim);
    }

    public List<String> getFlagsIdsForDim(DimensionRegionCache dimCache) {
        if (dimCache != null) {
            return dimCache.getDimensionalRegion().getFlags()
                    .stream()
                    .map(IFlag::getName)
                    .collect(Collectors.toList());
        }
        return new ArrayList<>();
    }

    public DimensionRegionCache newCacheFor(RegistryKey<World> dim) {
        DimensionRegionCache cache = new DimensionRegionCache(dim);
        addFlags(RegionConfig.getDefaultDimFlags(), cache.getDimensionalRegion());
        cache.getDimensionalRegion().setIsActive(RegionConfig.shouldActivateNewDimRegion());
        globalRegion.addChild(cache.getDimensionalRegion());
        dimCacheMap.put(dim, cache);
        dimensionDataNames.add(cache.getDimensionalRegion().getName());
        return cache;
    }
}
