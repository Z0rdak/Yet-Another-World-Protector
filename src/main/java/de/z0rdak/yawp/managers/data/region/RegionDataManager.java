package de.z0rdak.yawp.managers.data.region;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.commands.CommandConstants;
import de.z0rdak.yawp.commands.arguments.region.RegionArgumentType;
import de.z0rdak.yawp.config.server.RegionConfig;
import de.z0rdak.yawp.core.flag.BooleanFlag;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.*;
import net.minecraft.core.Registry;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.Tag;
import net.minecraft.network.chat.TranslatableComponent;
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

import static de.z0rdak.yawp.commands.CommandConstants.values;
import static de.z0rdak.yawp.util.constants.RegionNBT.*;

/**
 * Class which manages the region data for the mod. It is responsible for loading and saving the region data to disk and
 * provides methods to access the region data.
 */
@Mod.EventBusSubscriber(modid = YetAnotherWorldProtector.MODID)
public class RegionDataManager extends SavedData {

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
    private final Map<ResourceKey<Level>, DimensionRegionCache> dimCacheMap = new HashMap<>();
    private final Set<String> dimensionDataNames = new HashSet<>();

    private RegionDataManager() {
    }

    public static void save() {
        YetAnotherWorldProtector.LOGGER.debug(new TranslatableComponent("Save for RegionDataManager called. Attempting to save region data...").getString());
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
                YetAnotherWorldProtector.LOGGER.info(new TranslatableComponent("data.nbt.dimensions.load.success", data.getTotalRegionAmount(), data.getDimensionAmount()).getString());
            }
        } catch (NullPointerException npe) {
            YetAnotherWorldProtector.LOGGER.error(new TranslatableComponent("data.nbt.dimensions.load.failure").getString());
        }
    }

    /**
     * Method which gets called when a new RegionDataManager instance is created by loadRegionData.
     *
     * @param nbt compound region data read from disk to be deserialized for the region cache.
     */
    public static RegionDataManager load(CompoundTag nbt) {
        RegionDataManager rdm = new RegionDataManager();
        rdm.dimCacheMap.clear();
        CompoundTag globalNbt = nbt.getCompound(GLOBAL);
        if (globalNbt.isEmpty()) {
            YetAnotherWorldProtector.LOGGER.info(new TranslatableComponent("Missing global region data. Initializing new data. (Ignore this for the first server start)").getString());
            globalRegion = new GlobalRegion();
        } else {
            globalRegion = new GlobalRegion(globalNbt);
            YetAnotherWorldProtector.LOGGER.info(new TranslatableComponent("Loaded global region data").getString());
        }

        rdm.dimCacheMap.clear();
        CompoundTag dimensionRegions = nbt.getCompound(DIMENSIONS);
        YetAnotherWorldProtector.LOGGER.info(new TranslatableComponent("Loading region(s) for " + dimensionRegions.getAllKeys().size() + " dimension(s)").getString());
        // YetAnotherWorldProtector.LOGGER.info(new TranslationTextComponent("data.nbt.dimensions.load.amount", dimensionRegions.getAllKeys().size()).getString());
        // deserialize all region without parent and child references
        for (String dimKey : dimensionRegions.getAllKeys()) {
            rdm.dimensionDataNames.add(dimKey);
            ResourceKey<Level> dimension = ResourceKey.create(Registry.DIMENSION_REGISTRY, new ResourceLocation(dimKey));
            if (dimensionRegions.contains(dimKey, Tag.TAG_COMPOUND)) {
                CompoundTag dimCacheNbt = dimensionRegions.getCompound(dimKey);
                if (dimCacheNbt.contains(REGIONS, Tag.TAG_COMPOUND)) {
                    YetAnotherWorldProtector.LOGGER.info(new TranslatableComponent("data.nbt.dimensions.load.dim.amount", dimCacheNbt.getCompound(REGIONS).size(), dimKey).getString());
                } else {
                    YetAnotherWorldProtector.LOGGER.info(new TranslatableComponent("data.nbt.dimensions.load.dim.empty", dimKey).getString());
                }
                rdm.dimCacheMap.put(dimension, new DimensionRegionCache(dimCacheNbt));
            }
        }
        rdm.dimCacheMap.forEach((dimKey, cache) -> YetAnotherWorldProtector.LOGGER.info(new TranslatableComponent("data.nbt.dimensions.loaded.dim.amount", cache.getRegions().size(), dimKey.location().toString()).getString()));

        // set parent and child references
        for (String dimKey : dimensionRegions.getAllKeys()) {
            ResourceKey<Level> dimension = ResourceKey.create(Registry.DIMENSION_REGISTRY, new ResourceLocation(dimKey));
            DimensionRegionCache dimCache = rdm.dimCacheMap.get(dimension);
            if (!dimCache.getRegions().isEmpty()) {
                YetAnotherWorldProtector.LOGGER.info(new TranslatableComponent("data.nbt.dimensions.load.dim.restore", dimKey).getString());
                DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                dimCache.getRegionsInDimension().values().forEach(region -> {
                    // set child references
                    region.getChildrenNames().forEach(childName -> {
                        if (!dimCache.contains(childName)) {
                            YetAnotherWorldProtector.LOGGER.error(new TranslationTextComponent("Corrupt save data. Child region '" + childName + "' not found in dimension '" + dimKey + "'!").getString());
                        } else {
                            region.addChild(dimCache.getRegion(childName));
                        }
                    });
                    // set parent reference
                    String parentName = region.getParentName();
                    boolean hasValidParent = parentName != null && !parentName.isEmpty(); // hasNonEmptyStringParent rather
                    if (hasValidParent) {
                        if (region.getRegionType() == RegionType.LOCAL && parentName.equals(dimRegion.getName())) {
                            dimRegion.addChild(region);
                        } else {
                            if (!dimCache.contains(parentName)) {
                                YetAnotherWorldProtector.LOGGER.error(new TranslationTextComponent("Corrupt save data. Parent region '" + parentName + "' not found in dimension '" + dimKey + "'!").getString());
                            } else {
                                IMarkableRegion parent = dimCache.getRegion(parentName);
                                if (parent == null) {
                                    YetAnotherWorldProtector.LOGGER.error(new TranslationTextComponent("Corrupt save data. Parent region '" + parentName + "' not found in dimension '" + dimKey + "'!").getString());
                                } else {
                                    parent.addChild(region);
                                }
                            }
                        }
                    }
                });
            }
            globalRegion.addChild(dimCache.getDimensionalRegion());
        }
        globalRegion.addChild(globalRegion);
        return rdm;
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
        compound.put(GLOBAL, globalRegion.serializeNBT());
        CompoundTag dimRegionNbtData = new CompoundTag();
        // YetAnotherWorldProtector.LOGGER.info(new TranslationTextComponent("data.nbt.dimensions.save.amount", this.getTotalRegionAmount(), dimCacheMap.keySet().size()).getString());
        YetAnotherWorldProtector.LOGGER.info(new TranslatableComponent("Saving " + this.getTotalRegionAmount() + " region(s) for " + dimCacheMap.keySet().size() + " dimensions").getString());
        for (Map.Entry<ResourceKey<Level>, DimensionRegionCache> entry : dimCacheMap.entrySet()) {
            // YetAnotherWorldProtector.LOGGER.info(new TranslationTextComponent("data.nbt.dimensions.save.dim.amount", this.getRegionAmount(entry.getKey()), entry.getKey().location().toString()).getString());
            YetAnotherWorldProtector.LOGGER.info(new TranslatableComponent("Saving " + this.getRegionAmount(entry.getKey()) + " region(s) for dimension '" + entry.getKey().location() + "'").getString());
            String dimensionName = entry.getValue().getDimensionalRegion().getName();
            dimRegionNbtData.put(dimensionName, entry.getValue().serializeNBT());
        }
        compound.put(DIMENSIONS, dimRegionNbtData);
        return compound;
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
        if (!event.getPlayer().getCommandSenderWorld().isClientSide) {
            ResourceKey<Level> dim = event.getPlayer().getCommandSenderWorld().dimension();
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

    public int getTotalRegionAmount() {
        return dimCacheMap.values().stream()
                .mapToInt(regionCache -> regionCache.getRegionNames().size())
                .sum();
    }

    public int getRegionAmount(ResourceKey<Level> dim) {
        return cacheFor(dim).getRegions().size();
    }

    public Collection<String> getDimensionList() {
        return dimCacheMap.keySet().stream()
                .map(entry -> entry.location().toString())
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

    /**
     * Method to check if a region name is valid for a given dimension. <br></br>
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

    public boolean containsCacheFor(ResourceKey<Level> dim) {
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

    public DimensionRegionCache newCacheFor(ResourceKey<Level> dim) {
        DimensionRegionCache cache = new DimensionRegionCache(dim);
        addFlags(RegionConfig.getDefaultDimFlags(), cache.getDimensionalRegion());
        cache.getDimensionalRegion().setIsActive(RegionConfig.shouldActivateNewDimRegion());
        globalRegion.addChild(cache.getDimensionalRegion());
        dimCacheMap.put(dim, cache);
        dimensionDataNames.add(cache.getDimensionalRegion().getName());
        return cache;
    }
}
