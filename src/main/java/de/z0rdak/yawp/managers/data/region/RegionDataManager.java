package de.z0rdak.yawp.managers.data.region;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.commands.CommandConstants;
import de.z0rdak.yawp.commands.arguments.region.RegionArgumentType;
import de.z0rdak.yawp.config.server.RegionConfig;
import de.z0rdak.yawp.core.flag.BooleanFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.GlobalRegion;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import net.minecraft.entity.player.PlayerEntity;
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
import net.minecraftforge.common.util.Constants;
import net.minecraftforge.event.entity.EntityTravelToDimensionEvent;
import net.minecraftforge.event.entity.player.PlayerEvent;
import net.minecraftforge.eventbus.api.EventPriority;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.event.server.FMLServerStartingEvent;
import net.minecraftforge.fml.server.ServerLifecycleHooks;
import org.apache.commons.lang3.NotImplementedException;

import javax.annotation.Nonnull;
import java.util.*;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.commands.CommandConstants.values;
import static de.z0rdak.yawp.util.constants.RegionNBT.*;

/**
 * Class which manages the region data for the mod. It is responsible for loading and saving the region data to disk and
 * provides methods to access the region data.
 */
@EventBusSubscriber(modid = YetAnotherWorldProtector.MODID)
public class RegionDataManager extends WorldSavedData {

    /**
     * Name which is used for the file to store the NBT data: yawp-dimensions.dat
     */
    private static final String DATA_NAME = YetAnotherWorldProtector.MODID + "-dimensions";
    /**
     * Map which holds the mod region information. Each dimension has its on DimensionRegionCache.
     */
    private final static Map<RegistryKey<World>, DimensionRegionCache> dimCacheMap = new HashMap<>();
    /**
     * The global region of this mod which all sets common rules/flags for all regions.
     */
    private static GlobalRegion globalRegion = new GlobalRegion();
    /**
     * Singleton used to access methods to manage region data.
     */

    private static RegionDataManager regionDataCache = new RegionDataManager();

    private static final Set<String> dimensionDataNames = new HashSet<>();

    private RegionDataManager() {
        super(DATA_NAME);
    }

    public static void save() {
        YetAnotherWorldProtector.LOGGER.debug(new TranslationTextComponent("Save for RegionDataManager called. Attempting to save region data...").getString());
        RegionDataManager.get().setDirty();
    }

    /**
     * Returns the name of all dimension tracked by the region data manager.
     *
     * @return the set of dimension names which are tracked by the region data manager.
     */
    @SuppressWarnings("unused")
    public static Set<String> getDimensionDataNames() {
        return Collections.unmodifiableSet(dimensionDataNames);
    }

    public static List<DimensionRegionCache> getDimensionCaches() {
        return Collections.unmodifiableList(new ArrayList<>(dimCacheMap.values()));
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

    /**
     * Server startup hook for loading the region data from the yawp-dimension.dat file by creating an instance of RegionDataManager.
     *
     * @param event which is fired upon server start and acts as trigger to load region data from disk.
     */
    @SubscribeEvent
    public static void loadRegionData(FMLServerStartingEvent event) {
        try {
            ServerWorld world = Objects.requireNonNull(event.getServer().overworld());
            if (!world.isClientSide) {
                DimensionSavedDataManager storage = world.getDataStorage();
                RegionDataManager data = storage.computeIfAbsent(RegionDataManager::new, DATA_NAME);
                storage.set(data);
                regionDataCache = data;
                // YetAnotherWorldProtector.LOGGER.info(new TranslationTextComponent("data.nbt.dimensions.load.success", data.getTotalRegionAmount(), data.getDimensionAmount()).getString());
                YetAnotherWorldProtector.LOGGER.info(new TranslationTextComponent("Loaded " + data.getTotalRegionAmount() + " region(s) for " + data.getDimensionAmount()).getString() + " dimension(s)");
            }
        } catch (NullPointerException npe) {
            YetAnotherWorldProtector.LOGGER.error(new TranslationTextComponent("Loading regions failed!").getString());
            YetAnotherWorldProtector.LOGGER.error(new TranslationTextComponent(npe.getLocalizedMessage()).getString());
            // .LOGGER.error(new TranslationTextComponent("data.nbt.dimensions.load.failure").getString());
        }
    }

    /**
     * Event handler which is used to initialize the dimension cache with first dimension entry when a player logs in.
     *
     * @param event PlayerLoggedInEvent which serves as a trigger and provides the information about the dimension the player logged in to.
     */
    @SubscribeEvent(priority = EventPriority.HIGHEST, receiveCanceled = true)
    public static void addDimKeyOnPlayerLogin(PlayerEvent.PlayerLoggedInEvent event) {
        if (!event.getPlayer().getCommandSenderWorld().isClientSide) {
            RegistryKey<World> dim = event.getPlayer().getCommandSenderWorld().dimension();
            if (!dimCacheMap.containsKey(dim)) {
                DimensionRegionCache cache = RegionDataManager.get().newCacheFor(dim);
                YetAnotherWorldProtector.LOGGER.info("Player joining to server in dimension without region data. This should only happen the first time a player is joining.");
                YetAnotherWorldProtector.LOGGER.info("Init region data for dimension '" + cache.dimensionKey().location() + "'..");
                save();
            }
        }
    }

    /**
     * Event handler which creates a new DimensionRegionCache when a dimension is created the first time, by a player loading the dimension.     *
     *
     * @param event the EntityTravelToDimensionEvent which serves as a trigger and provides the information which dimension the player is traveling to.
     */
    @SubscribeEvent(priority = EventPriority.HIGHEST, receiveCanceled = true)
    public static void addDimKeyOnDimensionChange(EntityTravelToDimensionEvent event) {
        if (!event.getEntity().getCommandSenderWorld().isClientSide) {
            if (event.getEntity() instanceof PlayerEntity) {
                if (!dimCacheMap.containsKey(event.getDimension())) {
                    DimensionRegionCache cache = regionDataCache.newCacheFor(event.getDimension());
                    YetAnotherWorldProtector.LOGGER.info("Init region data for dimension '" + cache.dimensionKey().location() + "'..");
                    save();
                }
            }
        }
    }

    /**
     * Method which gets called the region data is marked as dirty via the save/markDirty method.
     *
     * @param compound nbt data to be filled with the region information.
     * @return the compound region nbt data to be saved to disk.
     */
    @Nonnull
    @Override
    public CompoundNBT save(@Nonnull CompoundNBT compound) {
        compound.put(GLOBAL, globalRegion.serializeNBT());
        CompoundNBT dimRegionNbtData = new CompoundNBT();
        // YetAnotherWorldProtector.LOGGER.info(new TranslationTextComponent("data.nbt.dimensions.save.amount", this.getTotalRegionAmount(), dimCacheMap.keySet().size()).getString());
        YetAnotherWorldProtector.LOGGER.info(new TranslationTextComponent("Saving " + this.getTotalRegionAmount() + " region(s) for " + dimCacheMap.keySet().size() + " dimensions").getString());
        for (Map.Entry<RegistryKey<World>, DimensionRegionCache> entry : dimCacheMap.entrySet()) {
            // YetAnotherWorldProtector.LOGGER.info(new TranslationTextComponent("data.nbt.dimensions.save.dim.amount", this.getRegionAmount(entry.getKey()), entry.getKey().location().toString()).getString());
            YetAnotherWorldProtector.LOGGER.info(new TranslationTextComponent("Saving " + this.getRegionAmount(entry.getKey()) + " region(s) for dimension '" + entry.getKey().location() + "'").getString());
            String dimensionName = entry.getValue().getDimensionalRegion().getName();
            dimRegionNbtData.put(dimensionName, entry.getValue().serializeNBT());
        }
        compound.put(DIMENSIONS, dimRegionNbtData);
        return compound;
    }

    /**
     * Method which gets called when a new RegionDataManager instance is created by loadRegionData.
     *
     * @param nbt compound region data read from disk to be deserialized for the region cache.
     */
    @Override
    public void load(CompoundNBT nbt) {
        if (!nbt.contains(GLOBAL) || nbt.getCompound(GLOBAL).isEmpty()) {
            YetAnotherWorldProtector.LOGGER.info(new TranslationTextComponent("Missing global region data. Initializing new data. (Ignore this for the first server start)").getString());
            globalRegion = new GlobalRegion();
        } else {
            CompoundNBT globalNbt = nbt.getCompound(GLOBAL);
            globalRegion = new GlobalRegion(globalNbt);
            globalRegion.setParent(globalRegion);
            YetAnotherWorldProtector.LOGGER.info(new TranslationTextComponent("Loaded global region data").getString());
        }

        if (!nbt.contains(DIMENSIONS) || nbt.getCompound(DIMENSIONS).isEmpty()) {
            YetAnotherWorldProtector.LOGGER.info(new TranslationTextComponent("No region data found for dimensions. Initializing new data...").getString());
            dimCacheMap.clear();
        } else {
            CompoundNBT dimensionRegions = nbt.getCompound(DIMENSIONS);
            YetAnotherWorldProtector.LOGGER.info(new TranslationTextComponent("Loading region(s) for " + dimensionRegions.getAllKeys().size() + " dimension(s)").getString());
            dimCacheMap.clear();
            // deserialize all region without parent and child references
            for (String dimKey : dimensionRegions.getAllKeys()) {
                if (dimensionRegions.contains(dimKey, Constants.NBT.TAG_COMPOUND)) {
                    RegistryKey<World> dimension = RegistryKey.create(Registry.DIMENSION_REGISTRY, new ResourceLocation(dimKey));
                    CompoundNBT dimCacheNbt = dimensionRegions.getCompound(dimKey);
                    if (dimCacheNbt.contains(REGIONS, Constants.NBT.TAG_COMPOUND)) {
                        YetAnotherWorldProtector.LOGGER.info(new TranslationTextComponent("Loading " + dimCacheNbt.getCompound(REGIONS).size() + " region(s) for dimension '" + dimKey + "'").getString());
                    } else {
                        YetAnotherWorldProtector.LOGGER.info(new TranslationTextComponent("No region data for dimension '" + dimKey + "' found").getString());
                    }
                    DimensionRegionCache dimCache = new DimensionRegionCache(dimCacheNbt);
                    globalRegion.addChild(dimCache.getDimensionalRegion());
                    dimCacheMap.put(dimension, dimCache);
                    dimensionDataNames.add(dimKey);
                    YetAnotherWorldProtector.LOGGER.info(new TranslationTextComponent("data.nbt.dimensions.loaded.dim.amount", dimCache.getRegions().size(), dimCache.dimensionKey().location().toString()).getString());
                }
            }
        }
        // restore parent/child hierarchy
        dimCacheMap.forEach((dimKey, cache) -> {
            if (!cache.getRegions().isEmpty()) {
                YetAnotherWorldProtector.LOGGER.info(new TranslationTextComponent("Restoring region hierarchy for regions in dimension '" + dimKey + "'").getString());
                ArrayList<IMarkableRegion> regions = new ArrayList<>(cache.getRegionsInDimension().values());
                regions.forEach(region -> {
                    // set child reference
                    region.getChildrenNames().forEach(childName -> {
                        if (!cache.contains(childName)) {
                            YetAnotherWorldProtector.LOGGER.error(new TranslationTextComponent("Corrupt save data. Child region '" + childName + "' not found in dimension '" + dimKey + "'!").getString());
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
    }

    public int getTotalRegionAmount() {
        return dimCacheMap.values().stream()
                .mapToInt(regionCache -> regionCache.getRegionNames().size())
                .sum();
    }

    public int getRegionAmount(RegistryKey<World> dim) {
        return cacheFor(dim).getRegions().size();
    }

    public int getDimensionAmount() {
        return dimCacheMap.keySet().size();
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

    public Collection<IMarkableRegion> getRegionsFor(RegistryKey<World> dim) {
        return cacheFor(dim).getRegions();
    }

    public boolean containsCacheFor(RegistryKey<World> dim) {
        return dimCacheMap.containsKey(dim);
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

    public DimensionRegionCache newCacheFor(RegistryKey<World> dim) {
        DimensionRegionCache cache = new DimensionRegionCache(dim);
        addFlags(RegionConfig.getDefaultDimFlags(), cache.getDimensionalRegion());
        cache.getDimensionalRegion().setIsActive(RegionConfig.shouldActivateNewDimRegion());
        globalRegion.addChild(cache.getDimensionalRegion());
        dimCacheMap.put(dim, cache);
        dimensionDataNames.add(cache.getDimensionalRegion().getName());
        return cache;
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
}
