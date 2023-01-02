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
import net.minecraftforge.common.util.Constants;
import net.minecraftforge.event.entity.player.PlayerEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.event.server.FMLServerStartingEvent;
import net.minecraftforge.fml.server.ServerLifecycleHooks;
import org.apache.commons.lang3.NotImplementedException;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.*;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.util.constants.RegionNBT.DIMENSIONS;
import static de.z0rdak.yawp.util.constants.RegionNBT.REGIONS;

@EventBusSubscriber(modid = YetAnotherWorldProtector.MODID, value = Dist.DEDICATED_SERVER)
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
    private final static GlobalRegion globalRegion = new GlobalRegion();
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
    public static void loadRegionData(FMLServerStartingEvent event) {
        try {
            ServerWorld world = Objects.requireNonNull(event.getServer().overworld());
            if (!world.isClientSide) {
                DimensionSavedDataManager storage = world.getDataStorage();
                RegionDataManager data = storage.computeIfAbsent(RegionDataManager::new, DATA_NAME);
                storage.set(data);
                regionDataCache = data;
                YetAnotherWorldProtector.LOGGER.info(new TranslationTextComponent("data.nbt.dimensions.load.success", data.getTotalRegionAmount(), data.getDimensionAmount()).getString());
            }
        } catch (NullPointerException npe) {
            YetAnotherWorldProtector.LOGGER.error(new TranslationTextComponent("data.nbt.dimensions.load.failure").getString());
        }
    }

    /**
     * Method which gets called when a new RegionDataManager instance is created by loadRegionData.
     *
     * @param nbt compound region data read from disk to be deserialized for the region cache.
     */
    @Override
    public void load(CompoundNBT nbt) {
        dimCacheMap.clear();
        CompoundNBT dimensionRegions = nbt.getCompound(DIMENSIONS);
        YetAnotherWorldProtector.LOGGER.info(new TranslationTextComponent("data.nbt.dimensions.load.amount", dimensionRegions.getAllKeys().size()).getString());
        // deserialize all region without parent and child references
        for (String dimKey : dimensionRegions.getAllKeys()) {
            dimensionDataNames.add(dimKey);
            RegistryKey<World> dimension = RegistryKey.create(Registry.DIMENSION_REGISTRY, new ResourceLocation(dimKey));
            if (dimensionRegions.contains(dimKey, Constants.NBT.TAG_COMPOUND)) {
                CompoundNBT dimCacheNbt = dimensionRegions.getCompound(dimKey);
                if (dimCacheNbt.contains(REGIONS, Constants.NBT.TAG_COMPOUND)) {
                    YetAnotherWorldProtector.LOGGER.info(new TranslationTextComponent("data.nbt.dimensions.load.dim.amount", dimCacheNbt.getCompound(REGIONS).size(), dimKey).getString());
                } else {
                    YetAnotherWorldProtector.LOGGER.info(new TranslationTextComponent("data.nbt.dimensions.load.dim.empty", dimKey).getString());
                }
                dimCacheMap.put(dimension, new DimensionRegionCache(dimCacheNbt));
            }
        }
        dimCacheMap.forEach((dimKey, cache) -> YetAnotherWorldProtector.LOGGER.info(new TranslationTextComponent("data.nbt.dimensions.loaded.dim.amount", cache.getRegions().size(), dimKey.location().toString()).getString()));

        // set parent and child references
        for (String dimKey : dimensionRegions.getAllKeys()) {
            RegistryKey<World> dimension = RegistryKey.create(Registry.DIMENSION_REGISTRY, new ResourceLocation(dimKey));
            DimensionRegionCache dimCache = dimCacheMap.get(dimension);
            if (dimCache.getRegions().size() > 0) {
                YetAnotherWorldProtector.LOGGER.info(new TranslationTextComponent(  "data.nbt.dimensions.load.dim.restore", dimKey).getString());
            }
            DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
            dimCache.regionsInDimension.values().forEach(region -> {
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
        CompoundNBT dimRegionNbtData = new CompoundNBT();
        YetAnotherWorldProtector.LOGGER.info(new TranslationTextComponent("data.nbt.dimensions.save.amount", this.getTotalRegionAmount(), dimCacheMap.keySet().size()).getString());
        for (Map.Entry<RegistryKey<World>, DimensionRegionCache> entry : dimCacheMap.entrySet()) {
            YetAnotherWorldProtector.LOGGER.info(new TranslationTextComponent("data.nbt.dimensions.save.dim.amount", this.getRegionAmount(entry.getKey()), entry.getKey().location().toString()).getString());
            String dimensionName = entry.getValue().getDimensionalRegion().getName();
            dimRegionNbtData.put(dimensionName, entry.getValue().serializeNBT());
        }
        compound.put(DIMENSIONS, dimRegionNbtData);
        return compound;
    }

    /**
     * Event handler which creates a new DimensionRegionCache when a dimension is created the first time, by a player loading the dimension.     *
     *
     * @param event the PlayerChangedDimensionEvent which serves as a trigger and provides the information which dimension the player is traveling to.
     */
    @SubscribeEvent
    public static void addDimKeyOnDimensionChange(PlayerEvent.PlayerChangedDimensionEvent event) {
        if (!event.getPlayer().getCommandSenderWorld().isClientSide) {
            if (!dimCacheMap.containsKey(event.getTo())) {
                DimensionRegionCache cache = RegionDataManager.get().newCacheFor(event.getTo());
                YetAnotherWorldProtector.LOGGER.info("Init region data for dimension '" + cache.dimensionKey().location() + "'..");
                save();
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
            RegistryKey<World> dim = event.getPlayer().getCommandSenderWorld().dimension();
            if (!dimCacheMap.containsKey(dim)) {
                DimensionRegionCache cache = RegionDataManager.get().newCacheFor(dim);
                YetAnotherWorldProtector.LOGGER.info("Player joining to server in dimension without region data. This should only happen the first time a player is joining.");
                YetAnotherWorldProtector.LOGGER.info("Init region data for dimension '" + cache.dimensionKey().location() + "'..");
                save();
            }
        }
    }

    public int getTotalRegionAmount() {
        return (int) dimCacheMap.values().stream()
                .mapToLong(regionCache -> regionCache.getRegionNames().size()).sum();
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

    public Collection<IMarkableRegion> getRegionsFor(RegistryKey<World> dim) {
        return cacheFor(dim).getRegions();
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

    public boolean containsCacheFor(RegistryKey<World> dim) {
        return dimCacheMap.containsKey(dim);
    }

    public DimensionRegionCache cacheFor(RegistryKey<World> dim) {
        return dimCacheMap.get(dim);
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
        cache.setDimState(RegionConfig.shouldActivateNewDimRegion());
        cache.getDimensionalRegion().setParent(globalRegion);
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
