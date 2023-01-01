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

import javax.annotation.Nullable;
import java.util.*;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.util.constants.RegionNBT.DIMENSIONS;

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
        RegionDataManager.get().setDirty();
    }

    /**
     * TODO: Should be used in CLI to provide a list of valid dimensions?
     *
     * @return
     */
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
     * @param event
     */
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
            this.dimensionDataNames.add(dimKey);
            RegistryKey<World> dimension = RegistryKey.create(Registry.DIMENSION_REGISTRY, new ResourceLocation(dimKey));
            if (dimensionRegions.contains(dimKey, Constants.NBT.TAG_COMPOUND)) {
                dimCacheMap.put(dimension, new DimensionRegionCache(dimensionRegions.getCompound(dimKey)));
            }

        }
        // set parent and child references
        for (String dimKey : dimensionRegions.getAllKeys()) {
            RegistryKey<World> dimension = RegistryKey.create(Registry.DIMENSION_REGISTRY, new ResourceLocation(dimKey));
            DimensionRegionCache dimCache = dimCacheMap.get(dimension);
            DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
            dimCache.regionsInDimension.values().forEach(region -> {
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
                // set child references
                region.getChildrenNames().forEach(childName -> region.addChild(dimCache.getRegion(childName)));
            });
        }
        save();
    }

    /**
     * Method which gets called the region data is marked as dirty via the save/markDirty method.
     *
     * @param compound nbt data to be filled with the region information.
     * @return the compound region nbt data to be saved to disk.
     */
    @Override
    public CompoundNBT save(CompoundNBT compound) {
        YetAnotherWorldProtector.LOGGER.info(new TranslationTextComponent("data.nbt.dimensions.save.amount", dimCacheMap.entrySet().size()).getString());
        CompoundNBT dimRegionNbtData = new CompoundNBT();
        for (Map.Entry<RegistryKey<World>, DimensionRegionCache> entry : dimCacheMap.entrySet()) {
            String dimensionName = entry.getValue().getDimensionalRegion().getName();
            dimRegionNbtData.put(dimensionName, entry.getValue().serializeNBT());
        }
        compound.put(DIMENSIONS, dimRegionNbtData);
        return compound;
    }

    /**
     * Event handler which creates a new DimensionRegionCache when a dimension is created the first time, by a player loading the dimension.
     *
     * @param event the PlayerChangedDimensionEvent which serves as a trigger and provides the information which dimension the player is traveling to.
     */
    @SubscribeEvent
    public static void addDimKeyOnDimensionChange(PlayerEvent.PlayerChangedDimensionEvent event) {
        if (!event.getPlayer().getCommandSenderWorld().isClientSide) {
            if (!dimCacheMap.containsKey(event.getTo())) {
                DimensionRegionCache cache = RegionDataManager.get().newCacheFor(event.getTo());
                YetAnotherWorldProtector.LOGGER.info("Player traveling to dimension without region data.");
                YetAnotherWorldProtector.LOGGER.info("Init region data for dimension '" + cache.dimensionKey().location() + "'..");
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
            }
        }
    }

    public Collection<String> getAllRegionNames() {
        return dimCacheMap.values().stream()
                .flatMap(regionCache -> regionCache.getRegionNames().stream())
                .collect(Collectors.toList());
    }

    public Collection<String> getRegionNamesFor(RegistryKey<World> dim) {
        return cacheFor(dim).getRegionNames();
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
        save();
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
                            throw new NotImplementedException("");
                        case INT_FLAG:
                            throw new NotImplementedException("");
                    }
                });
    }
}
