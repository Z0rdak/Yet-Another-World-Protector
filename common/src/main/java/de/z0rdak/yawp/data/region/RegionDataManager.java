package de.z0rdak.yawp.data.region;

import de.z0rdak.yawp.api.core.RegionManager;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.flag.BooleanFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.core.region.GlobalRegion;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.platform.Services;
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
import net.minecraft.world.level.storage.LevelResource;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.data.region.GlobalRegionData.GLOBAL_REGION_FILE_NAME;
import static de.z0rdak.yawp.handler.HandlerUtil.isServerSide;

public class RegionDataManager {

    public static final Logger LOGGER = LogManager.getLogger(Constants.MOD_ID.toUpperCase(Locale.ROOT) + "-DataManager");
    private static MinecraftServer serverInstance;
    private static LevelListData savedLevelData = new LevelListData();
    private static GlobalRegionData globalRegionData = new GlobalRegionData();
    private static final Map<ResourceLocation, LevelRegionData> dimRegionStorage = new  HashMap<>();

    public static LevelListData getSavedLevelData() {
        return savedLevelData;
    }

    public static Set<ResourceLocation> getLevels() {
        return new HashSet<>(savedLevelData.getLevels());
    }

    public static boolean hasLevel(ResourceLocation level) {
        return savedLevelData.hasDimEntry(level);
    }

    // TODO: Move to API
    public static Set<String> getLevelNames() {
        return getLevels().stream().map(ResourceLocation::toString).collect(Collectors.toSet());
    }

    public static GlobalRegionData getGlobalRegionData() {
        return globalRegionData;
    }
    public static GlobalRegion getGlobalRegion() {
        return getGlobalRegionData().getGlobal();
    }

    private RegionDataManager() {
    }


    public static void save() {
        save(true);
    }

    public static void save(boolean force) {
        if (force) {
            saveDimList(serverInstance);
            saveGlobalData(serverInstance);
            saveTrackedLevels(serverInstance);
        } else {
            savedLevelData.setDirty();
            globalRegionData.setDirty();
            dimRegionStorage.forEach((key, value) -> value.setDirty());
        }
    }

    public static LevelListData getSavedDims(@Nullable Supplier<LevelListData> defaultSupplier) {
        var overworld = serverInstance.overworld();
        Supplier<LevelListData> supplier = defaultSupplier == null ? LevelListData::new : defaultSupplier;
        DimensionDataStorage storage = overworld.getDataStorage();

        savedLevelData = storage.computeIfAbsent(
                new SavedData.Factory<LevelListData>(supplier, LevelListData::load, DataFixTypes.SAVED_DATA_MAP_DATA),
                LevelListData.TYPE);
        return savedLevelData;
    }

    public static void onServerStarting(MinecraftServer server) {
        LOGGER.info(Component.translatableWithFallback("data.region.init","Initializing RegionDataManager...").getString());
        serverInstance = server;
        checkYawpDir(server);
    }

    private static void saveTrackedLevels(MinecraftServer server){
        server.getAllLevels().forEach(level -> {
            ResourceLocation levelRl = level.dimension().location();
            if (savedLevelData.hasDimEntry(levelRl)) {
                saveLevelData(server, level);
            }
        });
    }

    public static void save(MinecraftServer server, boolean flush, boolean force) {
        LOGGER.info(Component.translatableWithFallback("data.region.levels.save.forced", "Requested save. Saving region data for all levels").getString());
        if (serverInstance == null) serverInstance = server;
        save(force);
    }

    private static void saveDimList(MinecraftServer server) {
        DimensionDataStorage dataStorage = server.overworld().getDataStorage();
        dataStorage.set(LevelListData.TYPE, savedLevelData);
    }

    private static void saveGlobalData(MinecraftServer server) {
        DimensionDataStorage dataStorage = server.overworld().getDataStorage();
        dataStorage.set(Constants.MOD_ID + "/" + GLOBAL_REGION_FILE_NAME, globalRegionData);
    }

    private static void saveLevelData(MinecraftServer server, Level level) {
        DimensionDataStorage storage = server.overworld().getDataStorage();
        ResourceLocation levelRl = level.dimension().location();
        LevelRegionData levelRegionData = dimRegionStorage.get(levelRl);
        LOGGER.info(Component.translatableWithFallback("data.region.level.save", "Saving region data for level '%s'", levelRl.toString()).getString());
        storage.set(LevelRegionData.buildSavedDataType(levelRl), levelRegionData);
        levelRegionData.setDirty();
    }

    public static void saveOnStop(MinecraftServer server) {
        if (serverInstance == null) serverInstance = server;
        LOGGER.info(Component.translatableWithFallback("data.region.levels.save.stopped", "Stopping server. Saving region data for all levels").getString());
        save(true);
    }

    public static void saveOnUnload(MinecraftServer server, ServerLevel level) {
        if (savedLevelData.hasDimEntry(level.dimension().location())) {
            LOGGER.info(Component.translatableWithFallback("data.region.level.save.unload", "Unloading level '%s'. Saving region data", level.dimension().location().toString()).getString());
            saveLevelData(server, level);
        }
    }

    public static void loadLevelListData(MinecraftServer server) {
        try {
            if (serverInstance == null)
                serverInstance = server;
            var dataStorage = server.overworld().getDataStorage();
            savedLevelData = LevelListData.get(dataStorage, () -> {
                LOGGER.info(Component.translatableWithFallback("data.region.levels.load.missing", "Missing level list for region data (ignore on first startup). Initializing...").getString());
                return new LevelListData();
            });
            saveDimList(server);
            LOGGER.info(Component.translatableWithFallback("data.region.levels.load.success", "Found region data for %s dimension(s)", savedLevelData.getLevels().size()).getString());

            globalRegionData = GlobalRegionData.get(dataStorage, () -> {
                LOGGER.info(Component.translatableWithFallback("data.region.global.missing", "Missing global region data (ignore on first startup). Initializing...").getString());
                return new GlobalRegionData();
            });
            saveGlobalData(server);
        } catch (NullPointerException npe) {
            LOGGER.error(Component.translatableWithFallback("data.region.level.local.load.failed", "Loading level region list failed!").getString(), npe);
        }
    }

    public static void worldLoad(MinecraftServer server, ServerLevel level) {
        try {
            if (serverInstance == null)
                serverInstance = server;
            var levelRl = level.dimension().location();
            var dataStorage = server.overworld().getDataStorage();
            // init level data
            if (savedLevelData.hasDimEntry(levelRl)) {
                LevelRegionData levelRegionData = LevelRegionData.get(dataStorage, levelRl, () -> {
                    LOGGER.info(Component.translatableWithFallback("data.region.level.local.missing", "Initializing region data for '%s'", levelRl.toString()).getString());
                    return new LevelRegionData(levelRl);
                });
                LOGGER.info(Component.translatableWithFallback("data.region.level.local.load.success", "Loaded %s region(s) for '%s'", levelRegionData.regionCount(), levelRl.toString()).getString());
                dimRegionStorage.put(levelRl, levelRegionData);
                savedLevelData.addDimEntry(levelRl);

                // restoring region hierarchy
                LOGGER.info(Component.translatableWithFallback("data.region.level.local.load.restore", "Restoring region hierarchy for '%s'.", levelRl.toString()).getString());

                // restore dim <-> global hierarchy
                DimensionalRegion dimensionalRegion = levelRegionData.getDim();
                RegionManager.get().getGlobalRegion().addChild(dimensionalRegion);
                restoreHierarchy(levelRegionData, dimensionalRegion);

                // restore dim <-> local <-> local hierarchy
                levelRegionData.getLocals().forEach((regionName, region) -> {
                    restoreHierarchy(dimRegionStorage.get(levelRl), region);
                });
            }
        } catch (NullPointerException npe) {
            LOGGER.error(Component.translatableWithFallback("data.region.level.local.load.failed", "Loading regions failed!").getString(), npe);
        }
    }
    
    private static void restoreHierarchy(LevelRegionData levelRegionData, IProtectedRegion region) {
        ArrayList<String> childNames = new ArrayList<>(region.getChildrenNames());
        childNames.forEach(childName -> {
            if (!levelRegionData.hasLocal(childName)) {
                LOGGER.warn(Component.translatableWithFallback("data.region.level.local.load.restore.failed", "No region with name '%s' found in save data of '%s'! Your region data is most likely corrupt.", childName, levelRegionData.getId().toString()).getString());
            } else {
                IMarkableRegion child = levelRegionData.getLocal(childName);
                if (child != null) {
                    levelRegionData.getDim().removeChild(child);
                    region.addChild(child);
                }
            }
        });
    }

    private static void checkYawpDir(MinecraftServer server) {
        Path worldRootPath = server.getWorldPath(LevelResource.ROOT).normalize();
        Path dataDirPath = worldRootPath.resolve("data/" + Constants.MOD_ID);
        if (Files.notExists(dataDirPath)) {
            try {
                Files.createDirectories(dataDirPath);
                LOGGER.info(Component.translatableWithFallback("data.region.env.init", "Created region data directory '%s'", dataDirPath.toString()).getString());
            } catch (IOException e) {
                LOGGER.error(Component.translatableWithFallback("data.region.env.error", "Failed to create directory for region data: '%s'").getString(), e);
                throw new RuntimeException(e);
            }
        }
    }


    public static void initLevelDataOnLogin(Entity entity, Level level) {
        if (isServerSide(level) && entity instanceof Player) {
            initLevelData(level.dimension().location());
        }
    }

    public static void initLevelDataOnChangeWorld(Player player, Level srcLvl, Level dstLvl) {
        if (isServerSide(srcLvl)) {
            initLevelData(dstLvl.dimension().location());
        }
    }

    private static LevelRegionData initLevelData(ResourceLocation rl){
        if (!savedLevelData.hasDimEntry(rl)) {
            LevelRegionData levelRegionData = new LevelRegionData(rl);
            DimensionalRegion dimensionalRegion = levelRegionData.getDim();
            // add default flags from config
            Set<String> defaultDimFlags = Services.REGION_CONFIG.getDefaultDimFlags();
            defaultDimFlags.stream()
                    .map(RegionFlag::fromId)
                    .forEach(flag -> dimensionalRegion.addFlag(new BooleanFlag(flag)));
            // set state from config
            dimensionalRegion.setIsActive(Services.REGION_CONFIG.shouldActivateNewDimRegion());
            // add as child of global
            RegionManager.get().getGlobalRegion().addChild(dimensionalRegion);

            dimRegionStorage.put(rl, levelRegionData);
            savedLevelData.addDimEntry(rl);
            LOGGER.info(Component.translatableWithFallback("data.region.level.init", "Initializing region data for level '%s'", rl.toString()).getString());
            save(false);
            return levelRegionData;
        }
        return dimRegionStorage.get(rl);
    }

    public static Optional<LevelRegionData> getLevelRegionData(ResourceLocation rl) {
        if (!savedLevelData.hasDimEntry(rl)) {
            return Optional.empty();
        }
        return Optional.of(dimRegionStorage.get(rl));
    }

    public static Optional<LevelRegionData> getLevelRegionData(ResourceKey<Level> dim) {
        return getLevelRegionData(dim.location());
    }

    public static LevelRegionData getOrCreate(ResourceLocation rl) {
        if (!savedLevelData.hasDimEntry(rl)) {
            return initLevelData(rl);
        }
        return dimRegionStorage.get(rl);
    }

    public static LevelRegionData getOrCreate(Level level)  {
        return getOrCreate(level.dimension().location());
    }

    public static Collection<IMarkableRegion> getLocalsFor(ResourceKey<Level> dim) {
        return getOrCreate(dim.location()).getLocalList();
    }

    public static LevelRegionData getOrCreate(ResourceKey<Level> dim) {
       return getOrCreate(dim.location());
    }

    public static void resetLevelData(ResourceLocation rl) {
        dimRegionStorage.remove(rl);
    }

    public static void resetLevelData(ResourceKey<Level> dim) {
        resetLevelData(dim.location());
    }
}
