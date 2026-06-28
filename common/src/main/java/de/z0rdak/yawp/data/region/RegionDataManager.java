package de.z0rdak.yawp.data.region;

import de.z0rdak.yawp.api.FlagRegister;
import de.z0rdak.yawp.api.core.RegionManager;
import de.z0rdak.yawp.api.core.region.hierarchy.RegionHierarchy;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.flag.BooleanFlag;

import de.z0rdak.yawp.core.region.GlobalRegion;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.Identifier;
import net.minecraft.resources.ResourceKey;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.saveddata.SavedDataType;
import net.minecraft.world.level.storage.SavedDataStorage;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.handler.HandlerUtil.isServerSide;
import static de.z0rdak.yawp.util.ChatLinkBuilder.buildRegionInfoLink;

public class RegionDataManager {

    public static final Logger LOGGER = LogManager.getLogger(Constants.MOD_ID.toUpperCase() + "-RegionDataManager");
    private static MinecraftServer serverInstance;
    /**
     * Implicit assumption: If savedLevelData contains a level, its also present in dimRegionStorage
     */
    private static LevelListData trackedLevelData;
    private static GlobalRegionData globalRegionData = new GlobalRegionData();
    private static final Map<Identifier, LevelData> levelRegionData = new  HashMap<>();

    public static LevelListData getTrackedLevelData() {
        return trackedLevelData;
    }

    public static Set<Identifier> getLevels() {
        return new HashSet<>(trackedLevelData.getLevels());
    }

    public static boolean hasLevel(Identifier level) {
        return trackedLevelData.doesTrack(level);
    }

    public static Set<String> getLevelNames() {
        return getLevels().stream().map(Identifier::toString).collect(Collectors.toSet());
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
        saveTrackedLevelList();
        saveGlobalData();
        saveTrackedLevels();
    }

    public static LevelListData getSavedDims() {
        if (trackedLevelData == null) {
            if (serverInstance != null) {
                ServerLevel overworld = serverInstance.overworld();
                if (!overworld.isClientSide()) {
                    SavedDataStorage storage = overworld.getDataStorage();
                    trackedLevelData = storage.computeIfAbsent(LevelListData.TYPE);
                }
            }
        }
        return trackedLevelData;
    }

    public static void onServerStarting(MinecraftServer server) {
        LOGGER.debug("Initializing RegionDataManager");
        serverInstance = server;
    }

    private static void saveTrackedLevels(){
        serverInstance.getAllLevels().forEach(RegionDataManager::saveLevelData);
    }

    public static void save(MinecraftServer server, boolean flush, boolean force) {
        if (serverInstance == null) serverInstance = server;
        save();
    }

    public static void saveLevel(Identifier rl) {
        if (!trackedLevelData.doesTrack(rl)) {
            return;
        }
        saveLevelData(rl);
    }

    public static void saveLevel(ServerLevel level) {
        saveLevel(level.dimension().identifier());
    }

    // Duplicated because I want the logging info at a common place and not in the hooks of the mod-loaders
    public static void saveOnUnload(ServerLevel level) {
        var levelRl = level.dimension().identifier();
        if (!trackedLevelData.doesTrack(levelRl)) {
            return;
        }
        LOGGER.info(Component.translatableWithFallback(  "data.region.levels.save.unload", "Unloading level '%s'. Saving region data", level.dimension().identifier().toString()).getString());
        saveLevelData(level);
    }

    private static void saveTrackedLevelList() {
        SavedDataStorage dataStorage = serverInstance.overworld().getDataStorage();
        dataStorage.set(LevelListData.TYPE, trackedLevelData);
        trackedLevelData.setDirty();
    }

    public static void saveGlobalData() {
        SavedDataStorage dataStorage = serverInstance.overworld().getDataStorage();
        dataStorage.set(GlobalRegionData.TYPE, globalRegionData);
        globalRegionData.setDirty();
    }

    private static void saveLevelData(ServerLevel level) {
        saveLevelData(level.dimension().identifier());
    }

    private static void saveLevelData(Identifier levelRl) {
        if (trackedLevelData.doesTrack(levelRl)) {
            SavedDataStorage storage = serverInstance.overworld().getDataStorage();
            LevelData levelData = RegionDataManager.levelRegionData.get(levelRl);
            LOGGER.debug(Component.translatableWithFallback("data.region.levels.save", "Saving region data for level '%s' (%s local region(s))", levelRl.toString(), levelData.regionCount()).getString());
            storage.set(LevelData.buildSavedDataType(levelRl), levelData);
            levelData.setDirty();
        }
    }

    @Nullable
    private static LevelData getStorageForLevel(MinecraftServer server, Identifier levelId) {
        SavedDataStorage storage = server.overworld().getDataStorage();
        var savedDataType = LevelData.buildSavedDataType(levelId);
        return storage.get(savedDataType);
    }

    public static void saveOnStop(MinecraftServer server) {
        if (serverInstance == null) serverInstance = server;
        LOGGER.info(Component.translatableWithFallback("data.region.levels.save.stopped", "Stopping server. Saving region data for all levels").getString());
        save();
    }

    public static void saveOnUnload(MinecraftServer server, ServerLevel level) {
        if (trackedLevelData.doesTrack(level.dimension().identifier())) {
            LOGGER.info(Component.translatableWithFallback("data.region.levels.save.unload", "Unloading level '%s'. Saving region data", level.dimension().identifier().toString()).getString());
            saveLevelData(level);
        }
    }

    public static void loadLevelListData(MinecraftServer server) {
        try {
            if (serverInstance == null)
                serverInstance = server;
            SavedDataStorage dataStorage = server.overworld().getDataStorage();
            trackedLevelData = dataStorage.get(LevelListData.TYPE);
            if (trackedLevelData == null) {
                LOGGER.info(Component.translatableWithFallback("data.region.levels.load.missing", "Missing level list for region data (ignore on first startup). Initializing...").getString());
                trackedLevelData = new LevelListData();
                saveTrackedLevelList();
            } else {
                LOGGER.info(Component.translatableWithFallback("data.region.levels.load.found", "Found region data for %s tracked level(s)", trackedLevelData.getLevels().size()).getString());
            }
            globalRegionData = dataStorage.get(GlobalRegionData.TYPE);
            if (globalRegionData == null) {
                LOGGER.info(Component.translatableWithFallback("data.region.global.missing", "Missing global region data (ignore on first startup). Initializing...").getString());
                globalRegionData = new GlobalRegionData();
                saveGlobalData();
            } else {
                LOGGER.info(Component.translatableWithFallback("data.region.global.success", "Loaded global region data").getString());
            }

        } catch (NullPointerException npe) {
            LOGGER.error(Component.translatableWithFallback("data.region.level.local.load.failed", "Loading level region list failed!").getString(), npe);
        }
    }

    public static void worldLoad(MinecraftServer server, ServerLevel level) {
        try {
            var global = RegionManager.get().getGlobalRegion();
            if (global.getParent() == null)
                RegionHierarchy.setParent(global, global);
            if (serverInstance == null)
                serverInstance = server;
            var levelId = level.dimension().identifier();
            // init level data
            if (trackedLevelData.doesTrack(levelId)) {
                var levelData = getStorageForLevel(server, levelId);
                if (levelData == null) {
                    levelData = new LevelData(levelId);
                    saveLevelData(level);
                    LOGGER.info(Component.translatableWithFallback("data.region.levels.load.missing", "Missing level list for region data (ignore on first startup). Initializing...", levelId.toString()).getString());
                } else {
                    levelRegionData.put(levelId, levelData);
                    trackedLevelData.addTrackingFor(levelId);
                    LOGGER.info(Component.translatableWithFallback("data.region.level.local.load.success", "Loaded %s region(s) for '%s'", levelData.regionCount(), levelId.toString()).getString());
                }
                // restoring region hierarchy
                LOGGER.debug(Component.translatableWithFallback("data.region.level.local.load.restore", "Restoring region hierarchy for '%s'", levelId.toString()).getString());
                // restore dim <-> global hierarchy
                var levelRegion = levelData.getDim();
                RegionHierarchy.setParent(levelRegion, global);
                // restore dim <-> local <-> local hierarchy
                restoreHierarchy(levelData, levelRegion);
                // restore local <-> local hierarchy
                levelData.getLocals().forEach((regionName, region) -> {
                    restoreHierarchy(RegionDataManager.levelRegionData.get(levelId), region);
                });
                levelData.getLocals().forEach((regionName, region) -> {
                    RegionHierarchy.normalizePriorityTree(region);
                });
            }
            Services.YAWP_EVENT_DISPATCHER.post(level);
        } catch (NullPointerException npe) {
            LOGGER.error(Component.translatableWithFallback(  "data.region.levels.load.failure", "Loading regions failed!").getString(), npe);
        }
    }
    
    private static void restoreHierarchy(LevelData levelData, IProtectedRegion parent) {
        for (var childId : parent.getChildrenIds()) {
            if (!levelData.hasLocal(childId)) {
                // TODO get name and output it also
                LOGGER.warn(Component.translatableWithFallback("data.region.level.local.load.restore.failed", "No region with name '%s' found in save data of '%s'! Your region data is most likely corrupt.", childId.toString(), levelData.getId().toString()).getString());
                continue;
            }
            var child = levelData.getLocal(childId);
            if (child == null) {
                LOGGER.error(Component.translatableWithFallback("data.region.level.local.load.restore.error", "Region with name '%s' was not found! Your region data is corrupt.", childId, levelData.getId().toString()).getString());
                continue;
            }
            RegionHierarchy.setParent(child, parent);
        }
    }

    public static void initLevelDataOnLogin(Entity entity, Level level) {
        if (isServerSide(level) && entity instanceof Player) {
            var shouldCreateNewLevelData = Services.FEATURE_MANAGER.shouldCreateNewLevelData();
            if (shouldCreateNewLevelData) {
                RegionManager.get().trackLevel(level.dimension());
            }
        }
    }

    public static void initLevelDataOnChangeWorld(Player player, Level srcLvl, Level dstLvl) {
        if (isServerSide(srcLvl)) {
            var shouldCreateNewLevelData = Services.FEATURE_MANAGER.shouldCreateNewLevelData();
            if (shouldCreateNewLevelData) {
                RegionManager.get().trackLevel(dstLvl.dimension());
            }
        }
    }

    public static void removeTrackingFor(Identifier rl){
        saveLevel(rl);
        trackedLevelData.removeTrackingFor(rl);
        levelRegionData.remove(rl);
        saveTrackedLevelList();

    }

    public static LevelData addTrackingFor(Identifier rl){
        if (trackedLevelData.doesTrack(rl) && levelRegionData.containsKey(rl)) {
            return levelRegionData.get(rl);
        }
        LevelData newLevelRegion = new LevelData(rl);
        trackedLevelData.addTrackingFor(rl);
        levelRegionData.put(rl, newLevelRegion);
        var dimensionalRegion = newLevelRegion.getDim();
        // add default flags from config
        Set<String> defaultDimFlags = Services.REGION_CONFIG.getDefaultDimFlags();
        defaultDimFlags.stream()
                .map(FlagRegister::byId)
                .forEach(flag -> dimensionalRegion.addFlag(new BooleanFlag(flag)));
        // set state from config
        dimensionalRegion.setIsActive(Services.REGION_CONFIG.shouldActivateNewDimRegion());
        // add as child of global
        RegionHierarchy.setParent(dimensionalRegion, RegionManager.get().getGlobalRegion());
        LOGGER.info(Component.translatableWithFallback("data.region.levels.init", "Initializing region data for level '%s'", rl.toString()).getString());
        saveLevel(rl);
        saveTrackedLevelList();
        return newLevelRegion;
    }

    public static Optional<LevelData> getLevelRegionData(Identifier rl) {
        if (!trackedLevelData.doesTrack(rl)) {
            return Optional.empty();
        }
        return Optional.of(levelRegionData.get(rl));
    }

    public static Optional<LevelData> getLevelRegionData(ResourceKey<Level> dim) {
        return getLevelRegionData(dim.identifier());
    }

    public static List<LevelData> getAllLevelRegionData() {
        return trackedLevelData.getLevels()
                .stream().sorted()
                .map(levelRegionData::get)
                .toList();
    }

    public static Collection<IMarkableRegion> getLocalsFor(ResourceKey<Level> dim) {
        var maybeRld = getLevelRegionData(dim.identifier());
        return maybeRld.isPresent() ? maybeRld.get().getLocalList() : new ArrayList<>();
    }

    public static void resetLevelData(Identifier rl) {
        levelRegionData.remove(rl);
    }

    public static void resetLevelData(ResourceKey<Level> dim) {
        resetLevelData(dim.identifier());
    }

}
