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
import net.minecraft.resources.Identifier;
import net.minecraft.resources.ResourceKey;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.storage.SavedDataStorage;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.*;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.handler.HandlerUtil.isServerSide;

public class RegionDataManager {

    public static final Logger LOGGER = LogManager.getLogger(Constants.MOD_ID.toUpperCase() + "-RegionDataManager");
    private static MinecraftServer serverInstance;
    /**
     * Implicit assumption: If savedLevelData contains a level, its also present in dimRegionStorage
     */
    private static LevelListData trackedLevelData;
    private static GlobalRegionData globalRegionData = new GlobalRegionData();
    private static final Map<Identifier, LevelRegionData> levelRegionData = new  HashMap<>();

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
        LOGGER.info(Component.translatableWithFallback("data.region.init","Initializing RegionDataManager...").getString());
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
        LOGGER.info(Component.translatableWithFallback("data.region.level.save.unload", "Unloading level '%s'. Saving region data", level.dimension().identifier().toString()).getString());
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
            LevelRegionData levelRegionData = RegionDataManager.levelRegionData.get(levelRl);
            LOGGER.debug(Component.translatableWithFallback("data.region.level.save", "Saving region data for level '%s'", levelRl.toString()).getString());
            storage.set(LevelRegionData.buildSavedDataType(levelRl), levelRegionData);
            levelRegionData.setDirty();
        }
    }

    private static LevelRegionData loadLevelData(MinecraftServer server, Level level) {
        SavedDataStorage storage = server.overworld().getDataStorage();
        Identifier dimLoc = level.dimension().identifier();
        return storage.get(LevelRegionData.buildSavedDataType(dimLoc));
    }

    public static void saveOnStop(MinecraftServer server) {
        if (serverInstance == null) serverInstance = server;
        LOGGER.info(Component.translatableWithFallback("data.region.levels.save.stopped", "Stopping server. Saving region data for all levels").getString());
        save();
    }

    public static void saveOnUnload(MinecraftServer server, ServerLevel level) {
        if (trackedLevelData.doesTrack(level.dimension().identifier())) {
            LOGGER.info(Component.translatableWithFallback("data.region.level.save.unload", "Unloading level '%s'. Saving region data", level.dimension().identifier().toString()).getString());
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
                LOGGER.info(Component.translatableWithFallback("data.region.levels.load.success", "Found region data for %s dimension(s)", trackedLevelData.getLevels().size()).getString());
            }
            globalRegionData = dataStorage.get(GlobalRegionData.TYPE);
            if (globalRegionData == null) {
                LOGGER.info(Component.translatableWithFallback("data.region.global.missing", "Missing global region data (ignore on first startup). Initializing...").getString());
                globalRegionData = new GlobalRegionData();
                saveGlobalData();
            }
        } catch (NullPointerException npe) {
            LOGGER.error(Component.translatableWithFallback("data.region.level.local.load.failed", "Loading level region list failed!").getString(), npe);
        }
    }

    public static void worldLoad(MinecraftServer server, ServerLevel level) {
        try {
            if (serverInstance == null)
                serverInstance = server;
            Identifier levelRl = level.dimension().identifier();
            // init level data
            if (trackedLevelData.doesTrack(levelRl)) {
                LevelRegionData newLevelRegionData = loadLevelData(server, level);
                if (newLevelRegionData == null) {
                    newLevelRegionData = new LevelRegionData(levelRl);
                    LOGGER.info(Component.translatableWithFallback("data.region.level.local.missing", "Initializing region data for '%s'", levelRl.toString()).getString());
                    saveLevelData(level);
                } else {
                    LOGGER.info(Component.translatableWithFallback("data.region.level.local.load.success", "Loaded %s region(s) for '%s'", newLevelRegionData.regionCount(), levelRl.toString()).getString());
                    levelRegionData.put(levelRl, newLevelRegionData);
                    trackedLevelData.addTrackingFor(levelRl);
                }
                // restoring region hierarchy
                LOGGER.info(Component.translatableWithFallback("data.region.level.local.load.restore", "Restoring region hierarchy for '%s'.", levelRl.toString()).getString());

                // restore dim <-> global hierarchy
                DimensionalRegion dimensionalRegion = newLevelRegionData.getDim();
                RegionManager.get().getGlobalRegion().addChild(dimensionalRegion);
                restoreHierarchy(newLevelRegionData, dimensionalRegion);

                // restore dim <-> local <-> local hierarchy
                newLevelRegionData.getLocals().forEach((regionName, region) -> {
                    restoreHierarchy(RegionDataManager.levelRegionData.get(levelRl), region);
                });
            }
            Services.YAWP_EVENT_DISPATCHER.post(level);
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
        trackedLevelData.removeTrackingFor(rl);
        levelRegionData.remove(rl);
        saveLevel(rl);
        saveTrackedLevelList();
    }

    public static LevelRegionData addTrackingFor(Identifier rl){
        if (trackedLevelData.doesTrack(rl) && levelRegionData.containsKey(rl)) {
            return levelRegionData.get(rl);
        }
        LevelRegionData newLevelRegion = new LevelRegionData(rl);
        trackedLevelData.addTrackingFor(rl);
        levelRegionData.put(rl, newLevelRegion);
        DimensionalRegion dimensionalRegion = newLevelRegion.getDim();
        // add default flags from config
        Set<String> defaultDimFlags = Services.REGION_CONFIG.getDefaultDimFlags();
        defaultDimFlags.stream()
                .map(RegionFlag::fromId)
                .forEach(flag -> dimensionalRegion.addFlag(new BooleanFlag(flag)));
        // set state from config
        dimensionalRegion.setIsActive(Services.REGION_CONFIG.shouldActivateNewDimRegion());
        // add as child of global
        RegionManager.get().getGlobalRegion().addChild(dimensionalRegion);
        LOGGER.info(Component.translatableWithFallback("data.region.level.init", "Initializing region data for level '%s'", rl.toString()).getString());
        saveLevel(rl);
        saveTrackedLevelList();
        return newLevelRegion;
    }

    public static Optional<LevelRegionData> getLevelRegionData(Identifier rl) {
        if (!trackedLevelData.doesTrack(rl)) {
            return Optional.empty();
        }
        return Optional.of(levelRegionData.get(rl));
    }

    public static Optional<LevelRegionData> getLevelRegionData(ResourceKey<Level> dim) {
        return getLevelRegionData(dim.identifier());
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
