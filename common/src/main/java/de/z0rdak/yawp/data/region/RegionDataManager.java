package de.z0rdak.yawp.data.region;

import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.region.GlobalRegion;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.storage.DimensionDataStorage;
import net.minecraft.world.level.storage.LevelResource;
import net.minecraft.world.scores.Team;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.handler.HandlerUtil.isServerSide;

public class RegionDataManager {

    public static final Logger LOGGER = LogManager.getLogger(Constants.MOD_ID.toUpperCase(Locale.ROOT) + "-DataManager");
    private static MinecraftServer serverInstance;
    private static LevelListData savedLevelData = new LevelListData();
    private static GlobalRegionData globalRegionData = new GlobalRegionData();
    private static Map<ResourceLocation, LevelRegionData> dimRegionStorage = new  HashMap<>();

    public static LevelListData getSavedLevelData() {
        return savedLevelData;
    }

    public static Set<ResourceLocation> getLevels() {
        return new HashSet<>(savedLevelData.getLevels());
    }

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

    public static void save(MinecraftServer server, boolean force) {
        if (force) {
            saveDimList(server);
            saveGlobalData(server);
            saveTrackedLevels(server);
        } else {
            LOGGER.debug(Component.translatableWithFallback("data.nbt.dimensions.save", "Save for RegionDataManager called. Attempting to save region data...").getString());
            savedLevelData.setDirty();
            globalRegionData.setDirty();
            dimRegionStorage.forEach((key, value) -> value.setDirty());
        }
    }

    public static LevelListData getSavedDims() {
        if (savedLevelData == null) {
            if (serverInstance != null) {
                ServerLevel overworld = serverInstance.overworld();
                if (!overworld.isClientSide) {
                    DimensionDataStorage storage = overworld.getDataStorage();
                    savedLevelData = storage.computeIfAbsent(LevelListData.TYPE);

                }
            }
        }
        return savedLevelData;
    }

    public static void onServerStat(MinecraftServer server) {
        LOGGER.info("Initializing RegionDataManager...");
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
        LOGGER.info("Cyclic or forced save. Saving region data for all levels.");
        save(server, force);
    }

    private static void saveDimList(MinecraftServer server) {
        DimensionDataStorage dataStorage = server.overworld().getDataStorage();
        dataStorage.set(LevelListData.TYPE, savedLevelData);
    }

    private static void saveGlobalData(MinecraftServer server) {
        DimensionDataStorage dataStorage = server.overworld().getDataStorage();
        dataStorage.set(GlobalRegionData.TYPE, globalRegionData);
    }

    private static void saveLevelData(MinecraftServer server, Level level) {
        DimensionDataStorage storage = server.overworld().getDataStorage();
        ResourceLocation levelRl = level.dimension().location();
        LevelRegionData levelRegionData = new LevelRegionData(levelRl);
        LOGGER.info("Saving region data for level '{}'", levelRl);
        levelRegionData.setDirty();
        storage.set(LevelRegionData.buildSavedDataType(levelRl), levelRegionData);
    }

    private static LevelRegionData loadLevelData(MinecraftServer server, Level level) {
        DimensionDataStorage storage = server.overworld().getDataStorage();
        ResourceLocation dimLoc = level.dimension().location();
        return storage.get(LevelRegionData.buildSavedDataType(dimLoc));
    }

    public static void saveOnStop(MinecraftServer server) {
        LOGGER.info("Stopping server. Saving region data for all levels.");
        saveDimList(server);
        saveGlobalData(server);
        saveTrackedLevels(server);
    }

    public static void saveOnUnload(MinecraftServer server, ServerLevel level) {
        if (savedLevelData.hasDimEntry(level.dimension().location())) {
            LOGGER.info("Unloading level '{}'. Saving region data.", level.dimension().location());
            saveLevelData(server, level);
        }
    }

    public static void worldLoad(MinecraftServer server, ServerLevel level) {
        try {
            if (serverInstance == null)
                serverInstance = server;
            if (isServerSide(level)) {
                if (level.dimension() == Level.OVERWORLD) {
                    DimensionDataStorage dataStorage = server.overworld().getDataStorage();
                    savedLevelData = dataStorage.get(LevelListData.TYPE);
                    if (savedLevelData == null) {
                        savedLevelData = new LevelListData();
                        saveDimList(server);
                    }
                    globalRegionData = dataStorage.get(GlobalRegionData.TYPE);
                    if (globalRegionData == null) {
                        globalRegionData = new GlobalRegionData();
                        saveGlobalData(server);
                    }
                }

                ResourceLocation levelRl = level.dimension().location();
                if (savedLevelData.hasDimEntry(levelRl)) {
                    LevelRegionData levelRegionData = loadLevelData(server, level);
                    if (levelRegionData == null) {
                        levelRegionData = new LevelRegionData(levelRl);
                        LOGGER.info("Initializing region data for '{}'", levelRl);
                        saveLevelData(server, level);
                    } else {
                        LOGGER.info("Loaded region data for '{}'", levelRl);
                    }
                    dimRegionStorage.put(levelRl, levelRegionData);
                }
            }
        } catch (NullPointerException npe) {
            LOGGER.error(Component.translatableWithFallback("data.nbt.dimensions.load.failure", "Loading regions failed!").getString());
        }
    }

    private static void checkYawpDir(MinecraftServer server) {
        Path worldRootPath = server.getWorldPath(LevelResource.ROOT).normalize();
        Path dataDirPath = worldRootPath.resolve("data/" + Constants.MOD_ID);
        if (Files.notExists(dataDirPath)) {
            try {
                Files.createDirectories(dataDirPath);
                LOGGER.info("Subdirectory created: {}", dataDirPath);
            } catch (IOException e) {
                LOGGER.error("Failed to create subdirectory: {}", dataDirPath, e);
                throw new RuntimeException(e);
            }
        }
    }


    public static void initDimDataOnLogin(Entity entity, ServerLevel level) {
        if (isServerSide(level) && entity instanceof Player) {
            initDimData(level.dimension().location());
        }
    }

    public static void initDimDataOnChangeWorld(ServerPlayer player, ServerLevel srcLvl, ServerLevel dstLvl) {
        if (isServerSide(srcLvl)) {
            initDimData(dstLvl.dimension().location());
        }
    }

    private static void initDimData(ResourceLocation rl){
        if (!dimRegionStorage.containsKey(rl)) {
            LevelRegionData dimData = new LevelRegionData(rl);
            dimRegionStorage.put(rl, dimData);
            savedLevelData.addDimEntry(rl);
            LOGGER.info("Initializing region data for: '{}'", rl);
            save(serverInstance, true);
        }
    }

    public static Optional<LevelRegionData> getLevelRegionData(ResourceLocation rl) {
        if (!dimRegionStorage.containsKey(rl)) {
            return Optional.empty();
        }
        return Optional.of(dimRegionStorage.get(rl));
    }

    public static Optional<LevelRegionData> getLevelRegionData(ResourceKey<Level> dim) {
        return getLevelRegionData(dim.location());
    }

    public static LevelRegionData getOrCreate(ResourceKey<Level> dim) {
        if (!dimRegionStorage.containsKey(dim.location())) {
            initDimData(dim.location());
        }
        return dimRegionStorage.get(dim.location());
    }


    public static Set<ResourceLocation> getDimKeys() {
        return new HashSet<>(dimRegionStorage.keySet());
    }

    public static void resetLevelData(ResourceLocation rl) {
        dimRegionStorage.remove(rl);
    }

    public static void resetLevelData(ResourceKey<Level> dim) {
        resetLevelData(dim.location());
    }

}
