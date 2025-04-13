package de.z0rdak.yawp.data.region;

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
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.storage.DimensionDataStorage;
import net.minecraft.world.level.storage.LevelResource;
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
        save(force);
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
                // if overworld, init level independent data
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

                // init level data
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

                // restoring region hierarchy
                LevelRegionData levelRegionData = dimRegionStorage.get(levelRl);
                int regionCount = levelRegionData.regionCount();
                LOGGER.info("Restoring region hierarchy for '{}'. Found {} local regions.", levelRl, regionCount);

                // restore dim <-> global hierarchy
                DimensionalRegion dimensionalRegion = levelRegionData.getDim();
                RegionDataManager.getGlobalRegion().addChild(dimensionalRegion);
                restoreHierarchy(levelRegionData, dimensionalRegion);
                
                // restore dim <-> local <-> local hierarchy
                levelRegionData.getLocals().forEach((regionName, region) -> {
                    restoreHierarchy(levelRegionData, region);
                });
            }
        } catch (NullPointerException npe) {
            LOGGER.error(Component.translatableWithFallback("data.nbt.dimensions.load.failure", "Loading regions failed!").getString());
        }
    }
    
    private static void restoreHierarchy(LevelRegionData levelRegionData, IProtectedRegion region) {
        region.getChildrenNames().forEach(childName -> {
            if (!levelRegionData.hasLocal(childName)) {
                LOGGER.warn("No region with name '{}' found in save data of '{}'! Your region data is most likely corrupt.", childName, levelRegionData.getId());
            } else {
                IMarkableRegion child = levelRegionData.getLocal(childName);
                if (child != null) {
                    region.addChild(child);
                }
            }
        });
    }

    public static void onStarted(MinecraftServer server) {
        // LOG about regions?
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


    public static void initLevelDataOnLogin(Entity entity, ServerLevel level) {
        if (isServerSide(level) && entity instanceof Player) {
            initLevelData(level.dimension().location());
        }
    }

    public static void initLevelDataOnChangeWorld(ServerPlayer player, ServerLevel srcLvl, ServerLevel dstLvl) {
        if (isServerSide(srcLvl)) {
            initLevelData(dstLvl.dimension().location());
        }
    }

    private static LevelRegionData initLevelData(ResourceLocation rl){
        if (!dimRegionStorage.containsKey(rl)) {
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
            RegionDataManager.getGlobalRegion().addChild(dimensionalRegion);

            dimRegionStorage.put(rl, levelRegionData);
            savedLevelData.addDimEntry(rl);
            LOGGER.info("Initializing region data for: '{}'", rl);
            save(true);
            return levelRegionData;
        }
        return dimRegionStorage.get(rl);
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

    public static LevelRegionData getOrCreate(ResourceLocation rl) {
        if (!dimRegionStorage.containsKey(rl)) {
            return initLevelData(rl);
        }
        return dimRegionStorage.get(rl);
    }

    public static Collection<IMarkableRegion> getLocalsFor(ResourceKey<Level> dim) {
        return getOrCreate(dim.location()).getLocalList();
    }

    public static LevelRegionData getOrCreate(ResourceKey<Level> dim) {
       return getOrCreate(dim.location());
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
