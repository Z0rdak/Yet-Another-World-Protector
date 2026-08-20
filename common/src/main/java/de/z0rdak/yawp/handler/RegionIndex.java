package de.z0rdak.yawp.handler;

import de.z0rdak.yawp.api.core.RegionManager;
import de.z0rdak.yawp.api.events.region.RegionEvent;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.data.region.RegionDataManager;
import it.unimi.dsi.fastutil.objects.Object2ObjectOpenHashMap;
import it.unimi.dsi.fastutil.objects.ObjectOpenHashSet;
import net.minecraft.core.BlockPos;
import net.minecraft.resources.Identifier;
import net.minecraft.resources.ResourceKey;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jspecify.annotations.NonNull;

import java.util.Map;
import java.util.Set;

import static de.z0rdak.yawp.constants.Constants.MOD_ID;

public final class RegionIndex {

    @Contract(pure = true)
    private RegionIndex() {
        /* This utility class should not be instantiated */
    }

    private static final Logger LOGGER = LogManager.getLogger(MOD_ID.toUpperCase()+ "-Spatial-Region-Index");

    public static RegionSpatialIndex getIndexFor(@NonNull ResourceKey<Level> levelRl) {
        return levelPlayerCaches.computeIfAbsent(levelRl.identifier(), _ -> new RegionSpatialIndex(levelRl));
    }

    private static final Map<Identifier, RegionSpatialIndex> levelPlayerCaches = new Object2ObjectOpenHashMap<>();

    private static final Set<Identifier> excludedLevels = new ObjectOpenHashSet<>();

    public static void excludeLevel(@NonNull Identifier id) {
        excludedLevels.add(id);
    }

    public static boolean excludes(@NonNull ServerLevel level) {
        return excludedLevels.contains(level.dimension().identifier());
    }

    public static void initRegions(@NonNull ServerLevel level) {
        var maybeLevelRegionData = RegionDataManager.getLevelRegionData(level.dimension());
        if (maybeLevelRegionData.isEmpty()) {
            return;
        }
        var levelRegionData = maybeLevelRegionData.get();
        RegionSpatialIndex cache = getIndexFor(levelRegionData.getDimKey());
        levelRegionData.getLocals().forEach((_, region) -> cache.addRegion(region));
    }

    public static boolean onCreateRegion(RegionEvent.@NonNull Create create) {
        RegionSpatialIndex cache = getIndexFor(create.getRegion().getDim());
        cache.addRegion(create.getRegion());
        LOGGER.info("Added region {} to region cache.", create.getRegion().getName());
        return true;
    }

    public static boolean onRemoveRegion(RegionEvent.@NonNull Remove remove) {
        RegionSpatialIndex cache = getIndexFor(remove.getRegion().getDim());
        cache.removeRegion(remove.getRegion());
        LOGGER.info("Removed region {} from region cache.", remove.getRegion().getName());
        return true;
    }

    public static boolean onUpdateRegionArea(RegionEvent.@NonNull UpdateArea updateArea) {
        RegionSpatialIndex cache = getIndexFor(updateArea.getRegion().getDim());
        cache.updateRegionArea(updateArea.getRegion());
        LOGGER.info("Updated region {} in region cache.", updateArea.getRegion().getName());
        return true;
    }

    /**
     * Determines the region responsible for the given position and dimension.  
     * The responsible region is the one with the highest priority among all regions that cover the specified location.
     * <p>
     * If no specific region is found, this method falls back to the dimensional region.
     * If the dimensional region is inactive, it further falls back to the global region if it is active.
     * </p>
     *
     * @param pos the position for which to find the responsible region, must not be {@code null}
     * @param dim the dimension in which to search for the responsible region, must not be {@code null}
     * @return the highest-priority active region covering the given position and dimension,  
     *         or {@code null} if no active region is found
     */
    @Nullable
    public static IProtectedRegion findResponsibleRegion(@NotNull BlockPos pos, @NotNull ResourceKey<Level> dim) {
        // since levels are no longer automatically tracked,
        // it needs to be considered when resolving responsible regions
        // level not tracked -> global
        if (!RegionDataManager.getTrackedLevelData().doesTrack(dim.identifier())){
            var globalRegion = RegionManager.get().getGlobalRegion();
            return globalRegion.isActive() ? globalRegion : null;
        }
        var localRegion = getIndexFor(dim).getInvolvedRegion(pos);
        if (localRegion == null) {
            var maybeLrd = RegionDataManager.getLevelRegionData(dim);
            if (maybeLrd.isEmpty()) {
                return null;
            }
            var dimRegion = maybeLrd.get().getDim();
            if (dimRegion.isActive()) {
                return dimRegion;
            } else {
                var globalRegion = RegionManager.get().getGlobalRegion();
                return globalRegion.isActive() ? globalRegion : null;
            }
        }
        return localRegion;
    }
}
