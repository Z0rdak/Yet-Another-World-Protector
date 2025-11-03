package de.z0rdak.yawp.handler;

import de.z0rdak.yawp.api.events.region.RegionEvent;
import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.area.SphereArea;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.data.region.LevelRegionData;
import de.z0rdak.yawp.data.region.RegionDataManager;
import it.unimi.dsi.fastutil.objects.Object2ObjectOpenHashMap;
import it.unimi.dsi.fastutil.objects.ObjectOpenHashSet;
import net.minecraft.core.BlockPos;
import net.minecraft.core.SectionPos;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import static de.z0rdak.yawp.constants.Constants.MOD_ID;

public final class RegionSpatialCache {

    private static final Logger LOGGER = LogManager.getLogger(MOD_ID.toUpperCase()+ "-Spatial-Region-Index");

    public static RegionSpatialCache get(ResourceKey<Level> levelRl) {
        return levelPlayerCaches.computeIfAbsent(levelRl.location(), key -> new RegionSpatialCache(levelRl));
    }

    private static final Map<ResourceLocation, RegionSpatialCache> levelPlayerCaches = new Object2ObjectOpenHashMap<>();

    private static final Set<ResourceLocation> excludedLevels = new ObjectOpenHashSet<>();

    public static void excludeLevel(ResourceLocation id) {
        excludedLevels.add(id);
    }

    public static boolean excludes(ServerLevel level) {
        return excludedLevels.contains(level.dimension().location());
    }

    public static void initRegions(ServerLevel level) {
        LevelRegionData levelRegionData = RegionDataManager.getOrCreate(level);
        RegionSpatialCache cache = RegionSpatialCache.get(levelRegionData.getDimKey());
        levelRegionData.getLocals().forEach((k, region) -> {
            cache.addRegion(region);
        });
    }

    public static boolean onCreateRegion(RegionEvent.Create create) {
        RegionSpatialCache cache = RegionSpatialCache.get(create.getRegion().getDim());
        cache.addRegion(create.getRegion());
        LOGGER.info("Added region {} to PlayerPosTracker cache.", create.getRegion().getName());
        return true;
    }

    public static boolean onRemoveRegion(RegionEvent.Remove remove) {
        RegionSpatialCache cache = RegionSpatialCache.get(remove.getRegion().getDim());
        cache.removeRegion(remove.getRegion());
        LOGGER.info("Removed region {} from PlayerPosTracker cache.", remove.getRegion().getName());
        return true;
    }

    public static boolean onUpdateRegion(RegionEvent.UpdateArea updateArea) {
        RegionSpatialCache cache = RegionSpatialCache.get(updateArea.getRegion().getDim());
        cache.updateRegion(updateArea.getRegion());
        LOGGER.info("Updated region {} in PlayerPosTracker cache.", updateArea.getRegion().getName());
        return true;
    }

    private final ResourceKey<Level> level;
    private final Map<Long, ObjectOpenHashSet<IMarkableRegion>> sectionToRegions;
    private final Map<String, ObjectOpenHashSet<Long>> regionToSections;

    public RegionSpatialCache(ResourceKey<Level> level) {
        this.level = level;
        this.sectionToRegions = new Object2ObjectOpenHashMap<>();
        regionToSections = new Object2ObjectOpenHashMap<>();
    }

    public ResourceKey<Level> getLevel() {
        return level;
    }

    public void addRegion(IMarkableRegion region) {
        BlockPos min, max;

        switch (region.getAreaType()) {
            case CUBOID -> {
                CuboidArea cuboid = (CuboidArea) region.getArea();
                min = cuboid.getAreaP1();
                max = cuboid.getAreaP2();
            }
            case SPHERE -> {
                SphereArea sphere = (SphereArea) region.getArea();
                min = sphere.getCenterPos().offset(-sphere.getRadius(), -sphere.getRadius(), -sphere.getRadius());
                max = sphere.getCenterPos().offset(sphere.getRadius(), sphere.getRadius(), sphere.getRadius());
            }
            default -> throw new UnsupportedOperationException("Unsupported area type " + region.getAreaType());
        }

        int minSecX = SectionPos.blockToSectionCoord(min.getX());
        int minSecY = SectionPos.blockToSectionCoord(min.getY());
        int minSecZ = SectionPos.blockToSectionCoord(min.getZ());
        int maxSecX = SectionPos.blockToSectionCoord(max.getX());
        int maxSecY = SectionPos.blockToSectionCoord(max.getY());
        int maxSecZ = SectionPos.blockToSectionCoord(max.getZ());

        for (int x = minSecX; x <= maxSecX; x++) {
            for (int y = minSecY; y <= maxSecY; y++) {
                for (int z = minSecZ; z <= maxSecZ; z++) {
                    long key = SectionPos.asLong(x, y, z);
                    sectionToRegions.computeIfAbsent(key, k -> new ObjectOpenHashSet<>()).add(region);
                }
            }
        }

        // Reverse mapping for remove and update lookup
        ObjectOpenHashSet<Long> keys = new ObjectOpenHashSet<>();
        for (int x = minSecX; x <= maxSecX; x++) {
            for (int y = minSecY; y <= maxSecY; y++) {
                for (int z = minSecZ; z <= maxSecZ; z++) {
                    long key = SectionPos.asLong(x, y, z);
                    sectionToRegions.computeIfAbsent(key, k -> new ObjectOpenHashSet<>()).add(region);
                    keys.add(key);
                }
            }
        }
        regionToSections.put(region.getName(), keys);
    }

    public void removeRegion(IMarkableRegion region) {
        ObjectOpenHashSet<Long> sections = regionToSections.remove(region.getName());
        if (sections == null) return;
        for (long key : sections) {
            ObjectOpenHashSet<IMarkableRegion> set = sectionToRegions.get(key);
            if (set != null) {
                set.remove(region);
                if (set.isEmpty()) sectionToRegions.remove(key);
            }
        }
    }

    public void updateRegion(IMarkableRegion region) {
        removeRegion(region);
        addRegion(region);
    }


    public ObjectOpenHashSet<IMarkableRegion> getRegionsAtSection(SectionPos pos) {
        return sectionToRegions.getOrDefault(pos.asLong(), new ObjectOpenHashSet<>());
    }
}