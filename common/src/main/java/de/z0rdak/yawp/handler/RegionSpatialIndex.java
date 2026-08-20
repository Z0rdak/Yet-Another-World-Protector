package de.z0rdak.yawp.handler;

import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.area.SphereArea;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import it.unimi.dsi.fastutil.longs.Long2ObjectOpenHashMap;
import it.unimi.dsi.fastutil.longs.LongOpenHashSet;
import it.unimi.dsi.fastutil.objects.Object2ObjectOpenHashMap;
import it.unimi.dsi.fastutil.objects.ObjectOpenHashSet;
import net.minecraft.core.BlockPos;
import net.minecraft.core.SectionPos;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.level.Level;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.UUID;

public final class RegionSpatialIndex {
    private final ResourceKey<Level> level;
    /**
     * Broad-phase spatial index:
     * section -> regions whose bounding area intersects the section.
     */
    private final Long2ObjectOpenHashMap<ObjectOpenHashSet<IMarkableRegion>> sectionToRegions;
    /**
     * Reverse lookup:
     * region id -> sections occupied by the region.
     */
    private final Object2ObjectOpenHashMap<UUID, LongOpenHashSet> regionToSections;

    public RegionSpatialIndex(ResourceKey<Level> level) {
        this.level = level;
        this.sectionToRegions = new Long2ObjectOpenHashMap<>();
        this.regionToSections = new  Object2ObjectOpenHashMap<>();
    }

    public ResourceKey<Level> getLevel() {
        return level;
    }

    public void addRegion(IMarkableRegion region) {
        BlockPos min;
        BlockPos max;

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
        LongOpenHashSet sections = new LongOpenHashSet();

        for (int x = minSecX; x <= maxSecX; x++) {
            for (int y = minSecY; y <= maxSecY; y++) {
                for (int z = minSecZ; z <= maxSecZ; z++) {
                    long key = SectionPos.asLong(x, y, z);
                    sectionToRegions
                            .computeIfAbsent(key, _ -> new ObjectOpenHashSet<>())
                            .add(region);
                    sections.add(key);
                }
            }
        }
        regionToSections.put(region.getUuid(), sections);
    }


    public void removeRegion(@NotNull IMarkableRegion region) {
        LongOpenHashSet sections = regionToSections.remove(region.getUuid());
        if (sections == null) {
            return;
        }
        for (long key : sections) {
            ObjectOpenHashSet<IMarkableRegion> regions = sectionToRegions.get(key);
            if (regions == null) {
                continue;
            }
            regions.remove(region);
            if (regions.isEmpty()) {
                sectionToRegions.remove(key);
            }
        }
    }

    /**
     * Gets the region with the highest priority among all involved regions at the given location. <br>
     * This considers the active state of the region as well. <br>
     *
     * @param position the position to check for involved regions
     * @return the region with the highest priority among all involved regions which contain the given location
     */
    @Nullable
    public IMarkableRegion getInvolvedRegion(@NotNull BlockPos position) {
        ObjectOpenHashSet<IMarkableRegion> regions = this.getRegionsAt(position);
        if (regions == null || regions.isEmpty()) {
            return null;
        }
        IMarkableRegion resolvedRegion = null;
        int highestPriority = Integer.MIN_VALUE;
        for (IMarkableRegion region : regions) {
            if (!region.isActive() || !region.getArea().contains(position))  {
                continue;
            }
            int priority = region.getPriority();
            if (resolvedRegion == null || priority > highestPriority) {
                resolvedRegion = region;
                highestPriority = priority;
            }
        }
        return resolvedRegion;
    }

    public void updateRegionArea(IMarkableRegion region) {
        removeRegion(region);
        addRegion(region);
    }

    @Nullable
    public ObjectOpenHashSet<IMarkableRegion> getRegionsAt(@NotNull BlockPos pos) {
        return sectionToRegions.get(sectionKey(pos));
    }

    private static long sectionKey(@NotNull BlockPos pos) {
        return SectionPos.asLong(
                pos.getX() >> 4,
                pos.getY() >> 4,
                pos.getZ() >> 4
        );
    }
}