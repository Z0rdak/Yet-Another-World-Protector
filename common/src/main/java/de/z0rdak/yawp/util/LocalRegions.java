package de.z0rdak.yawp.util;

import de.z0rdak.yawp.api.permission.Permissions;
import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.area.IMarkableArea;
import de.z0rdak.yawp.core.area.SphereArea;
import de.z0rdak.yawp.core.region.CuboidRegion;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.SphereRegion;
import de.z0rdak.yawp.core.stick.MarkerStick;
import de.z0rdak.yawp.data.region.RegionDataManager;
import net.minecraft.core.BlockPos;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import org.apache.commons.lang3.NotImplementedException;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

public final class LocalRegions {

    private LocalRegions() {
    }

    public static IMarkableRegion regionFrom(Player player, MarkerStick marker, String regionName) {
        return regionFrom(player, marker, regionName, marker.getDimension());
    }

    public static IMarkableArea areaFrom(MarkerStick marker) {
        List<BlockPos> blocks = marker.getMarkedBlocks();
        switch (marker.getAreaType()) {
            case CUBOID:
                return new CuboidArea(blocks.get(0), blocks.get(1));
            case SPHERE:
                return new SphereArea(blocks.get(0), blocks.get(1));
            default:
                throw new NotImplementedException("Area type not implemented yet");
        }
    }

    public static IMarkableRegion regionFrom(Player player, MarkerStick marker, String regionName, ResourceKey<Level> dim) {
        switch (marker.getAreaType()) {
            case CUBOID:
                return cuboidRegionFrom(marker, regionName, player, dim);
            case SPHERE:
                return sphericalRegionFrom(marker, regionName, player, dim);
            default:
                throw new NotImplementedException("Area type not implemented yet");
        }
    }

    private static SphereRegion sphericalRegionFrom(MarkerStick marker, String regionName, Player player, ResourceKey<Level> dim) {
        List<BlockPos> blocks = marker.getMarkedBlocks();
        SphereArea sphereArea = new SphereArea(blocks.get(0), blocks.get(1));
        if (marker.getTeleportPos() != null) {
            return new SphereRegion(regionName, sphereArea, marker.getTeleportPos(), player, dim);
        }
        return new SphereRegion(regionName, sphereArea, player, dim);
    }

    private static CuboidRegion cuboidRegionFrom(MarkerStick marker, String regionName, Player player, ResourceKey<Level> dim) {
        List<BlockPos> blocks = marker.getMarkedBlocks();
        CuboidArea cuboidArea = new CuboidArea(blocks.get(0), blocks.get(1));
        if (marker.getTeleportPos() != null) {
            return new CuboidRegion(regionName, cuboidArea, marker.getTeleportPos(), player, dim);
        }
        return new CuboidRegion(regionName, cuboidArea, player, dim);
    }

    public static IMarkableRegion regionFromArea(IMarkableArea area, BlockPos tpTarget, String regionName, ResourceKey<Level> dim) {
        switch (area.getAreaType()) {
            case CUBOID:
                return new CuboidRegion(regionName, (CuboidArea) area, tpTarget, null, dim);
            case SPHERE:
                return new SphereRegion(regionName, (SphereArea) area, tpTarget, null, dim);
            default:
                throw new NotImplementedException("Area type not implemented yet");
        }
    }

    public static boolean hasAnyRegionWithSamePriority(IMarkableRegion region, int priority) {
        return hasAnyRegionWithSamePriority(getIntersectingRegionsFor(region), priority);
    }

    public static int ensureHigherRegionPriorityFor(IMarkableRegion markableRegion, int defaultPriority) {
        List<IMarkableRegion> intersectingRegions = getIntersectingRegionsFor(markableRegion);
        boolean hasRegionWithSamePriority = hasAnyRegionWithSamePriority(intersectingRegions, defaultPriority);
        if (hasRegionWithSamePriority) {
            int maxPriority = intersectingRegions.stream()
                    .mapToInt(IMarkableRegion::getPriority)
                    .max().getAsInt();
            markableRegion.setPriority(maxPriority + 1);
        } else {
            markableRegion.setPriority(defaultPriority);
        }
        return markableRegion.getPriority();
    }

    // TODO: FIXME
    // TODO: recursive check for ensuring region priorities
    @Deprecated
    public static void rectifyRegionPriorities(IMarkableRegion parent, int defaultPriority) {
        List<IMarkableRegion> children = getIntersectingRegionsFor(parent);
        if (children.isEmpty()) {
            return;
        }
        for (IMarkableRegion child : children) {

            rectifyRegionPriorities(child, parent.getPriority());
        }
        List<IMarkableRegion> intersectingRegions = getIntersectingRegionsFor(parent);
        boolean hasRegionWithSamePriority = intersectingRegions.stream().anyMatch(r -> r.getPriority() == parent.getPriority());
        if (hasRegionWithSamePriority) {
            int minPriority = intersectingRegions.stream().mapToInt(IMarkableRegion::getPriority).min().getAsInt();
            parent.setPriority(minPriority - 1);
        } else {
            parent.setPriority(defaultPriority);
        }
    }

    public static int ensureLowerRegionPriorityFor(IMarkableRegion markableRegion, int defaultPriority) {
        List<IMarkableRegion> intersectingRegions = getIntersectingRegionsFor(markableRegion);
        boolean hasRegionWithSamePriority = intersectingRegions.stream().anyMatch(r -> r.getPriority() == markableRegion.getPriority());
        if (hasRegionWithSamePriority) {
            int minPriority = intersectingRegions.stream().mapToInt(IMarkableRegion::getPriority).min().getAsInt();
            markableRegion.setPriority(minPriority - 1);
        } else {
            markableRegion.setPriority(defaultPriority);
        }
        return markableRegion.getPriority();
    }

    public static RegionOverlappingInfo getOverlappingRegions(IMarkableRegion region) {
        Collection<IMarkableRegion> regionsInDim = RegionDataManager.get().getRegionsFor(region.getDim()).stream()
                .filter(r -> !r.equals(region))
                .collect(Collectors.toList());
        List<IMarkableRegion> intersectingRegions = regionsInDim.stream()
                .filter(r -> r.getArea().intersects(region.getArea()))
                .collect(Collectors.toList());
        List<IMarkableRegion> containingRegions = regionsInDim.stream()
                .filter(r -> r.getArea().containsOther(region.getArea()))
                .collect(Collectors.toList());
        return new RegionOverlappingInfo(region, intersectingRegions, containingRegions);
    }

    public static RegionOverlappingInfo getOverlappingRegions(IMarkableArea area, ResourceKey<Level> dim) {
        Collection<IMarkableRegion> regionsInDim = RegionDataManager.get().getRegionsFor(dim);
        List<IMarkableRegion> intersectingRegions = regionsInDim.stream()
                .filter(r -> r.getArea().intersects(area))
                .collect(Collectors.toList());
        List<IMarkableRegion> containingRegions = regionsInDim.stream()
                .filter(r -> r.getArea().containsOther(area))
                .collect(Collectors.toList());
        return new RegionOverlappingInfo(null, intersectingRegions, containingRegions);
    }

    public static RegionOverlappingInfo getOverlappingOwned(IMarkableRegion region, Player player) {
        RegionOverlappingInfo overlappingRegions = getOverlappingRegions(region);
        List<IMarkableRegion> intersecting = overlappingRegions.intersectingRegions.stream()
                .filter(r -> r.isInGroup(player, Permissions.OWNER))
                .collect(Collectors.toList());
        List<IMarkableRegion> contained = overlappingRegions.containingRegions.stream()
                .filter(r -> r.isInGroup(player, Permissions.OWNER))
                .collect(Collectors.toList());
        return new RegionOverlappingInfo(region, intersecting, contained);
    }

    public static RegionOverlappingInfo getOverlappingWithPermission(IMarkableArea area, Player player) {
        RegionOverlappingInfo overlappingRegions = getOverlappingRegions(area, player.level().dimension());
        return getOverlappingWithPermission(null, player, overlappingRegions);
    }

    public static RegionOverlappingInfo getOverlappingWithPermission(IMarkableRegion region, Player player) {
        RegionOverlappingInfo overlappingRegions = getOverlappingRegions(region);
        return getOverlappingWithPermission(region, player, overlappingRegions);
    }

    private static @NotNull RegionOverlappingInfo getOverlappingWithPermission(IMarkableRegion region, Player player, RegionOverlappingInfo overlappingRegions) {
        List<IMarkableRegion> intersecting = overlappingRegions.intersectingRegions.stream()
                .filter(r -> Permissions.get().hasAnyPermission(r, player, Permissions.getGroups(r, player)))
                .collect(Collectors.toList());
        List<IMarkableRegion> contained = overlappingRegions.containingRegions.stream()
                .filter(r -> Permissions.get().hasAnyPermission(r, player, Permissions.getGroups(r, player)))
                .collect(Collectors.toList());
        return new RegionOverlappingInfo(region, intersecting, contained);
    }

    public static List<IMarkableRegion> getIntersectingRegionsFor(IMarkableRegion markableRegion) {
        return markableRegion.getParent().getChildren().values()
                .stream()
                .map(r -> (IMarkableRegion) r)
                .filter(r -> !r.equals(markableRegion)) // filter input region from the result
                .filter(r -> (markableRegion.getArea()).intersects((r).getArea()))
                .collect(Collectors.toList());
    }

    private static boolean hasAnyRegionWithSamePriority(List<IMarkableRegion> region, int priority) {
        return region.stream().anyMatch(r -> r.getPriority() == priority);
    }

    private static List<IMarkableRegion> getIntersectingWithSamePriority(IMarkableRegion markableRegion) {
        return markableRegion.getParent().getChildren().values()
                .stream()
                .filter(r -> !r.equals(markableRegion)) // filter input region from the result
                .map(r -> (IMarkableRegion) r)
                .filter(region -> (markableRegion.getArea()).intersects((region).getArea()))
                .filter(r -> r.getPriority() == markableRegion.getPriority())
                .collect(Collectors.toList());
    }

    public static class RegionOverlappingInfo {
        @Nullable
        public final IMarkableRegion region;
        public final List<IMarkableRegion> intersectingRegions;
        public final List<IMarkableRegion> containingRegions;

        public RegionOverlappingInfo(@Nullable IMarkableRegion region, List<IMarkableRegion> intersectingRegions, List<IMarkableRegion> containingRegions) {
            this.region = region;
            this.intersectingRegions = intersectingRegions;
            this.containingRegions = containingRegions;
        }

        public boolean hasOverlapping() {
            return !intersectingRegions.isEmpty() || !containingRegions.isEmpty();
        }

        public boolean hasIntersecting() {
            return !intersectingRegions.isEmpty();
        }

        public boolean hasContaining() {
            return !containingRegions.isEmpty();
        }
    }

}
