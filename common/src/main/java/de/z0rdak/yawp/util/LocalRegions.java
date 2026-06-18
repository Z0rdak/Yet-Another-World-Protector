package de.z0rdak.yawp.util;

import de.z0rdak.yawp.api.permission.Permissions;
import de.z0rdak.yawp.core.area.IMarkableArea;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.data.region.RegionDataManager;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

public final class LocalRegions {

    private LocalRegions() {
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
        Collection<IMarkableRegion> regionsInDim = RegionDataManager.getLocalsFor(region.getDim()).stream()
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
        Collection<IMarkableRegion> regionsInDim = RegionDataManager.getLocalsFor(dim);
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
