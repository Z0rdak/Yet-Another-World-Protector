package de.z0rdak.yawp.util;

import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.AbstractMarkableRegion;
import de.z0rdak.yawp.core.region.CuboidRegion;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.SphereRegion;
import de.z0rdak.yawp.core.stick.MarkerStick;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.registry.RegistryKey;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

public final class LocalRegions {

    private LocalRegions() {
    }

    public static AbstractMarkableRegion regionFrom(PlayerEntity player, MarkerStick marker, String regionName) {
        return regionFrom(player, marker, regionName, marker.getDimension());
    }

    public static AbstractMarkableRegion regionFrom(PlayerEntity player, MarkerStick marker, String regionName, RegistryKey<World> dim) {
        switch (marker.getAreaType()) {
            case CUBOID:
                return cuboidRegionFrom(marker, regionName, player, dim);
            case SPHERE:
                return sphericalRegionFrom(marker, regionName, player, dim);
            default:
                return null;
        }
    }

    private static SphereRegion sphericalRegionFrom(MarkerStick marker, String regionName, PlayerEntity player, RegistryKey<World> dim) {
        return null;
    }

    private static CuboidRegion cuboidRegionFrom(MarkerStick marker, String regionName, PlayerEntity player) {
        return cuboidRegionFrom(marker, regionName, player, marker.getDimension());
    }

    private static CuboidRegion cuboidRegionFrom(MarkerStick marker, String regionName, PlayerEntity player, RegistryKey<World> dim) {
        List<BlockPos> blocks = marker.getMarkedBlocks();
        CuboidArea cuboidArea = new CuboidArea(blocks);
        if (marker.getTeleportPos() != null) {
            return new CuboidRegion(regionName, cuboidArea, marker.getTeleportPos(), player, dim);
        }
        return new CuboidRegion(regionName, cuboidArea, player, dim);
    }

    /**
     * Returns all involved regions for this event.
     * An involved region is defined as follows: <br>
     * 1. The region is active- <br>
     * 2. The region contains the flag- <br>
     * 3. The containing flag is active.
     *
     * @param flag     which must be contained in the region and must be active
     * @param position the position which must be in the region
     * @param dim      the dimension to check regions for
     * @return a list of regions which match the criteria, can be empty.
     */
    public static List<IMarkableRegion> getInvolvedRegionsFor(RegionFlag flag, BlockPos position, RegistryKey<World> dim) {
        return RegionDataManager.get().getRegionsFor(dim).stream()
                .filter(IMarkableRegion::isActive)
                .filter(region -> region.containsFlag(flag) && region.getFlag(flag.name).isActive())
                // position check should always be the last check to do, because it is the most computation expensive
                .filter(region -> region.contains(position))
                .collect(Collectors.toList());
    }

    // TODO: Start from dimRegion with only the parents, so possible regions to check are far less
    public static List<IMarkableRegion> getInvolvedRegionsFor(RegionFlag flag, BlockPos position, PlayerEntity player, RegistryKey<World> dim) {
        return RegionDataManager.get().getRegionsFor(dim).stream()
                .filter(IMarkableRegion::isActive)
                .filter(region -> region.containsFlag(flag) && region.getFlag(flag.name).isActive())
                .filter(region -> !region.permits(player))
                // position check should always be the last check to do, because it is the most computation expensive
                .filter(region -> region.contains(position))
                .collect(Collectors.toList());
    }

    public static IMarkableRegion getInvolvedRegionFor(RegionFlag flag, BlockPos position, RegistryKey<World> dim) {
        List<IMarkableRegion> regionsForPos = getInvolvedRegionsFor(flag, position, dim);
        if (regionsForPos.isEmpty()) {
            return null;
        } else {
            return Collections.max(regionsForPos, Comparator.comparing(IMarkableRegion::getPriority));
        }
    }

    public static IMarkableRegion getInvolvedRegionFor(RegionFlag flag, BlockPos position, PlayerEntity player, RegistryKey<World> dim) {
        List<IMarkableRegion> regionsForPos = getInvolvedRegionsFor(flag, position, player, dim);
        if (regionsForPos.isEmpty()) {
            return null;
        } else {
            return Collections.max(regionsForPos, Comparator.comparing(IMarkableRegion::getPriority));
        }
    }

    /**
     * Gets intersecting region at the same region hierarchy.
     *
     * @param cuboidRegion
     * @return
     */
    public static List<CuboidRegion> getIntersectingRegionsFor(CuboidRegion cuboidRegion) {
        return cuboidRegion.getParent().getChildren().values()
                .stream()
                .filter(r -> !r.equals(cuboidRegion)) // filter input region from the result
                .map(r -> (CuboidRegion) r)
                .filter(r -> ((CuboidArea) cuboidRegion.getArea()).intersects((CuboidArea) (r).getArea()))
                .collect(Collectors.toList());
    }

    public static List<CuboidRegion> getIntersectingWithSamePriority(CuboidRegion cuboidRegion) {
        return cuboidRegion.getParent().getChildren().values()
                .stream()
                .filter(r -> !r.equals(cuboidRegion)) // filter input region from the result
                .map(r -> (CuboidRegion) r)
                .filter(region -> ((CuboidArea) cuboidRegion.getArea()).intersects((CuboidArea) (region).getArea()))
                .filter(r -> r.getPriority() == cuboidRegion.getPriority())
                .collect(Collectors.toList());
    }


    public static int ensureHigherRegionPriorityFor(CuboidRegion cuboidRegion, int defaultPriority) {
        List<CuboidRegion> intersectingRegions = getIntersectingRegionsFor(cuboidRegion);
        boolean hasRegionWithSamePriority = intersectingRegions.stream().anyMatch(r -> r.getPriority() == cuboidRegion.getPriority());
        if (hasRegionWithSamePriority) {
            int maxPriority = intersectingRegions.stream().mapToInt(AbstractMarkableRegion::getPriority).max().getAsInt();
            cuboidRegion.setPriority(maxPriority + 1);
        } else {
            cuboidRegion.setPriority(defaultPriority);
        }
        return cuboidRegion.getPriority();
    }


    // TODO: recursive check for ensuring region priorities
    public static void rectifyRegionPriorities(CuboidRegion parent, int defaultPriority) {
        List<CuboidRegion> children = getIntersectingRegionsFor(parent);
        if (children.size() == 0) {
            return;
        }
        for (CuboidRegion child : children) {

            rectifyRegionPriorities(child, parent.getPriority());
        }


        List<CuboidRegion> intersectingRegions = getIntersectingRegionsFor(parent);
        boolean hasRegionWithSamePriority = intersectingRegions.stream().anyMatch(r -> r.getPriority() == parent.getPriority());
        if (hasRegionWithSamePriority) {
            int minPriority = intersectingRegions.stream().mapToInt(AbstractMarkableRegion::getPriority).min().getAsInt();
            parent.setPriority(minPriority - 1);
        } else {
            parent.setPriority(defaultPriority);
        }
    }

    public static int ensureLowerRegionPriorityFor(CuboidRegion cuboidRegion, int defaultPriority) {
        List<CuboidRegion> intersectingRegions = getIntersectingRegionsFor(cuboidRegion);
        boolean hasRegionWithSamePriority = intersectingRegions.stream().anyMatch(r -> r.getPriority() == cuboidRegion.getPriority());
        if (hasRegionWithSamePriority) {
            int minPriority = intersectingRegions.stream().mapToInt(AbstractMarkableRegion::getPriority).min().getAsInt();
            cuboidRegion.setPriority(minPriority - 1);
        } else {
            cuboidRegion.setPriority(defaultPriority);
        }
        return cuboidRegion.getPriority();
    }

}
