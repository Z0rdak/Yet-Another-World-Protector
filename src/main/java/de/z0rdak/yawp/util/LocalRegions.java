package de.z0rdak.yawp.util;

import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.*;
import de.z0rdak.yawp.core.stick.MarkerStick;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.core.BlockPos;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import org.jetbrains.annotations.NotNull;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

public final class LocalRegions {

    private LocalRegions() {
    }

    /**
     * Gets flags of region sorted by active state and name
     *
     * @param region
     * @return
     */
    public static List<IFlag> getSortedFlags(IProtectedRegion region) {
        List<IFlag> activeFlags = region.getFlags().stream()
                .filter(IFlag::isActive)
                .sorted()
                .collect(Collectors.toList());
        List<IFlag> inActiveFlags = region.getFlags().stream()
                .filter(f -> !f.isActive())
                .sorted()
                .collect(Collectors.toList());
        List<IFlag> flags = new ArrayList<>(activeFlags);
        flags.addAll(inActiveFlags);
        return flags;
    }

    public static AbstractMarkableRegion regionFrom(Player player, MarkerStick marker, String regionName) {
        return regionFrom(player, marker, regionName, marker.getDimension());
    }

    public static AbstractMarkableRegion regionFrom(Player player, MarkerStick marker, String regionName, ResourceKey<Level> dim) {
        switch (marker.getAreaType()) {
            case CUBOID:
                return cuboidRegionFrom(marker, regionName, player, dim);
            case SPHERE:
                return sphericalRegionFrom(marker, regionName, player, dim);
            default:
                return null;
        }
    }

    private static SphereRegion sphericalRegionFrom(MarkerStick marker, String regionName, Player player, ResourceKey<Level> dim) {
        return null;
    }

    private static CuboidRegion cuboidRegionFrom(MarkerStick marker, String regionName, Player player) {
        return cuboidRegionFrom(marker, regionName, player, marker.getDimension());
    }

    private static CuboidRegion cuboidRegionFrom(MarkerStick marker, String regionName, Player player, ResourceKey<Level> dim) {
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
    public static List<IMarkableRegion> getInvolvedRegionsFor(RegionFlag flag, BlockPos position, ResourceKey<Level> dim) {
        return RegionDataManager.get().getRegionsFor(dim).stream()
                .filter(IMarkableRegion::isActive)
                .filter(region -> region.containsFlag(flag) && region.getFlag(flag.name).isActive())
                .filter(region -> region.contains(position))
                .collect(Collectors.toList());
    }

    public static List<IMarkableRegion> getRegionsWithoutFlag(RegionFlag flag, BlockPos position, ResourceKey<Level> dim) {
        return RegionDataManager.get().getRegionsFor(dim).stream()
                .filter(IMarkableRegion::isActive)
                .filter(region -> !region.containsFlag(flag))
                .filter(region -> region.contains(position))
                .collect(Collectors.toList());
    }

    @Nullable
    public static IMarkableRegion getRegionWithoutFlag(RegionFlag flag, BlockPos position, ResourceKey<Level> dim) {
        List<IMarkableRegion> regionsForPos = getRegionsWithoutFlag(flag, position, dim);
        if (regionsForPos.isEmpty()) {
            return null;
        } else {
            return Collections.max(regionsForPos, Comparator.comparing(IMarkableRegion::getPriority));
        }
    }

    /**
     * Gets all active regions which contain the provided position in the given dimension. <br>
     *
     * @param position the position to check for involved regions
     * @param dim      the dimension to check for involved regions
     * @return all active regions which contain the given location and dimension
     */
    public static List<IMarkableRegion> getInvolvedRegionsFor(BlockPos position, ResourceKey<Level> dim) {
        return RegionDataManager.get().getRegionsFor(dim).stream()
                .filter(IMarkableRegion::isActive)
                .filter(region -> region.contains(position))
                .collect(Collectors.toList());
    }

    /**
     * Gets the region with the highest priority among all involved regions at the given location and dimension. <br>
     * This considers the active state of the region as well. <br>
     *
     * @param position the position to check for involved regions
     * @param dim      the dimension to check for involved regions
     * @return the region with the highest priority among all involved regions which contain the given location
     */
    @Nullable
    public static IMarkableRegion getInvolvedRegionFor(BlockPos position, ResourceKey<Level> dim) {
        List<IMarkableRegion> regionsForPos = getInvolvedRegionsFor(position, dim);
        if (regionsForPos.isEmpty()) {
            return null;
        } else {
            return Collections.max(regionsForPos, Comparator.comparing(IMarkableRegion::getPriority));
        }
    }

    /**
     * Gets the responsible region for the given position and dimension. <br>
     * The responsible region is the region with the highest priority among all involved regions at the given location and dimension. <br>
     * If no involved region is found, the dimensional region is returned. <br>
     *
     * @param pos the position to get the responsible region for
     * @param dim the dimension to get the responsible region for
     * @return the responsible region for the given position and dimension
     */
    public static IProtectedRegion getResponsible(BlockPos pos, RegistryKey<World> dim) {
        IMarkableRegion region = getInvolvedRegionFor(pos, dim);
        if (region == null) {
            return RegionDataManager.get().cacheFor(dim).getDimensionalRegion();
        }
        return region;
    }

    /**
     * Gets all active flags of the given region including all it's parents.
     * @param region the region to get active flags
     * @param carry list holding information about region flags
     * @return a list of all active flags of the given region including its parents.
     */
    public static List<FlagCorrelation> getFlagsRecursive(IProtectedRegion region, List<FlagCorrelation> carry) {
        if (region == null) {
            return carry;
        }
        List<FlagCorrelation> parentFlags = region.getFlags().stream()
                .filter(IFlag::isActive)
                .map(f -> new FlagCorrelation(region, getRegionType(region), f))
                .collect(Collectors.toList());
        carry.addAll(parentFlags);
        return getFlagsRecursive(region.getParent(), carry);
    }

    public static List<IFlag> getFlags(IProtectedRegion region, List<IFlag> carry) {
        if (region == null) {
            return carry;
        }
        List<IFlag> parentFlags = region.getFlags().stream()
                .filter(IFlag::isActive)
                .collect(Collectors.toList());
        carry.addAll(parentFlags);
        return getFlags(region.getParent(), carry);
    }

    public static RegionType getRegionType(IProtectedRegion region) {
        return region instanceof DimensionalRegion ? RegionType.DIMENSION : region instanceof IMarkableRegion ? RegionType.LOCAL : RegionType.GLOBAL;
    }

    // TODO: Start from dimRegion with only the parents, so possible regions to check are far less
    public static List<IMarkableRegion> getInvolvedRegionsFor(RegionFlag flag, BlockPos position, Player player, ResourceKey<Level> dim) {
        return RegionDataManager.get().getRegionsFor(dim).stream()
                .filter(IMarkableRegion::isActive)
                .filter(region -> region.containsFlag(flag) && region.getFlag(flag.name).isActive())
                //.filter(region -> !region.permits(player))
                // position check should always be the last check to do, because it is the most computation expensive
                .filter(region -> region.contains(position))
                .collect(Collectors.toList());
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
