package de.z0rdak.yawp.util;

import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.flag.FlagContainer;
import de.z0rdak.yawp.core.flag.FlagState;
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
import java.util.*;
import java.util.stream.Collectors;

public final class LocalRegions {

    private LocalRegions() {
    }

    /**
     * Gets flags of region sorted by active state and name
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

    public static Map<FlagState, List<IFlag>> sortFlagsByState(FlagContainer flagContainer) {
        List<IFlag> denied = flagContainer.values().stream()
                .filter(f -> f.getState() == FlagState.DENIED)
                .sorted()
                .collect(Collectors.toList());
        List<IFlag> allowed = flagContainer.values().stream()
                .filter(f -> f.getState() == FlagState.ALLOWED)
                .sorted()
                .collect(Collectors.toList());
        List<IFlag> disabled = flagContainer.values().stream()
                .filter(f -> f.getState() == FlagState.ALLOWED)
                .sorted()
                .collect(Collectors.toList());
        List<IFlag> undefined = flagContainer.values().stream()
                .filter(f -> f.getState() == FlagState.ALLOWED)
                .sorted()
                .collect(Collectors.toList());

        HashMap<FlagState, List<IFlag>> flagStateListMap = new HashMap<>();
        flagStateListMap.put(FlagState.DENIED, denied);
        flagStateListMap.put(FlagState.ALLOWED, allowed);
        flagStateListMap.put(FlagState.DISABLED, disabled);
        flagStateListMap.put(FlagState.UNDEFINED, undefined);
        return flagStateListMap;
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
    @Deprecated
    public static List<IMarkableRegion> getInvolvedRegionsFor(RegionFlag flag, BlockPos position, ResourceKey<Level> dim) {
        return RegionDataManager.get().getRegionsFor(dim).stream()
                .filter(IMarkableRegion::isActive)
                .filter(region -> region.containsFlag(flag) && region.getFlag(flag.name).isActive())
                .filter(region -> region.contains(position))
                .collect(Collectors.toList());
    }

    @Nullable
    public static IMarkableRegion getRegionWithoutFlag(RegionFlag flag, BlockPos position, ResourceKey<Level> dim) {
        List<IMarkableRegion> regionsForPos = RegionDataManager.get().getRegionsFor(dim).stream()
                .filter(IMarkableRegion::isActive)
                .filter(region -> !region.containsFlag(flag))
                .filter(region -> region.contains(position))
                .collect(Collectors.toList());
        if (regionsForPos.isEmpty()) {
            return null;
        } else {
            return Collections.max(regionsForPos, Comparator.comparing(IMarkableRegion::getPriority));
        }
    }

    public static List<IMarkableRegion> getIntersectingRegions(IMarkableRegion region) {
        return RegionDataManager.get().getRegionsFor(region.getDim()).stream()
                .filter(r -> !r.equals(region)) // filter input region from the result
                .map(r -> (CuboidRegion) r)
                .filter(r -> ((CuboidArea) region.getArea()).intersects((CuboidArea) (r).getArea()))
                .collect(Collectors.toList());
    }

    public static boolean hasAnyRegionWithSamePriority(IMarkableRegion region, int priority) {
        return hasAnyRegionWithSamePriority(getIntersectingRegionsFor(region), priority);
    }

    public static int ensureHigherRegionPriorityFor(IMarkableRegion cuboidRegion, int defaultPriority) {
        List<IMarkableRegion> intersectingRegions = getIntersectingRegionsFor(cuboidRegion);
        boolean hasRegionWithSamePriority = hasAnyRegionWithSamePriority(intersectingRegions, defaultPriority);
        if (hasRegionWithSamePriority) {
            int maxPriority = intersectingRegions.stream()
                    .map(r -> (CuboidRegion) r)
                    .mapToInt(AbstractMarkableRegion::getPriority)
                    .max().getAsInt();
            cuboidRegion.setPriority(maxPriority + 1);
        } else {
            cuboidRegion.setPriority(defaultPriority);
        }
        return cuboidRegion.getPriority();
    }

    // TODO: FIXME
    // TODO: recursive check for ensuring region priorities
    @Deprecated
    public static void rectifyRegionPriorities(IMarkableRegion parent, int defaultPriority) {
        List<IMarkableRegion> children = getIntersectingRegionsFor(parent);
        if (children.size() == 0) {
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

    public static int ensureLowerRegionPriorityFor(IMarkableRegion cuboidRegion, int defaultPriority) {
        List<IMarkableRegion> intersectingRegions = getIntersectingRegionsFor(cuboidRegion);
        boolean hasRegionWithSamePriority = intersectingRegions.stream().anyMatch(r -> r.getPriority() == cuboidRegion.getPriority());
        if (hasRegionWithSamePriority) {
            int minPriority = intersectingRegions.stream().mapToInt(IMarkableRegion::getPriority).min().getAsInt();
            cuboidRegion.setPriority(minPriority - 1);
        } else {
            cuboidRegion.setPriority(defaultPriority);
        }
        return cuboidRegion.getPriority();
    }

    private static List<IMarkableRegion> getIntersectingRegionsFor(IMarkableRegion cuboidRegion) {
        return cuboidRegion.getParent().getChildren().values()
                .stream()
                .filter(r -> !r.equals(cuboidRegion)) // filter input region from the result
                .map(r -> (CuboidRegion) r)
                .filter(r -> ((CuboidArea) cuboidRegion.getArea()).intersects((CuboidArea) (r).getArea()))
                .collect(Collectors.toList());
    }

    private static boolean hasAnyRegionWithSamePriority(List<IMarkableRegion> region, int priority) {
        return region.stream().anyMatch(r -> r.getPriority() == priority);
    }

    private static List<CuboidRegion> getIntersectingWithSamePriority(CuboidRegion cuboidRegion) {
        return cuboidRegion.getParent().getChildren().values()
                .stream()
                .filter(r -> !r.equals(cuboidRegion)) // filter input region from the result
                .map(r -> (CuboidRegion) r)
                .filter(region -> ((CuboidArea) cuboidRegion.getArea()).intersects((CuboidArea) (region).getArea()))
                .filter(r -> r.getPriority() == cuboidRegion.getPriority())
                .collect(Collectors.toList());
    }

}
