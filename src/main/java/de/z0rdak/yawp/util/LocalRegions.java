package de.z0rdak.yawp.util;

import de.z0rdak.yawp.commands.CommandUtil;
import de.z0rdak.yawp.config.server.CommandPermissionConfig;
import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.area.IMarkableArea;
import de.z0rdak.yawp.core.area.SphereArea;
import de.z0rdak.yawp.core.flag.FlagContainer;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.region.CuboidRegion;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.core.region.SphereRegion;
import de.z0rdak.yawp.core.stick.MarkerStick;
import de.z0rdak.yawp.handler.flags.FlagCorrelation;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.apache.commons.lang3.NotImplementedException;

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

    public static Map<FlagState, List<IFlag>> sortFlagsByState(IProtectedRegion region) {
        List<IFlag> denied = getFlagsWithState(region.getFlagContainer(), FlagState.DENIED);
        List<IFlag> allowed = getFlagsWithState(region.getFlagContainer(), FlagState.ALLOWED);
        List<IFlag> disabled = getFlagsWithState(region.getFlagContainer(), FlagState.DISABLED);
        HashMap<FlagState, List<IFlag>> flagStateListMap = new HashMap<>();
        flagStateListMap.put(FlagState.DENIED, denied);
        flagStateListMap.put(FlagState.ALLOWED, allowed);
        flagStateListMap.put(FlagState.DISABLED, disabled);
        return flagStateListMap;
    }

    public static Map<FlagState, List<FlagCorrelation>> sortFlagsByState(Map<String, FlagCorrelation> flagMap) {
        List<FlagCorrelation> denied = getCorrelationByState(flagMap, FlagState.DENIED);
        List<FlagCorrelation> allowed = getCorrelationByState(flagMap, FlagState.ALLOWED);
        List<FlagCorrelation> disabled = getCorrelationByState(flagMap, FlagState.DISABLED);
        HashMap<FlagState, List<FlagCorrelation>> flagStateListMap = new HashMap<>();
        flagStateListMap.put(FlagState.DENIED, denied);
        flagStateListMap.put(FlagState.ALLOWED, allowed);
        flagStateListMap.put(FlagState.DISABLED, disabled);
        return flagStateListMap;
    }

    private static List<FlagCorrelation> getCorrelationByState(Map<String, FlagCorrelation> flagMap, FlagState state) {
        return flagMap.values().stream()
                .filter(correlation -> correlation.getFlag().getState() == state)
                .collect(Collectors.toList());
    }

    public static List<IFlag> getFlagsWithState(FlagContainer flags, FlagState state) {
        return flags.values().stream()
                .filter(flag -> flag.getState() == state)
                .collect(Collectors.toList());
    }

    public static IMarkableRegion regionFrom(PlayerEntity player, MarkerStick marker, String regionName) {
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

    public static IMarkableRegion regionFrom(PlayerEntity player, MarkerStick marker, String regionName, RegistryKey<World> dim) {
        switch (marker.getAreaType()) {
            case CUBOID:
                return cuboidRegionFrom(marker, regionName, player, dim);
            case SPHERE:
                return sphericalRegionFrom(marker, regionName, player, dim);
            default:
                throw new NotImplementedException("Area type not implemented yet");
        }
    }

    private static SphereRegion sphericalRegionFrom(MarkerStick marker, String regionName, PlayerEntity player, RegistryKey<World> dim) {
        List<BlockPos> blocks = marker.getMarkedBlocks();
        SphereArea sphereArea = new SphereArea(blocks.get(0), blocks.get(1));
        if (marker.getTeleportPos() != null) {
            return new SphereRegion(regionName, sphereArea, marker.getTeleportPos(), player, dim);
        }
        return new SphereRegion(regionName, sphereArea, player, dim);
    }

    private static CuboidRegion cuboidRegionFrom(MarkerStick marker, String regionName, PlayerEntity player, RegistryKey<World> dim) {
        List<BlockPos> blocks = marker.getMarkedBlocks();
        CuboidArea cuboidArea = new CuboidArea(blocks.get(0), blocks.get(1));
        if (marker.getTeleportPos() != null) {
            return new CuboidRegion(regionName, cuboidArea, marker.getTeleportPos(), player, dim);
        }
        return new CuboidRegion(regionName, cuboidArea, player, dim);
    }

    public static IMarkableRegion regionFromArea(IMarkableArea area, BlockPos tpTarget, String regionName, RegistryKey<World> dim) {
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
                .filter(r -> (region.getArea()).intersects((r).getArea()))
                .collect(Collectors.toList());
        List<IMarkableRegion> containingRegions = regionsInDim.stream()
                .filter(r -> region.getArea().containsOther(r.getArea()))
                .collect(Collectors.toList());
        return new RegionOverlappingInfo(region, intersectingRegions, containingRegions);
    }

    public static RegionOverlappingInfo getOverlappingOwned(IMarkableRegion region, PlayerEntity player) {
        RegionOverlappingInfo overlappingRegions = getOverlappingRegions(region);
        List<IMarkableRegion> intersecting = overlappingRegions.intersectingRegions.stream()
                .filter(r -> r.permits(player))
                .collect(Collectors.toList());
        List<IMarkableRegion> contained = overlappingRegions.containingRegions.stream()
                .filter(r -> r.permits(player))
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
        public final IMarkableRegion region;
        public final List<IMarkableRegion> intersectingRegions;
        public final List<IMarkableRegion> containingRegions;

        public RegionOverlappingInfo(IMarkableRegion region, List<IMarkableRegion> intersectingRegions, List<IMarkableRegion> containingRegions) {
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
