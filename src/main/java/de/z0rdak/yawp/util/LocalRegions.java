package de.z0rdak.yawp.util;

import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.flag.BooleanFlag;
import de.z0rdak.yawp.core.flag.FlagType;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.*;
import de.z0rdak.yawp.core.stick.MarkerStick;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

import javax.annotation.Nullable;
import java.util.*;
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

    private static CuboidRegion cuboidRegionFrom(MarkerStick marker, String regionName, PlayerEntity player){
        return cuboidRegionFrom(marker, regionName, player, marker.getDimension());
    }

    private static CuboidRegion cuboidRegionFrom(MarkerStick marker, String regionName, PlayerEntity player, RegistryKey<World> dim){
        List<BlockPos> blocks = marker.getMarkedBlocks();
        // TODO: test this before hand? this would eliminate these double methods with and without dim
        if (blocks.size() != 2) {
            return null;
        }
        CuboidArea cuboidArea = new CuboidArea(blocks);
        if (marker.getTeleportPos() == null) {
            return new CuboidRegion(regionName, cuboidArea, marker.getTeleportPos(), player, dim);
        }
        return new CuboidRegion(regionName, cuboidArea, player, dim);
    }


    // TODO: Maybe trim down number of regions even more by filtering regions of active chunks
    // TODO: What about children regions and parent regions?
    /**
     * Returns all involved regions for this event.
     * An involved region is defined as follows: <br>
     * 1. The region is active- <br>
     * 2. The region contains the flag- <br>
     * 3. The containing flag is active.
     * @param flag which must be contained in the region and must be active
     * @param position the position which must be in the region
     * @param dim the dimension to check regions for
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

    public static List<IMarkableRegion> getRegionsAround(BlockPos position, RegistryKey<World> dim) {
        return RegionDataManager.get().getRegionsFor(dim).stream()
                .filter(region -> region.contains(position))
                .collect(Collectors.toList());
    }

    public static List<IMarkableRegion> getRegionsFor(RegionFlag flag, BlockPos position, RegistryKey<World> dim) {
        return getRegionsWithHighestPriority(getInvolvedRegionsFor(flag, position, dim));
    }

    @Nullable
    public static IMarkableRegion getInvolvedRegionFor(RegionFlag flag, BlockPos position, RegistryKey<World> dim) {
        List<IMarkableRegion> regionsForPos = getInvolvedRegionsFor(flag, position, dim);
        if (regionsForPos.isEmpty()) {
            return null;
        } else {
            return Collections.max(regionsForPos, Comparator.comparing(IMarkableRegion::getPriority));
        }
    }

    /**
     * Filter the regions for the ones with the highest priority. <br>
     * This list may contain multiple regions with the same priority. <br>
     * In an optimal region setup, the list only contains one region. <br>
     * It is assumed that the provided list of regions only contains 'involved' regions.
     * @param involvedRegions valid involved regions, may be empty.
     * @return a reduced list of involved regions with the highest priority
     */
    public static List<IMarkableRegion> getRegionsWithHighestPriority(List<IMarkableRegion> involvedRegions) {
        int highestHandlingPriority = Integer.MIN_VALUE;
        List<IMarkableRegion> filteredRegions = new ArrayList<>();
        for (IMarkableRegion region : involvedRegions) {
            if (region.getPriority() == highestHandlingPriority) {
                involvedRegions.add(region);
            } else if (region.getPriority() > highestHandlingPriority) {
                involvedRegions.clear();
                highestHandlingPriority = region.getPriority();
                involvedRegions.add(region);
            }
        }
        return filteredRegions;
    }

    /**
     * Regions are assumed to already be checked to contain the specified flag
     * @param player which is triggering the event
     * @param dimRegion involved dimensional region
     * @param regions list of regions which do contain the flag to be checked
     * @param targetPos event source position
     * @param flag value to be checked
     * @return
     */
    public static boolean checkFlagByPassForPlayerEvent(PlayerEntity player, DimensionalRegion dimRegion, List<IMarkableRegion> regions, BlockPos targetPos, RegionFlag flag) {
        boolean isMemberOfDim = dimRegion.permits(player);



        IFlag dimFlag = dimRegion.getFlagContainer().get(flag.name);
        FlagType dimFlagType = dimFlag.getFlagType();
        boolean isActive = dimFlag.isActive();
        boolean isInverted = dimFlag.isInverted();



        switch (dimFlagType) {
            case BOOLEAN_FLAG:
                BooleanFlag boolFlag = (BooleanFlag) dimFlag;

                break;
            case LIST_FLAG:
                break;
            case INT_FLAG:
                break;
        }
        return false;
    }


}