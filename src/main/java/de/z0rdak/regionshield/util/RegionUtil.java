package de.z0rdak.regionshield.util;

import de.z0rdak.regionshield.core.area.CuboidArea;
import de.z0rdak.regionshield.core.region.AbstractMarkableRegion;
import de.z0rdak.regionshield.core.region.CuboidRegion;
import de.z0rdak.regionshield.core.stick.MarkerStick;
import net.minecraft.util.math.BlockPos;

import java.util.List;

public final class RegionUtil {

    private RegionUtil() {
    }

    // TODO: create region from marker stick info
    public static AbstractMarkableRegion regionFrom(MarkerStick marker, String regionName) {
        List<BlockPos> blocks = marker.getMarkedBlocks();
        switch (marker.getAreaType()) {
            case CUBOID:
                if (blocks.size() != 2) {
                    return null;
                }
                CuboidArea cuboidArea = new CuboidArea(blocks);
                if (marker.getTeleportPos() == null) {
                    return new CuboidRegion(regionName, cuboidArea.getArea(), marker.getTeleportPos(), marker.getDimension());
                }
                return new CuboidRegion(regionName, cuboidArea, marker.getDimension());
            case UNKNOWN:
            default:
                // TODO: should not happen?
                break;
        }
        return null;
    }
}
