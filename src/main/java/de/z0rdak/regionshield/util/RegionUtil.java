package de.z0rdak.regionshield.util;

import de.z0rdak.regionshield.core.area.CuboidArea;
import de.z0rdak.regionshield.core.region.AbstractMarkableRegion;
import de.z0rdak.regionshield.core.region.CuboidRegion;
import de.z0rdak.regionshield.core.stick.MarkerStick;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.util.math.BlockPos;

import java.util.List;

public final class RegionUtil {

    private RegionUtil() {
    }

    // TODO: create region from marker stick info
    public static AbstractMarkableRegion regionFrom(PlayerEntity player, MarkerStick marker, String regionName) {
        switch (marker.getAreaType()) {
            case CUBOID:
                return cuboidRegionFrom(marker, regionName, player);
            case CYLINDER:
            case SPHERE:
            case PRISM:
            case POLYGON_3D:
            case UNKNOWN:
            default:
                return null;
        }
    }

    private static CuboidRegion cuboidRegionFrom(MarkerStick marker, String regionName, PlayerEntity player){
        List<BlockPos> blocks = marker.getMarkedBlocks();
        if (blocks.size() != 2) {
            return null;
        }
        CuboidArea cuboidArea = new CuboidArea(blocks);
        if (marker.getTeleportPos() == null) {
            return new CuboidRegion(regionName, cuboidArea, marker.getTeleportPos(), player, marker.getDimension());
        }
        return new CuboidRegion(regionName, cuboidArea, player, marker.getDimension());
    }
}
