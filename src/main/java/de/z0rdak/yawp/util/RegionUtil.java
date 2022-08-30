package de.z0rdak.yawp.util;

import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.region.AbstractMarkableRegion;
import de.z0rdak.yawp.core.region.CuboidRegion;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.SphereRegion;
import de.z0rdak.yawp.core.stick.MarkerStick;
import net.minecraft.core.BlockPos;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import org.apache.commons.lang3.NotImplementedException;

import java.util.List;

public final class RegionUtil {

    private RegionUtil() {
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
            case UNKNOWN:
            default:
                return null;
        }
    }

    private static SphereRegion sphericalRegionFrom(MarkerStick marker, String regionName, Player player, ResourceKey<Level> dim) {
        return null;
    }

    private static CuboidRegion cuboidRegionFrom(MarkerStick marker, String regionName, Player player){
        return cuboidRegionFrom(marker, regionName, player, marker.getDimension());
    }

    private static CuboidRegion cuboidRegionFrom(MarkerStick marker, String regionName, Player player, ResourceKey<Level> dim){
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

    public static List<IMarkableRegion> getHandlingRegionsFor(BlockPos blockPos, Level world) {
        throw new NotImplementedException("");
    }
}
