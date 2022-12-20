package de.z0rdak.yawp.util;

import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import net.minecraft.entity.player.PlayerEntity;

public final class DimensionalRegions {

    private DimensionalRegions() {}

    public static boolean isProhibitedPlayerAction(PlayerEntity player, RegionFlag regionFlag, DimensionalRegion dimRegion) {
        if (dimRegion.isActive()) {
            if (!dimRegion.permits(player)) {
                if (dimRegion.containsFlag(regionFlag)) {
                    IFlag flag = dimRegion.getFlag(regionFlag.name);

                }
            }
            return dimRegion.containsFlag(regionFlag) && !dimRegion.permits(player);
        } else {
            return false;
        }
    }


}
