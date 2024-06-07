package de.z0rdak.yawp.util;

import static de.z0rdak.yawp.core.flag.RegionFlag.MOB_GRIEFING;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.checkTargetEvent;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.getEntityDim;

import de.z0rdak.yawp.handler.flags.FlagCheckEvent;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

public final class MobGriefingHelper {

    MobGriefingHelper() {}

    public static boolean preventGrief(Entity entity) {
        World world = entity.getWorld();
        return preventGrief(world, entity);
    }

    public static boolean preventGrief(World world, Entity entity) {
        DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(entity));
        FlagCheckEvent flagCheck = checkTargetEvent(entity.getBlockPos(), MOB_GRIEFING, dimCache.getDimensionalRegion());
        return flagCheck.isDenied();
    }

    public static boolean preventGrief(World world, BlockPos pos) {
        DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(world.getRegistryKey());
        FlagCheckEvent flagCheck = checkTargetEvent(pos, MOB_GRIEFING, dimCache.getDimensionalRegion());
        return flagCheck.isDenied();
    }


}