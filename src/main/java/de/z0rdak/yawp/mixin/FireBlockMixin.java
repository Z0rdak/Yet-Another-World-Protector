package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.block.FireBlock;
import net.minecraft.world.level.block.state.BlockState;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import java.util.Random;


@Mixin(FireBlock.class)
public abstract class FireBlockMixin {

    @Inject(method = "tick", at = @At(value = "HEAD"), cancellable = true, remap = false)
    private void onFireTick(BlockState state, ServerLevel world, BlockPos pos, Random rand, CallbackInfo info) {
        if (!world.isClientSide) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(world.dimension());
            /*
            FlagCheckEvent flagCheckEvent = HandlerUtil.checkTargetEvent(pos, NO_FIRE_TICK, dimCache.getDimensionalRegion());
            if (flagCheckEvent.isDenied()) {
               info.cancel();
            }

             */
        }
    }
}