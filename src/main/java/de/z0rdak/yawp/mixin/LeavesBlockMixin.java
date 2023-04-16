package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.handler.flags.FlagCheckEvent;
import de.z0rdak.yawp.handler.flags.HandlerUtil;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.block.BlockState;
import net.minecraft.block.LeavesBlock;
import net.minecraft.server.world.ServerWorld;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.random.Random;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import static de.z0rdak.yawp.core.flag.RegionFlag.LEAF_DECAY;

@Mixin(LeavesBlock.class)
public class LeavesBlockMixin {

    @Inject(method = "randomTick", at = @At(value = "HEAD"), cancellable = true)
    private void spread(BlockState state, ServerWorld world, BlockPos pos, Random random, CallbackInfo info) {
        if (!world.isClient) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(world.getRegistryKey());
            FlagCheckEvent flagCheckEvent = HandlerUtil.checkTargetEvent(pos, LEAF_DECAY, dimCache.getDimensionalRegion());
            if (flagCheckEvent.isDenied()) {
                info.cancel();
            }
        }
    }
}
