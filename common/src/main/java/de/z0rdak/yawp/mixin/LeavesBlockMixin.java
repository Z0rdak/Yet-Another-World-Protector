package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.util.RandomSource;
import net.minecraft.world.level.block.LeavesBlock;
import net.minecraft.world.level.block.state.BlockState;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import static de.z0rdak.yawp.core.flag.RegionFlag.LEAF_DECAY;
import static de.z0rdak.yawp.handler.HandlerUtil.isServerSide;
import static de.z0rdak.yawp.api.FlagEvaluator.processCheck;

@Mixin(LeavesBlock.class)
public class LeavesBlockMixin {

    @Inject(method = "randomTick", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/level/block/LeavesBlock;dropResources(Lnet/minecraft/world/level/block/state/BlockState;Lnet/minecraft/world/level/Level;Lnet/minecraft/core/BlockPos;)V"), cancellable = true)
    private void spread(BlockState state, ServerLevel level, BlockPos pos, RandomSource rnd, CallbackInfo ci) {
        if (isServerSide(level)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(pos, LEAF_DECAY, level.dimension());
            if (Services.EVENT.post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, denyResult -> {
                ci.cancel();
            });
        }
    }
}
