package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.api.FlagRegister;
import de.z0rdak.yawp.api.events.flag.FlagCheckRequest;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.util.RandomSource;
import net.minecraft.world.level.block.FireBlock;
import net.minecraft.world.level.block.state.BlockState;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import static de.z0rdak.yawp.handler.HandlerUtil.isServerSide;
import static de.z0rdak.yawp.api.FlagEvaluator.processCheck;

@Mixin(FireBlock.class)
public abstract class FireBlockMixin {

    @Inject(method = "tick", at = @At(value = "HEAD"), cancellable = true)
    private void onFireTick(BlockState state, ServerLevel world, BlockPos pos, RandomSource rand, CallbackInfo info) {
        if (isServerSide(world)) {
            FlagCheckRequest checkEvent = new FlagCheckRequest(pos, FlagRegister.FIRE_TICK, world.dimension());
            if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, denyResult -> {
                info.cancel();
            });
        }
    }
}