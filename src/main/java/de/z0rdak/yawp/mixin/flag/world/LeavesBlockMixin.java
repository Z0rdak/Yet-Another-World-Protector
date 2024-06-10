package de.z0rdak.yawp.mixin.flag.world;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import net.minecraft.block.BlockState;
import net.minecraft.block.LeavesBlock;
import net.minecraft.server.world.ServerWorld;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.random.Random;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import static de.z0rdak.yawp.api.events.region.RegionEvents.post;
import static de.z0rdak.yawp.core.flag.RegionFlag.LEAF_DECAY;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;
import static de.z0rdak.yawp.util.MessageSender.sendFlagMsg;

@Mixin(LeavesBlock.class)
public class LeavesBlockMixin {

    @Inject(method = "randomTick", at = @At(value = "HEAD"), cancellable = true)
    private void spread(BlockState state, ServerWorld world, BlockPos pos, Random random, CallbackInfo ci) {
        if (isServerSide(world)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(pos, LEAF_DECAY, getDimKey(world), null);
            if (post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, null, deny -> {
                sendFlagMsg(deny);
                ci.cancel();
            });
        }
    }
}
