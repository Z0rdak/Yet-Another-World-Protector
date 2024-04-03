package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.api.events.region.FlagCheckResult;
import de.z0rdak.yawp.handler.flags.HandlerUtil;
import net.minecraft.block.BlockState;
import net.minecraft.block.LeavesBlock;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.server.ServerWorld;
import net.minecraftforge.common.MinecraftForge;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import java.util.Random;

import static de.z0rdak.yawp.core.flag.RegionFlag.LEAF_DECAY;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.handleAndSendMsg;

@Mixin(LeavesBlock.class)
public class LeavesBlockMixin {

    @Inject(method = "randomTick", at = @At(value = "INVOKE", target = "Lnet/minecraft/block/LeavesBlock;dropResources(Lnet/minecraft/block/BlockState;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)V"), cancellable = true)
    private void spread(BlockState state, ServerWorld world, BlockPos pos, Random rnd, CallbackInfo info) {
        if (!world.isClientSide) {
            if (!world.isClientSide) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(pos, LEAF_DECAY, world.dimension(), null);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
                FlagCheckResult result = HandlerUtil.evaluate(checkEvent);
                MinecraftForge.EVENT_BUS.post(result);
                handleAndSendMsg(result, null, denyResult -> {
                    info.cancel();
                });
            }
        }
    }
}
