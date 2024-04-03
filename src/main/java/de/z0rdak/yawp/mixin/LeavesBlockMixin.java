package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.api.events.region.FlagCheckResult;
import de.z0rdak.yawp.handler.flags.HandlerUtil;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.block.LeavesBlock;
import net.minecraft.world.level.block.state.BlockState;
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

    @Inject(method = "randomTick", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/level/block/LeavesBlock;dropResources(Lnet/minecraft/world/level/block/state/BlockState;Lnet/minecraft/world/level/Level;Lnet/minecraft/core/BlockPos;)V"), cancellable = true)
    private void spread(BlockState state, ServerLevel world, BlockPos pos, Random rnd, CallbackInfo info) {
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
