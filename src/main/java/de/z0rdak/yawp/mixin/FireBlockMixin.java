package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.api.events.region.FlagCheckResult;
import de.z0rdak.yawp.handler.flags.HandlerUtil;
import net.minecraft.block.BlockState;
import net.minecraft.block.FireBlock;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.server.ServerWorld;
import net.minecraftforge.common.MinecraftForge;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import java.util.Random;

import static de.z0rdak.yawp.core.flag.RegionFlag.FIRE_TICK;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.handleAndSendMsg;


@Mixin(FireBlock.class)
public abstract class FireBlockMixin {

    @Inject(method = "tick", at = @At(value = "HEAD"), cancellable = true)
    private void onFireTick(BlockState state, ServerWorld world, BlockPos pos, Random rand, CallbackInfo info) {
        if (!world.isClientSide) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(pos, FIRE_TICK, world.dimension(), null);
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