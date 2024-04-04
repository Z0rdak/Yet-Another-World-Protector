package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.handler.flags.HandlerUtil;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.block.FireBlock;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraftforge.common.MinecraftForge;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import java.util.Random;

import static de.z0rdak.yawp.core.flag.RegionFlag.FIRE_TICK;


@Mixin(FireBlock.class)
public abstract class FireBlockMixin {

    @Inject(method = "tick", at = @At(value = "HEAD"), cancellable = true)
    private void onFireTick(BlockState state, ServerLevel world, BlockPos pos, Random rand, CallbackInfo info) {
        if (!world.isClientSide) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(pos, FIRE_TICK, world.dimension(), null);
            if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                return;
            }
            HandlerUtil.processCheck(checkEvent, null, denyResult -> {
                info.cancel();
            });
        }
    }
}