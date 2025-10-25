package de.z0rdak.yawp.mixin.flag;

import net.fabricmc.fabric.api.event.Event;
import net.fabricmc.fabric.api.event.EventFactory;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.state.BlockState;

public interface BlockBreakCallback {
    Event<BlockBreakCallback> EVENT = EventFactory.createArrayBacked(
            BlockBreakCallback.class,
            (listeners) -> (player, world, pos, state, preCanceled) -> {
                for (BlockBreakCallback listener : listeners) {
                    BlockBreakResult result = listener.onBreak(player, world, pos, state, preCanceled);
                    if (result != BlockBreakResult.PASS) return result;
                }
                return BlockBreakResult.PASS;
            }
    );

    BlockBreakResult onBreak(ServerPlayer player, Level world, BlockPos pos, BlockState state, boolean preCanceled);
}
