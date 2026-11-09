package de.z0rdak.yawp.gametest;

import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraft.world.phys.Vec3;

public class GameTestUtils {

    /**
     * Creates a mock ServerPlayer in the level of the given GameTestHelper.
     * Acts as a wrapper around the helper make mock to correctly initialize the player.
     * @return a mock ServerPlayer in the level of the given GameTestHelper
     */
    protected static ServerPlayer makeMockServerPlayer(GameTestHelper helper) {
        ServerPlayer player = helper.makeMockServerPlayerInLevel();
        return player;
    }

    /**
     * Simulates a player clicking on a block with an item in hand.
     * @param helper The GameTestHelper to use for the click
     * @param player The player to use for the click
     * @param item The item to use for the click
     * @param sneaking If the player is sneaking while clicking
     * @param pos The position of the block to click on
     * @param face The face of the block to click on
     * @return The result of the click action, sent through the ServerPlayer's game mode.
     */
    protected static InteractionResult click(GameTestHelper helper, ServerPlayer player, Item item,
                                           boolean sneaking, BlockPos pos, Direction face) {
        BlockPos abs = helper.absolutePos(pos);
        ItemStack stack = new ItemStack(item);
        player.setItemInHand(InteractionHand.MAIN_HAND, stack);
        player.setShiftKeyDown(sneaking);
        Vec3 hitPos = Vec3.atCenterOf(abs).add(Vec3.atLowerCornerOf(face.getUnitVec3i()).scale(0.5));
        BlockHitResult hit = new BlockHitResult(hitPos, face, abs, false);
        return player.gameMode.useItemOn(player, helper.getLevel(), stack, InteractionHand.MAIN_HAND, hit);
    }
}
