package de.z0rdak.yawp.handler;

import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.stick.MarkerStick;
import de.z0rdak.yawp.util.StickUtil;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraft.world.phys.HitResult;

import static de.z0rdak.yawp.handler.HandlerUtil.isServerSide;
import static de.z0rdak.yawp.util.StickUtil.*;

public class MarkerStickHandler {

    public static void onCreateStick(Player player, ItemStack input, ItemStack output) {
        // split stack and only create one stick, also refund xp
        input.setCount(output.getCount() - 1);
        player.addItem(input);
        player.giveExperienceLevels(1);
        StickUtil.initMarkerNbt(output, player.level().dimension());
    }

    public static void onMarkBlock(Player player, ItemStack involvedItem, BlockPos target) {
        if (isServerSide(player.level())) {
            if (!involvedItem.equals(ItemStack.EMPTY) && isMarker(involvedItem)) {
                MarkerStick marker = new MarkerStick(StickUtil.getStickNBT(involvedItem));
                AreaType areaType = marker.getAreaType();
                if (areaType == null) {
                    Constants.LOGGER.warn("Unknown area type on marking - should really not happening");
                    return;
                }
                if (player.isShiftKeyDown()) {
                    marker.setTeleportPos(target);
                    StickUtil.setMarkerNbt(involvedItem, marker.serializeNBT());
                    StickUtil.updateStickName(involvedItem);
                    return;
                }
                // add block to NBT list
                marker.addMarkedBlock(target);
                // check whether marked blocks form a valid marked area
                marker.checkValidArea();
                StickUtil.setMarkerNbt(involvedItem, marker.serializeNBT());
                StickUtil.updateStickName(involvedItem);
            }
        }
    }

    public static void onCycleMode(Player player, ItemStack involvedItem, BlockHitResult target) {
        if (isServerSide(player.level())) {
            // is some valid mod stick
            if (!involvedItem.equals(ItemStack.EMPTY) && StickUtil.isMarker(involvedItem)) {
                boolean targetIsAir;
                if (target.getType() == HitResult.Type.BLOCK) { // should always be block
                    BlockPos blockpos = target.getBlockPos();
                    BlockState blockstate = player.level().getBlockState(blockpos);
                    targetIsAir = blockstate.getBlock().equals(Blocks.AIR);
                } else {
                    targetIsAir = target.getType() == HitResult.Type.MISS;
                }

                if (player.isShiftKeyDown() && targetIsAir) {
                    CompoundTag stickNBT = getStickNBT(involvedItem);
                    MarkerStick marker = new MarkerStick(stickNBT);
                    // change area nbt, reset marked blocks, set valid to false
                    marker.cycleMode();
                    // update stick name
                    StickUtil.setMarkerNbt(involvedItem, marker.serializeNBT());
                    StickUtil.updateStickName(involvedItem);
                }
            }
        }
    }
}
