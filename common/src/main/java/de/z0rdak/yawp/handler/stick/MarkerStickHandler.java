package de.z0rdak.yawp.handler.stick;

import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.constants.serialization.ItemNbtKeys;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.stick.MarkerStick;
import de.z0rdak.yawp.util.StickType;
import de.z0rdak.yawp.util.StickUtil;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraft.world.phys.HitResult;

import java.util.Objects;

import static de.z0rdak.yawp.handler.flags.HandlerUtil.isServerSide;
import static de.z0rdak.yawp.util.StickUtil.*;

public class MarkerStickHandler {

    public static void onCreateStick(Player player, ItemStack input, ItemStack output, StickType type) {
        // split stack and only create one stick, also refund xp
        input.setCount(output.getCount() - 1);
        player.addItem(input);
        player.giveExperienceLevels(1);
        StickUtil.initMarkerNbt(output, type, player.level().dimension());
    }

    public static void onMarkBlock(Player player, ItemStack involvedItem, BlockPos target) {
        if (isServerSide(player.level())) {
            // TODO: Maybe check if player is allowed to mark block
            if (!involvedItem.equals(ItemStack.EMPTY) && isVanillaStick(involvedItem)) {
                StickType stickType = getStickType(involvedItem);
                if (Objects.requireNonNull(stickType) == StickType.MARKER) {
                    MarkerStick marker = new MarkerStick(involvedItem.getTag().getCompound(ItemNbtKeys.STICK));
                    AreaType areaType = marker.getAreaType();
                    if (areaType == null) {
                        Constants.LOGGER.warn("Unknown area type on marking - should really not happening");
                        return;
                    }
                    if (player.isShiftKeyDown()) {
                        marker.setTeleportPos(target);
                        involvedItem.getTag().put(ItemNbtKeys.STICK, marker.serializeNBT());
                        return;
                    }
                    // add block to NBT list
                    marker.addMarkedBlock(target);
                    // check whether marked blocks form a valid marked area
                    marker.checkValidArea();
                    involvedItem.getTag().put(ItemNbtKeys.STICK, marker.serializeNBT());
                    setStickName(involvedItem, StickType.MARKER);
                }
            }
        }
    }

    public static void onCycleMode(Player player, ItemStack involvedItem, BlockHitResult target) {
        if (isServerSide(player.level())) {
            // is some valid mod stick
            if (!involvedItem.equals(ItemStack.EMPTY)
                    && hasNonNullTag(involvedItem)
                    && involvedItem.getTag().contains(ItemNbtKeys.STICK)) {
                boolean targetIsAir;
                if (target.getType() == HitResult.Type.BLOCK) { // should always be block
                    BlockPos blockpos = target.getBlockPos();
                    BlockState blockstate = player.level().getBlockState(blockpos);
                    targetIsAir = blockstate.getBlock().equals(Blocks.AIR);
                } else {
                    targetIsAir = target.getType() == HitResult.Type.MISS;
                }

                if (player.isShiftKeyDown() && targetIsAir) {
                    StickType stickType = getStickType(involvedItem);
                    if (Objects.requireNonNull(stickType) == StickType.MARKER) {
                        CompoundTag nbt = involvedItem.getTag();
                        MarkerStick marker = new MarkerStick(nbt.getCompound(ItemNbtKeys.STICK));
                        // change area nbt, reset marked blocks, set valid to false
                        marker.cycleMode();
                        // update stick name
                        involvedItem.getTag().put(ItemNbtKeys.STICK, marker.serializeNBT());
                        StickUtil.setStickName(involvedItem, StickType.MARKER);
                    }
                }
            }
        }
    }
}
