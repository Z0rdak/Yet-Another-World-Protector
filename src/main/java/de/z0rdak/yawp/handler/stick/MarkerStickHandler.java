package de.z0rdak.yawp.handler.stick;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.stick.MarkerStick;
import de.z0rdak.yawp.util.StickType;
import de.z0rdak.yawp.util.StickUtil;
import net.minecraft.block.BlockState;
import net.minecraft.block.Blocks;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.item.Items;
import net.minecraft.nbt.NbtCompound;
import net.minecraft.util.hit.BlockHitResult;
import net.minecraft.util.hit.HitResult;
import net.minecraft.util.math.BlockPos;

import java.util.Objects;

import static de.z0rdak.yawp.util.StickUtil.*;

public class MarkerStickHandler {

    public static void onCreateStick(PlayerEntity player, ItemStack input, ItemStack output, StickType type) {
        // split stack and only create one stick, also refund xp
        input.setCount(output.getCount() - 1);
        player.giveItemStack(input);
        player.addExperienceLevels(1);
        StickUtil.initMarkerNbt(output, type, player.getWorld().getRegistryKey());
    }

    public static void onMarkBlock(PlayerEntity player, ItemStack involvedItem, BlockPos target) {
        if (!player.getWorld().isClient) {
            // TODO: Maybe check if player is allowed to mark block
            if (!involvedItem.equals(ItemStack.EMPTY) && isVanillaStick(involvedItem)) {
                StickType stickType = getStickType(involvedItem);
                if (Objects.requireNonNull(stickType) == StickType.MARKER) {
                    NbtCompound stickTag = getStickTag(involvedItem);
                    MarkerStick marker = new MarkerStick(stickTag);
                    AreaType areaType = marker.getAreaType();
                    if (areaType == null) {
                        YetAnotherWorldProtector.LOGGER.warn("Unknown area type on marking - should really not happening");
                        return;
                    }
                    if (player.isCrawling()) {
                        marker.setTeleportPos(target);
                        
                     //   involvedItem.getNbt().put(STICK, marker.serializeNBT());
                        return;
                    }
                    // add block to NBT list
                    marker.addMarkedBlock(target);
                    // check whether marked blocks form a valid marked area
                    marker.checkValidArea();
               //     involvedItem.getNbt().put(STICK, marker.serializeNBT());
                    setStickName(involvedItem, StickType.MARKER);
                }
            }
        }
    }

    public static void onCycleMode(PlayerEntity player, ItemStack involvedItem, BlockHitResult target) {
        if (!player.getWorld().isClient) {
            // is some valid mod stick
            if (!involvedItem.equals(ItemStack.EMPTY)
                    && hasNonNullTag(involvedItem)
                    && involvedItem.isOf(Items.STICK)) {
                boolean targetIsAir;
                if (target.getType() == HitResult.Type.BLOCK) { // should always be block
                    BlockPos blockpos = target.getBlockPos();
                    BlockState blockstate = player.getWorld().getBlockState(blockpos);
                    targetIsAir = blockstate.getBlock().equals(Blocks.AIR);
                } else {
                    targetIsAir = target.getType() == HitResult.Type.MISS;
                }

                if (player.isCrawling() && targetIsAir) {
                    /* // TODO: Alpha1 - RegionMarker disabled
                    StickType stickType = getStickType(involvedItem);
                    if (Objects.requireNonNull(stickType) == StickType.MARKER) {
                        NbtCompound nbt = involvedItem.getNbt();
                        MarkerStick marker = new MarkerStick(nbt.getCompound(STICK));
                        // change area nbt, reset marked blocks, set valid to false
                        marker.cycleMode();
                        // update stick name
                        involvedItem.getNbt().put(STICK, marker.serializeNBT());
                        StickUtil.setStickName(involvedItem, StickType.MARKER);
                    }                    
                     */
                }
            }
        }
    }
}
