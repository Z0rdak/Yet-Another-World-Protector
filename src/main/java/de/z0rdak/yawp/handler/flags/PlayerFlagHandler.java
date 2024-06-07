package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.api.events.region.RegionEvents;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.MessageSender;
import net.fabricmc.fabric.api.entity.event.v1.EntityElytraEvents;
import net.fabricmc.fabric.api.entity.event.v1.EntitySleepEvents;
import net.fabricmc.fabric.api.event.player.PlayerBlockBreakEvents;
import net.fabricmc.fabric.api.event.player.UseBlockCallback;
import net.fabricmc.fabric.api.event.player.UseEntityCallback;
import net.fabricmc.fabric.api.event.player.UseItemCallback;
import net.minecraft.block.BlockState;
import net.minecraft.block.entity.BlockEntity;
import net.minecraft.block.entity.EnderChestBlockEntity;
import net.minecraft.block.entity.LecternBlockEntity;
import net.minecraft.block.entity.LockableContainerBlockEntity;
import net.minecraft.entity.Entity;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.vehicle.StorageMinecartEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.item.Items;
import net.minecraft.util.ActionResult;
import net.minecraft.util.Hand;
import net.minecraft.util.TypedActionResult;
import net.minecraft.util.UseAction;
import net.minecraft.util.hit.BlockHitResult;
import net.minecraft.util.hit.EntityHitResult;
import net.minecraft.util.hit.HitResult;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.jetbrains.annotations.Nullable;

import static de.z0rdak.yawp.core.flag.RegionFlag.*;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;

/**
 * Contains flag handler for events directly related/cause to/by players.
 */
public final class PlayerFlagHandler {

    private PlayerFlagHandler() {
    }

    public static void registerEventHandler() {
        EntitySleepEvents.ALLOW_SLEEPING.register(PlayerFlagHandler::onAllowSleeping);
        EntitySleepEvents.ALLOW_SETTING_SPAWN.register(PlayerFlagHandler::onSettingSpawn);
        PlayerBlockBreakEvents.BEFORE.register(PlayerFlagHandler::onBreakBlock);
        EntityElytraEvents.ALLOW.register(PlayerFlagHandler::onElytraFlight);
        UseItemCallback.EVENT.register(PlayerFlagHandler::onUseItem);
        UseBlockCallback.EVENT.register(PlayerFlagHandler::onUseBlock);
        UseBlockCallback.EVENT.register(PlayerFlagHandler::onAccessContainer);
        UseEntityCallback.EVENT.register(PlayerFlagHandler::onUseEntity);
    }

    private static TypedActionResult<ItemStack> onUseItem(PlayerEntity player, World world, Hand hand) {
        if (isServerSide(world)) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(player));
            if (!hasEmptyHands(player)) {
                FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, player.getBlockPos(), USE_ENTITIES, dimCache.getDimensionalRegion());
                if (flagCheckEvent.isDenied()) {
                    sendFlagDeniedMsg(flagCheckEvent);
                    return TypedActionResult.fail(player.getStackInHand(hand));
                }
            }
            FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, player.getBlockPos(), USE_ITEMS, dimCache.getDimensionalRegion());
            if (flagCheckEvent.isDenied()) {
                sendFlagDeniedMsg(flagCheckEvent);
                return TypedActionResult.fail(player.getStackInHand(hand));
            }
        }
        return TypedActionResult.pass(player.getStackInHand(hand));
    }

    private static ActionResult onUseBlock(PlayerEntity player, World world, Hand hand, BlockHitResult blockHitResult) {
        if (isServerSide(world)) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(world.getRegistryKey());
            BlockPos targetPos = blockHitResult.getBlockPos();
            boolean hasEmptyHands = hasEmptyHands(player);
            ItemStack stackInHand = player.getStackInHand(hand);
            boolean isSneakingWithEmptyHands = player.isSneaking() && hasEmptyHands;
            boolean isBlock = blockHitResult.getType() == HitResult.Type.BLOCK;

            //  // does this include water blocks and placing lilly pads for example?
            if (isBlock) {
                // allow player to place blocks when shift clicking usable bock
                if ((isSneakingWithEmptyHands || !player.isSneaking()) || hasEmptyHands) {
                    FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, targetPos, USE_BLOCKS, dimCache.getDimensionalRegion());
                    if (flagCheckEvent.isDenied()) {
                        sendFlagDeniedMsg(flagCheckEvent);
                        return ActionResult.FAIL;
                    }
                }
            }
            if (!hasEmptyHands) {
                FlagCheckEvent.PlayerFlagEvent useItemCheck = checkPlayerEvent(player, player.getBlockPos(), USE_ITEMS, dimCache.getDimensionalRegion());
                if (useItemCheck.isDenied()) {
                    sendFlagDeniedMsg(useItemCheck);
                    return ActionResult.FAIL;
                }

                // TODO: FIXME This needs to be improved like to custom check used in forge
                boolean isBerry = stackInHand.isOf(Items.GLOW_BERRIES) || stackInHand.isOf(Items.SWEET_BERRIES);
                UseAction useAction = stackInHand.getUseAction();
                if (useAction == UseAction.NONE || (isBerry && useAction == UseAction.EAT)) {
                    // PlayerEntity attempting to place block
                    FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, blockHitResult.getBlockPos(), PLACE_BLOCKS, dimCache.getDimensionalRegion());
                    if (flagCheckEvent.isDenied()) {
                        sendFlagDeniedMsg(flagCheckEvent);
                        // sync inventory
                        player.getInventory().updateItems();
                        return ActionResult.FAIL;
                    }
                }
            }
        }
        return ActionResult.PASS;
    }

    private static ActionResult onAccessContainer(PlayerEntity player, World world, Hand hand, BlockHitResult blockHitResult) {
        if (isServerSide(world)) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(world.getRegistryKey());
            BlockPos targetPos = blockHitResult.getBlockPos();
            BlockEntity targetEntity = world.getBlockEntity(targetPos);
            boolean isLockableTileEntity = targetEntity instanceof LockableContainerBlockEntity;
            boolean isEnderChest = targetEntity instanceof EnderChestBlockEntity;
            boolean isContainer = targetEntity instanceof LecternBlockEntity || isLockableTileEntity;
            boolean hasEmptyHands = hasEmptyHands(player);
            if (blockHitResult.getType() == HitResult.Type.BLOCK) {
                // Note: following flags are already covered with use_blocks
                if (isEnderChest) {
                    // check allows player to place blocks when shift clicking container
                    if (player.isSneaking() && hasEmptyHands || !player.isSneaking()) {
                        FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, targetPos, ENDER_CHEST_ACCESS, dimCache.getDimensionalRegion());
                        if (flagCheckEvent.isDenied()) {
                            sendFlagDeniedMsg(flagCheckEvent);
                            return ActionResult.FAIL;
                        }
                    }
                }
                if (isContainer) {
                    // check allows player to place blocks when shift clicking container
                    if (player.isSneaking() && hasEmptyHands || !player.isSneaking()) {
                        FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, targetPos, CONTAINER_ACCESS, dimCache.getDimensionalRegion());
                        if (flagCheckEvent.isDenied()) {
                            sendFlagDeniedMsg(flagCheckEvent);
                            return ActionResult.FAIL;
                        }
                    }
                }
            }
        }
        return ActionResult.PASS;
    }

    private static ActionResult onUseEntity(PlayerEntity player, World world, Hand hand, Entity entity, @Nullable EntityHitResult entityHitResult) {
        if (isServerSide(world)) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(player));

            FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, entity.getBlockPos(), USE_ENTITIES, dimCache.getDimensionalRegion());
            if (flagCheckEvent.isDenied()) {
                sendFlagDeniedMsg(flagCheckEvent);
                return ActionResult.FAIL;
            }

            if (!hasEmptyHands(player)) {
                FlagCheckEvent.PlayerFlagEvent useItemCheck = checkPlayerEvent(player, entity.getBlockPos(), USE_ITEMS, dimCache.getDimensionalRegion());
                if (useItemCheck.isDenied()) {
                    sendFlagDeniedMsg(useItemCheck);
                    return ActionResult.FAIL;
                }
            }

            if (entity instanceof StorageMinecartEntity) {
                flagCheckEvent = checkPlayerEvent(player, entity.getBlockPos(), CONTAINER_ACCESS, dimCache.getDimensionalRegion());
                if (flagCheckEvent.isDenied()) {
                    sendFlagDeniedMsg(flagCheckEvent);
                    return ActionResult.FAIL;
                }
            }
            return ActionResult.PASS;
        }
        return ActionResult.PASS;
    }

    private static boolean hasEmptyHands(PlayerEntity player) {
        return player.getStackInHand(Hand.MAIN_HAND).getItem().equals(Items.AIR)
                && player.getStackInHand(Hand.OFF_HAND).getItem().equals(Items.AIR);
    }


    private static boolean onElytraFlight(LivingEntity livingEntity) {
        if (!livingEntity.getWorld().isClient) {
            if (livingEntity instanceof PlayerEntity player) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(player.getBlockPos(), BREAK_BLOCKS, player.getWorld().getRegistryKey(), player);
                if (RegionEvents.CHECK_FLAG.invoker().checkFlag(checkEvent)) {
                    return true;
                }
                FlagState flagState = processCheck(checkEvent, null, MessageSender::sendFlagMsg);
                return flagState == FlagState.DENIED;
            }
        }
        return true;
    }

    private static boolean onBreakBlock(World world, PlayerEntity player, BlockPos blockPos, BlockState blockState, BlockEntity blockEntity) {
        if (isServerSide(world)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(blockPos, BREAK_BLOCKS, player.getWorld().getRegistryKey(), player);
            if (RegionEvents.CHECK_FLAG.invoker().checkFlag(checkEvent)) {
                return true;
            }
            FlagState flagState = processCheck(checkEvent, null, MessageSender::sendFlagMsg);
            return flagState == FlagState.DENIED;
        }
        return true;
    }

    private static boolean onSettingSpawn(PlayerEntity player, BlockPos blockPos) {
        if (isServerSide(player)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(blockPos, SET_SPAWN, player.getWorld().getRegistryKey(), player);
            if (RegionEvents.CHECK_FLAG.invoker().checkFlag(checkEvent)) {
                return true;
            }
            FlagState flagState = processCheck(checkEvent, null, MessageSender::sendFlagMsg);
            return flagState == FlagState.DENIED;
        }
        return true;
    }

    private static PlayerEntity.SleepFailureReason onAllowSleeping(PlayerEntity player, BlockPos blockPos) {
        if (isServerSide(player)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(blockPos, SLEEP, player.getWorld().getRegistryKey(), player);
            if (RegionEvents.CHECK_FLAG.invoker().checkFlag(checkEvent)) {
                return null;
            }
            FlagState flagState = processCheck(checkEvent, null, MessageSender::sendFlagMsg);
            if (flagState != FlagState.DENIED) {
                return PlayerEntity.SleepFailureReason.NOT_POSSIBLE_HERE;
            }
        }
        return null;
    }
}