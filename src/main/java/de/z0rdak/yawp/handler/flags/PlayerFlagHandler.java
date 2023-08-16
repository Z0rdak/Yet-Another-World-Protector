package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
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
        // ServerEntityEvents.EQUIPMENT_CHANGE
        // EntitySleepEvents.ALLOW_BED
        // EntitySleepEvents.ALLOW_SLEEP_TIME
        // EntitySleepEvents.ALLOW_RESETTING_TIME
        // EntitySleepEvents.ALLOW_NEARBY_MONSTERS.register(PlayerFlagHandler::onSleepingWithStrangers);
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
            boolean hasStackInHand = !player.getStackInHand(hand).isEmpty();
            boolean isSneakingWithEmptyHands = player.isSneaking() && hasEmptyHands;

            //  // does this include water blocks and placing lilly pads for example?
            if (blockHitResult.getType() == HitResult.Type.BLOCK) {
                // allow player to place blocks when shift clicking usable bock
                if ((isSneakingWithEmptyHands || !player.isSneaking()) || !hasStackInHand) {
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
            }

            if (hasStackInHand && player.getStackInHand(hand).getUseAction() == UseAction.NONE) {
                // Player attempting to place block
                FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, blockHitResult.getBlockPos(), PLACE_BLOCKS, dimCache.getDimensionalRegion());
                if (flagCheckEvent.isDenied()) {
                    sendFlagDeniedMsg(flagCheckEvent);
                    // sync inventory
                    player.getInventory().updateItems();
                    return ActionResult.FAIL;
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
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(HandlerUtil.getEntityDim(player));
                FlagCheckEvent.PlayerFlagEvent flagCheckEvent = HandlerUtil.checkPlayerEvent(player, player.getBlockPos(), USE_ELYTRA, dimCache.getDimensionalRegion());
                handleAndSendMsg(flagCheckEvent);
                return !flagCheckEvent.isDenied();
            }
        }
        return true;
    }

    private static boolean onBreakBlock(World world, PlayerEntity player, BlockPos blockPos, BlockState blockState, BlockEntity blockEntity) {
        if (isServerSide(world)) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(world.getRegistryKey());
            FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, blockPos, BREAK_BLOCKS, dimCache.getDimensionalRegion());
            handleAndSendMsg(flagCheckEvent);
            return !flagCheckEvent.isDenied();
        }
        return true;
    }

    private static boolean onSettingSpawn(PlayerEntity player, BlockPos blockPos) {
        if (isServerSide(player)) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(player));
            FlagCheckEvent.PlayerFlagEvent flagCheck = checkPlayerEvent(player, blockPos, RegionFlag.SET_SPAWN, dimCache.getDimensionalRegion());
            if (flagCheck.isDenied()) {
                sendFlagDeniedMsg(flagCheck);
                return false;
            }
        }
        return true;
    }

    private static PlayerEntity.SleepFailureReason onAllowSleeping(PlayerEntity player, BlockPos blockPos) {
        if (isServerSide(player)) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(player));
            FlagCheckEvent.PlayerFlagEvent flagCheck = checkPlayerEvent(player, blockPos, RegionFlag.SLEEP, dimCache.getDimensionalRegion());
            if (flagCheck.isDenied()) {
                sendFlagDeniedMsg(flagCheck);
                return PlayerEntity.SleepFailureReason.NOT_POSSIBLE_HERE;
            }
        }
        return null;
    }
}