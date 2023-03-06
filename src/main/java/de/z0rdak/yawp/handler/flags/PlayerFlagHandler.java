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
import net.minecraft.entity.Entity;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.util.ActionResult;
import net.minecraft.util.Hand;
import net.minecraft.util.TypedActionResult;
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
        UseEntityCallback.EVENT.register(PlayerFlagHandler::onUseEntity);
    }


    // TODO: Use Items
    // TODO: place fluid / use bucket
    private static TypedActionResult<ItemStack> onUseItem(PlayerEntity player, World world, Hand hand) {
        if (isServerSide(world)) {
            return TypedActionResult.success(player.getStackInHand(hand));
        }
        return TypedActionResult.success(player.getStackInHand(hand));
    }

    // TODO: UseBlocks
    // TODO: scoop fluid
    private static ActionResult onUseBlock(PlayerEntity player, World world, Hand hand, BlockHitResult blockHitResult) {
        if (isServerSide(world)) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(world.getRegistryKey());

            if (blockHitResult.getType() == HitResult.Type.BLOCK && !player.getStackInHand(hand).isEmpty()) {
                // Player attempting to place block
                FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, blockHitResult.getBlockPos(), PLACE_BLOCKS, dimCache.getDimensionalRegion());
                handleAndSendMsg(flagCheckEvent);
                if (flagCheckEvent.isDenied()) {
                    sendFlagDeniedMsg(flagCheckEvent);
                    return ActionResult.PASS;
                }
            }
        }
        return ActionResult.SUCCESS;
    }

    // TODO: use entity
    private static ActionResult onUseEntity(PlayerEntity playerEntity, World world, Hand hand, Entity entity, @Nullable EntityHitResult entityHitResult) {
        if (isServerSide(world)) {
            return ActionResult.SUCCESS;
        }
        return ActionResult.SUCCESS;
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