package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.api.events.region.RegionEvents;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.MessageSender;
import net.fabricmc.fabric.api.entity.event.v1.EntityElytraEvents;
import net.fabricmc.fabric.api.entity.event.v1.EntitySleepEvents;
import net.fabricmc.fabric.api.event.player.*;
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
import net.minecraft.item.ItemUsageContext;
import net.minecraft.item.Items;
import net.minecraft.network.packet.c2s.play.PlayerInteractBlockC2SPacket;
import net.minecraft.screen.NamedScreenHandlerFactory;
import net.minecraft.util.ActionResult;
import net.minecraft.util.Hand;
import net.minecraft.util.TypedActionResult;
import net.minecraft.util.UseAction;
import net.minecraft.util.hit.BlockHitResult;
import net.minecraft.util.hit.EntityHitResult;
import net.minecraft.util.hit.HitResult;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Direction;
import net.minecraft.world.GameMode;
import net.minecraft.world.World;
import org.apache.commons.lang3.NotImplementedException;
import org.jetbrains.annotations.Nullable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.nio.channels.NetworkChannel;

import static de.z0rdak.yawp.api.events.region.RegionEvents.post;
import static de.z0rdak.yawp.core.flag.RegionFlag.*;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;
import static de.z0rdak.yawp.util.MessageSender.*;

/**
 * Contains flag handler for events directly related/cause to/by players.
 */
public final class PlayerFlagHandler {

    private static final Logger log = LoggerFactory.getLogger(PlayerFlagHandler.class);

    private PlayerFlagHandler() {
    }

    public static void registerEventHandler() {
        EntitySleepEvents.ALLOW_SLEEPING.register(PlayerFlagHandler::onAllowSleeping);
        EntitySleepEvents.ALLOW_SETTING_SPAWN.register(PlayerFlagHandler::onSettingSpawn);
        EntityElytraEvents.ALLOW.register(PlayerFlagHandler::onElytraFlight);

        UseItemCallback.EVENT.register(PlayerFlagHandler::onUseItem);
        UseBlockCallback.EVENT.register(PlayerFlagHandler::onUseBlock);
        UseEntityCallback.EVENT.register(PlayerFlagHandler::onUseEntity);
        
        
        PlayerBlockBreakEvents.BEFORE.register(PlayerFlagHandler::onBreakBlock);
        AttackBlockCallback.EVENT.register(PlayerFlagHandler::onBeginBreakBlock);
    }

    private static boolean onBreakBlock(World world, PlayerEntity playerEntity, BlockPos blockPos, BlockState blockState, BlockEntity blockEntity) {
        if (isServerSide(world)) {
            YetAnotherWorldProtector.LOGGER.info("onBreakBlock");
        }
        return true;
    }


    /**
     * This event is fired before the player triggers {@link net.minecraft.item.Item#use(World, PlayerEntity, Hand)}.
     * Note that this is NOT fired if the player is targeting a block {@link UseBlockCallback} or entity {@link UseEntityCallback}.
     */
    private static TypedActionResult<ItemStack> onUseItem(PlayerEntity player, World world, Hand hand) {
        /* Vanilla code - START 
        This is in place to ensure same behaviour of flags across fabric and forge - check this on each update! */
        ItemStack stackInHand = player.getStackInHand(hand);
        if (player.isSpectator() || player.getItemCooldownManager().isCoolingDown(stackInHand.getItem())) {
            return TypedActionResult.pass(stackInHand);
        }
        /* Vanilla code - END */
        if (isServerSide(world)) {
            YetAnotherWorldProtector.LOGGER.info("onUseItem");
            FlagCheckEvent checkEvent = new FlagCheckEvent(player.getBlockPos(), USE_ITEMS, getDimKey(player), player);
            if (post(checkEvent)) {
                return TypedActionResult.pass(stackInHand);
            }
            FlagState flagState = processCheck(checkEvent, null, MessageSender::sendFlagMsg);
            if (flagState == FlagState.DENIED) {
                return TypedActionResult.fail(stackInHand);
            }          
        }
        return TypedActionResult.pass(stackInHand);
    }


    /**
     * This event is fired whenever the player right clicks while targeting a block. <br>
     * This event controls which of 
     * {@link net.minecraft.item.Item#onItemUseFirst}, <br>
     * {@link net.minecraft.block.AbstractBlockState#onUse(BlockState, World, BlockPos, PlayerEntity, Hand, BlockHitResult)}, and  <br>
     * {@link net.minecraft.item.Item#useOnBlock(ItemUsageContext)}  <br>
     * will be called. <br>
     * Canceling the event will cause none of the above three to be called. <br>
     * <br>
     * Let result be the first non-pass return value of the above three methods, or pass, if they all pass. <br>
     * If result equals {@link ActionResult#PASS}, we proceed to {@link RightClickItem}.  <br>
     */
    private static ActionResult onUseBlock(PlayerEntity player, World world, Hand hand, BlockHitResult blockHitResult) {
        if (isServerSide(world)) {
            YetAnotherWorldProtector.LOGGER.info("onUseBlock");
            BlockPos targetPos = blockHitResult.getBlockPos();
            boolean hasEmptyHands = hasEmptyHands(player);
            ItemStack stackInHand = player.getStackInHand(hand);
            boolean isSneakingWithEmptyHands = player.isSneaking() && hasEmptyHands;
            BlockEntity targetEntity = world.getBlockEntity(targetPos);
            boolean isLockableTileEntity = targetEntity instanceof LockableContainerBlockEntity;
            boolean isEnderChest = targetEntity instanceof EnderChestBlockEntity;
            boolean isContainer = targetEntity instanceof LecternBlockEntity || isLockableTileEntity;

            RegionFlag flag = isEnderChest ? ENDER_CHEST_ACCESS : isContainer ? CONTAINER_ACCESS : USE_BLOCKS;
            
            if (isEnderChest) {
                // check allows player to place blocks when shift clicking container
                if (player.isSneaking() && hasEmptyHands || !player.isSneaking()) {
                    FlagCheckEvent checkEvent = new FlagCheckEvent(targetPos, flag, getDimKey(player), player);
                    if (post(checkEvent))
                        return ActionResult.PASS;
                    FlagState flagState = processCheck(checkEvent, null, MessageSender::sendFlagMsg);
                    if (flagState == FlagState.DENIED)
                        return ActionResult.FAIL;
                }
            }
            if (isContainer) {
                // check allows player to place blocks when shift clicking container
                if (player.isSneaking() && hasEmptyHands || !player.isSneaking()) {
                    FlagCheckEvent checkEvent = new FlagCheckEvent(targetPos, flag, getDimKey(player), player);
                    if (post(checkEvent))
                        return ActionResult.PASS;
                    FlagState flagState = processCheck(checkEvent, null, MessageSender::sendFlagMsg);
                    if (flagState == FlagState.DENIED)
                        return ActionResult.FAIL;
                }
            }
            
            // allow player to place blocks when shift clicking usable bock
            if ((isSneakingWithEmptyHands || !player.isSneaking()) || hasEmptyHands) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(targetPos, USE_BLOCKS, getDimKey(player), player);
                if (post(checkEvent))
                    return ActionResult.PASS;
                FlagState flagState = processCheck(checkEvent, null, MessageSender::sendFlagMsg);
                if (flagState == FlagState.DENIED)
                    return ActionResult.FAIL;
            }
            
            if (!hasEmptyHands) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(targetPos, USE_ITEMS, getDimKey(player), player);
                if (post(checkEvent))
                    return ActionResult.PASS;
                FlagState flagState = processCheck(checkEvent, null, MessageSender::sendFlagMsg);
                if (flagState == FlagState.DENIED)
                    return ActionResult.FAIL;
                
                // TODO: FIXME This needs to be improved like to custom check used in forge
                boolean isBerry = stackInHand.isOf(Items.GLOW_BERRIES) || stackInHand.isOf(Items.SWEET_BERRIES);
                UseAction useAction = stackInHand.getUseAction();
                if (useAction == UseAction.NONE || (isBerry && useAction == UseAction.EAT)) {
                    checkEvent = new FlagCheckEvent(targetPos, PLACE_BLOCKS, getDimKey(player), player);
                    if (post(checkEvent))
                        return ActionResult.PASS;
                    flagState = processCheck(checkEvent, null, MessageSender::sendFlagMsg);
                    if (flagState == FlagState.DENIED)
                        return ActionResult.FAIL;
                    
                    player.getInventory().updateItems();
                }
            }
        }
        return ActionResult.PASS;
    }
    
    /**
     * This event is fired on both sides when the player right clicks an entity.
     * It is responsible for all general entity interactions.
     *
     * This event is fired only if the result of the above {@link EntityInteractSpecific} is not {@link InteractionResult#SUCCESS}.
     * This event's state affects whether {@link Entity#interact(Player, InteractionHand)} and
     * {@link Item#interactLivingEntity(ItemStack, Player, LivingEntity, InteractionHand)} are called.
     *
     * Let result be {@link InteractionResult#SUCCESS} if {@link Entity#interact(Player, InteractionHand)} or
     * {@link Item#interactLivingEntity(ItemStack, Player, LivingEntity, InteractionHand)} return true,
     * or {@link #cancellationResult} if the event is cancelled.
     * If we are on the client and result is not {@link InteractionResult#SUCCESS}, the client will then try {@link RightClickItem}.
     */
    private static ActionResult onUseEntity(PlayerEntity player, World world, Hand hand, Entity entity, @Nullable EntityHitResult entityHitResult) {
        /* Vanilla code - START 
        This is in place to ensure same behaviour of flags across fabric and forge - check this on each update! */
        if (player.isSpectator()) {
            if (entity instanceof NamedScreenHandlerFactory) {
                player.openHandledScreen((NamedScreenHandlerFactory) entity);
            }
            return ActionResult.PASS;
        }
        /* Vanilla code - END */

        if (entityHitResult != null) {
            // Same as EntityInteractSpecific in Forge - only used for ArmorStands in Vanilla
            ActionResult actionResult = onUseEntitySpecific(player, world, hand, entity, entityHitResult);
            if (actionResult == ActionResult.SUCCESS) {
                return actionResult;
            }
        }
        
        if (isServerSide(world)) {
            YetAnotherWorldProtector.LOGGER.info("onUseEntity");
            FlagCheckEvent checkEvent = new FlagCheckEvent(entity.getBlockPos(), USE_ENTITIES, getDimKey(player), player);
            if (post(checkEvent))
                return ActionResult.PASS;
            FlagState flagState = processCheck(checkEvent, null, MessageSender::sendFlagMsg);
            if (flagState == FlagState.DENIED)
                return ActionResult.FAIL;


            if (!hasEmptyHands(player)) {
                checkEvent = new FlagCheckEvent(entity.getBlockPos(), USE_ITEMS, getDimKey(player), player);
                if (post(checkEvent))
                    return ActionResult.PASS;
                flagState = processCheck(checkEvent, null, MessageSender::sendFlagMsg);
                if (flagState == FlagState.DENIED)
                    return ActionResult.FAIL;
            }

            if (entity instanceof StorageMinecartEntity) {
                checkEvent = new FlagCheckEvent(player.getBlockPos(), CONTAINER_ACCESS, getDimKey(player), player);
                if (post(checkEvent)) 
                    return ActionResult.PASS;
                flagState = processCheck(checkEvent, null, MessageSender::sendFlagMsg);
                if (flagState == FlagState.DENIED)
                    return ActionResult.FAIL;
            }
            return ActionResult.PASS;
        }
        return ActionResult.PASS;
    }

    private static ActionResult onUseEntitySpecific(PlayerEntity player, World world, Hand hand, Entity entity, EntityHitResult entityHitResult) {
        if (isServerSide(world)) {
            YetAnotherWorldProtector.LOGGER.info("onUseEntitySpecific");
        }
        return ActionResult.PASS;
    }

    private static ActionResult onBeginBreakBlock(PlayerEntity player, World world, Hand hand, BlockPos blockPos, Direction direction) {
        if (isServerSide(world)) {
            YetAnotherWorldProtector.LOGGER.info("onAttackBlock");
            FlagCheckEvent checkEvent = new FlagCheckEvent(blockPos, BREAK_BLOCKS, getDimKey(player), player);
            if (post(checkEvent)) {
                return ActionResult.PASS;
            }
            FlagState flagState = processCheck(checkEvent, null, MessageSender::sendFlagMsg);
            return flagState == FlagState.DENIED ? ActionResult.FAIL : ActionResult.PASS;
        }
        return ActionResult.PASS;
    }

    private static boolean onSettingSpawn(PlayerEntity player, BlockPos blockPos) {
        if (isServerSide(player)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(blockPos, SET_SPAWN, getDimKey(player), player);
            if (post(checkEvent)) {
                return true;
            }
            FlagState flagState = processCheck(checkEvent, null, MessageSender::sendFlagMsg);
            return flagState == FlagState.DENIED;
        }
        return true;
    }

    private static PlayerEntity.SleepFailureReason onAllowSleeping(PlayerEntity player, BlockPos blockPos) {
        if (isServerSide(player)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(blockPos, SLEEP, getDimKey(player), player);
            if (post(checkEvent)) {
                return null;
            }
            FlagState flagState = processCheck(checkEvent, null, MessageSender::sendFlagMsg);
            if (flagState != FlagState.DENIED) {
                return PlayerEntity.SleepFailureReason.NOT_POSSIBLE_HERE;
            }
        }
        return null;
    }

    private static boolean onElytraFlight(LivingEntity livingEntity) {
        if (isServerSide(livingEntity.getWorld())) {
            if (livingEntity instanceof PlayerEntity player) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(player.getBlockPos(), USE_ELYTRA, getDimKey(player), player);
                if (post(checkEvent)) {
                    return true;
                }
                FlagState flagState = processCheck(checkEvent, null, MessageSender::sendFlagMsg);
                return flagState == FlagState.DENIED;
            }
        }
        return true;
    }
    
    private static boolean hasEmptyHands(PlayerEntity player) {
        return player.getStackInHand(Hand.MAIN_HAND).getItem().equals(Items.AIR)
                && player.getStackInHand(Hand.OFF_HAND).getItem().equals(Items.AIR);
    }
}