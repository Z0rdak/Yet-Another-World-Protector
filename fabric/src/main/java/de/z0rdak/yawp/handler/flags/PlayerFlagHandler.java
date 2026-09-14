package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.api.FlagEvaluator;
import de.z0rdak.yawp.api.FlagRegister;
import de.z0rdak.yawp.api.events.flag.FlagCheckRequest;
import de.z0rdak.yawp.config.server.FlagConfig;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.platform.Services;
import de.z0rdak.yawp.api.MessageSender;
import net.fabricmc.fabric.api.entity.event.v1.EntitySleepEvents;
import net.fabricmc.fabric.api.event.player.AttackBlockCallback;
import net.fabricmc.fabric.api.event.player.UseBlockCallback;
import net.fabricmc.fabric.api.event.player.UseEntityCallback;
import net.fabricmc.fabric.api.event.player.UseItemCallback;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.references.BlockItemId;
import net.minecraft.references.BlockItemIds;
import net.minecraft.resources.Identifier;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.MenuProvider;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.HasCustomInventoryScreen;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.entity.vehicle.ContainerEntity;
import net.minecraft.world.item.*;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.item.context.UseOnContext;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.block.entity.BaseContainerBlockEntity;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.entity.EnderChestBlockEntity;
import net.minecraft.world.level.block.entity.LecternBlockEntity;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraft.world.phys.EntityHitResult;
import net.minecraft.world.phys.Vec3;
import org.jetbrains.annotations.Nullable;

import java.util.HashSet;
import java.util.Set;

import static de.z0rdak.yawp.handler.HandlerUtil.*;

/**
 * Contains flag handler for events directly related/cause to/by players.
 */
public final class PlayerFlagHandler {

    public static final boolean ALLOW = true;

    private PlayerFlagHandler() {
    }

    public static void register() {
        EntitySleepEvents.ALLOW_SLEEPING.register(PlayerFlagHandler::onAllowSleeping);
        EntitySleepEvents.ALLOW_SETTING_SPAWN.register(PlayerFlagHandler::onSettingSpawn);
        UseItemCallback.EVENT.register(PlayerFlagHandler::onUseItem);
        UseBlockCallback.EVENT.register(PlayerFlagHandler::onUseBlock);
        UseEntityCallback.EVENT.register(PlayerFlagHandler::onUseEntity);
        AttackBlockCallback.EVENT.register(PlayerFlagHandler::onAttackBlock);
    }

    /**
     * This event is fired before the player triggers {@link Item#use(Level, Player, InteractionHand)}.
     * Note that this is NOT fired if the player is targeting a block {@link UseBlockCallback} or entity {@link UseEntityCallback}.
     */
    private static InteractionResult onUseItem(Player player, Level world, InteractionHand hand) {
        /* Vanilla code - START
        This is in place to ensure same behavior of flags across fabric and forge - check this on each update! */
        ItemStack stackInHand = player.getItemInHand(hand);
        if (player.isSpectator() || player.getCooldowns().isOnCooldown(stackInHand)) {
            return InteractionResult.PASS;
        }
        /* Vanilla code - END */
        if (isServerSide(world)) {
            FlagCheckRequest checkEvent = new FlagCheckRequest(player.blockPosition(), FlagRegister.PLAYER_USE_ITEMS, getDimKey(player), player);
            if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent)) {
                return InteractionResult.PASS;
            }
            FlagState flagState = FlagEvaluator.processCheck(checkEvent, MessageSender::sendFlagMsg);
            if (flagState == FlagState.DENIED) {
                return InteractionResult.FAIL;
            }
        }
        return InteractionResult.PASS;
    }


    /**
     * This event is fired whenever the player right clicks while targeting a block. <br>
     * This event controls which of
     * {@link net.minecraft.world.level.block.state.BlockState#useItemOn(ItemStack, Level, Player, InteractionHand, BlockHitResult)}, and  <br>
     * {@link ItemStack#useOn(UseOnContext)}  <br>
     * will be called. <br>
     * Canceling the event will cause none of the above to be called. <br>
     * <br>
     * Let result be the first non-pass return value of the above methods, or pass, if they all pass. <br>
     * If result equals {@link InteractionResult#PASS}, we proceed to {@link UseItemCallback}.  <br>
     */
    private static InteractionResult onUseBlock(Player player, Level world, InteractionHand hand, BlockHitResult blockHitResult) {
        if (isServerSide(world)) {
            UseOnContext useOnContext = new UseOnContext(player, hand, blockHitResult);
            BlockPos targetPos = useOnContext.getClickedPos();
            BlockEntity targetEntity = world.getBlockEntity(targetPos);
            boolean hasEmptyHand = hasEmptyHand(player, hand);

            boolean isSneakingWithEmptyHands = player.isShiftKeyDown() && hasEmptyHand;
            boolean isLockableTileEntity = targetEntity instanceof BaseContainerBlockEntity;
            boolean isEnderChest = targetEntity instanceof EnderChestBlockEntity;
            boolean isContainer = targetEntity instanceof LecternBlockEntity || isLockableTileEntity;

        
            Identifier targetBlockId = BuiltInRegistries.BLOCK.getKey(targetEntity.getBlockState().getBlock());
            // TODO check if player ia allowed to use this specific block

            // allow player to place blocks when shift clicking usable block
            if ((isSneakingWithEmptyHands || !player.isShiftKeyDown())) {
                FlagCheckRequest checkEvent = new FlagCheckRequest(targetPos, FlagRegister.PLAYER_USE_BLOCKS, getDimKey(player), player);
                if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent))
                    return InteractionResult.PASS;
                FlagState flagState = FlagEvaluator.processCheck(checkEvent, MessageSender::sendFlagMsg);
                if (flagState == FlagState.DENIED)
                    return InteractionResult.FAIL;

                var protectedEntrySet = new HashSet<Identifier>();
                if (isEnderChest) {
                    Identifier protectedEntry = BlockItemIds.ENDER_CHEST.block().identifier();
                    protectedEntrySet.add(protectedEntry);

                    // check allows player to place blocks when shift clicking container
                    if (player.isShiftKeyDown() && hasEmptyHand || !player.isShiftKeyDown()) {
                        checkEvent = new FlagCheckRequest(targetPos, FlagRegister.PLAYER_OPEN_ENDER_CHEST, getDimKey(player), player);
                        if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent))
                            return InteractionResult.PASS;
                        flagState = FlagEvaluator.processCheck(checkEvent, MessageSender::sendFlagMsg);
                        if (flagState == FlagState.DENIED)
                            return InteractionResult.FAIL;
                    }
                }

                // TODO: default protected sets register
                //  FLAG -> { name: string, set: Set<Identifier> }
                //  player/use_blocks -> { name: container, ["minecraft:chest", ...] }
                var containerEntrySet = new HashSet<Identifier>();
                Identifier lectern = BlockItemIds.LECTERN.block().identifier();
                Identifier chest = BlockItemIds.CHEST.block().identifier();
                Identifier barrel = BlockItemIds.BARREL.block().identifier();
                Identifier brewingStand = BlockItemIds.BREWING_STAND.block().identifier();

                containerEntrySet.add(lectern);
                containerEntrySet.add(chest);
                containerEntrySet.add(barrel);
                containerEntrySet.add(brewingStand);

                if (isContainer || containerEntrySet.contains(targetBlockId)) {
                    // check allows player to place blocks when shift clicking container
                    if (player.isShiftKeyDown() && hasEmptyHand || !player.isShiftKeyDown()) {
                        checkEvent = new FlagCheckRequest(targetPos, FlagRegister.PLAYER_OPEN_CONTAINER, getDimKey(player), player);
                        if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent))
                            return InteractionResult.PASS;
                        flagState = FlagEvaluator.processCheck(checkEvent, MessageSender::sendFlagMsg);
                        if (flagState == FlagState.DENIED)
                            return InteractionResult.FAIL;
                    }
                }
            }

            if (!hasEmptyHand) {
                Identifier id = BuiltInRegistries.ITEM.getKey(useOnContext.getItemInHand().getItem());
                // TODO check if item in hand is allowed to be used
                FlagCheckRequest checkEvent = new FlagCheckRequest(targetPos, FlagRegister.PLAYER_USE_ITEMS, getDimKey(player), player);
                if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent))
                    return InteractionResult.PASS;
                FlagState flagState = FlagEvaluator.processCheck(checkEvent, MessageSender::sendFlagMsg);
                if (flagState == FlagState.DENIED)
                    return InteractionResult.FAIL;
            }
        }
        player.getInventory().setChanged();
        return InteractionResult.PASS;
    }

    /**
     * This event is fired on both sides when the player right-clicks an entity.
     * It is responsible for all general entity interactions.
     * This event's state affects whether {@link Entity#interact(Player, InteractionHand, Vec3)} and
     * {@link Item#interactLivingEntity(ItemStack, Player, LivingEntity, InteractionHand)} are called.
     * Let result be {@link InteractionResult#SUCCESS} if {@link Entity#interact(Player, InteractionHand, Vec3)} or
     * {@link Item#interactLivingEntity(ItemStack, Player, LivingEntity, InteractionHand)} return true,
     * or FAIL if the event is cancelled.
     */
    private static InteractionResult onUseEntity(Player player, Level world, InteractionHand hand, Entity entity, @Nullable EntityHitResult entityHitResult) {
        /* Vanilla code - START
        This is in place to ensure same behavior of flags across fabric and forge - check this on each update! */
        if (player.isSpectator()) {
            if (entity instanceof MenuProvider) {
                player.openMenu((MenuProvider) entity);
            }
            return InteractionResult.PASS;
        }
        /* Vanilla code - END */

        if (entityHitResult != null) {
            // present entity hit result acts the same as EntityInteractSpecificEvent in Forge - only used for ArmorStands in Vanilla
            InteractionResult actionResult = onUseEntitySpecific(player, world, hand, entity, entityHitResult);
            if (actionResult == InteractionResult.SUCCESS) {
                return actionResult;
            }
        }

        if (isServerSide(world)) {
            FlagCheckRequest checkEvent = new FlagCheckRequest(entity.blockPosition(), FlagRegister.PLAYER_INTERACT, getDimKey(player), player);
            if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent))
                return InteractionResult.PASS;
            FlagState flagState = FlagEvaluator.processCheck(checkEvent, MessageSender::sendFlagMsg);
            if (flagState == FlagState.DENIED)
                return InteractionResult.FAIL;

            if (!hasEmptyHand(player, hand)) {
                checkEvent = new FlagCheckRequest(entity.blockPosition(), FlagRegister.PLAYER_USE_ITEMS, getDimKey(player), player);
                if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent))
                    return InteractionResult.PASS;
                flagState = FlagEvaluator.processCheck(checkEvent, MessageSender::sendFlagMsg);
                if (flagState == FlagState.DENIED)
                    return InteractionResult.FAIL;
            }
            if (entity instanceof ContainerEntity || entity instanceof HasCustomInventoryScreen) {
                checkEvent = new FlagCheckRequest(player.blockPosition(), FlagRegister.PLAYER_OPEN_CONTAINER, getDimKey(player), player);
                if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent))
                    return InteractionResult.PASS;
                flagState = FlagEvaluator.processCheck(checkEvent, MessageSender::sendFlagMsg);
                if (flagState == FlagState.DENIED)
                    return InteractionResult.FAIL;
            }
        }
        return InteractionResult.PASS;
    }

    private static InteractionResult onUseEntitySpecific(Player player, Level world, InteractionHand hand, Entity entity, EntityHitResult entityHitResult) {
        return InteractionResult.PASS;
    }

    private static InteractionResult onAttackBlock(Player player, Level world, InteractionHand hand, BlockPos blockPos, Direction direction) {
        if (isServerSide(world)) {
            FlagCheckRequest checkEvent = new FlagCheckRequest(blockPos, FlagRegister.PLAYER_BREAK_BLOCKS, getDimKey(player), player);
            if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent)) {
                return InteractionResult.PASS;
            }
            FlagState flagState = FlagEvaluator.processCheck(checkEvent, MessageSender::sendFlagMsg);
            return flagState == FlagState.DENIED ? InteractionResult.FAIL : InteractionResult.PASS;
        }
        return InteractionResult.PASS;
    }

    private static boolean onSettingSpawn(Player player, BlockPos blockPos) {
        if (isServerSide(player)) {
            FlagCheckRequest checkEvent = new FlagCheckRequest(blockPos, FlagRegister.PLAYER_SET_SPAWN, getDimKey(player), player);
            if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent)) {
                return ALLOW;
            }
            FlagState flagState = FlagEvaluator.processCheck(checkEvent, MessageSender::sendFlagMsg);
            return flagState != FlagState.DENIED;
        }
        return ALLOW;
    }

    private static Player.BedSleepingProblem onAllowSleeping(Player player, BlockPos blockPos) {
        if (isServerSide(player)) {
            FlagCheckRequest checkEvent = new FlagCheckRequest(blockPos, FlagRegister.PLAYER_SLEEP, getDimKey(player), player);
            if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent)) {
                return null;
            }
            FlagState flagState = FlagEvaluator.processCheck(checkEvent, MessageSender::sendFlagMsg);
            if (flagState == FlagState.DENIED) {
                return Player.BedSleepingProblem.OTHER_PROBLEM;
            }
        }
        return null;
    }

    private static boolean hasEmptyHand(Player player, InteractionHand hand) {
        return player.getItemInHand(hand).getItem().equals(Items.AIR);
    }
}
