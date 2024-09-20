package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.config.server.FlagConfig;
import de.z0rdak.yawp.config.server.LoggingConfig;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.util.MessageSender;
import net.fabricmc.fabric.api.entity.event.v1.EntityElytraEvents;
import net.fabricmc.fabric.api.entity.event.v1.EntitySleepEvents;
import net.fabricmc.fabric.api.event.player.AttackBlockCallback;
import net.fabricmc.fabric.api.event.player.UseBlockCallback;
import net.fabricmc.fabric.api.event.player.UseEntityCallback;
import net.fabricmc.fabric.api.event.player.UseItemCallback;
import net.minecraft.block.Block;
import net.minecraft.block.entity.BlockEntity;
import net.minecraft.block.entity.EnderChestBlockEntity;
import net.minecraft.block.entity.LecternBlockEntity;
import net.minecraft.block.entity.LockableContainerBlockEntity;
import net.minecraft.entity.Entity;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.vehicle.StorageMinecartEntity;
import net.minecraft.inventory.Inventory;
import net.minecraft.item.*;
import net.minecraft.registry.Registries;
import net.minecraft.screen.NamedScreenHandlerFactory;
import net.minecraft.util.*;
import net.minecraft.util.hit.BlockHitResult;
import net.minecraft.util.hit.EntityHitResult;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Direction;
import net.minecraft.world.World;
import org.jetbrains.annotations.Nullable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Set;

import static de.z0rdak.yawp.api.events.region.RegionEvents.post;
import static de.z0rdak.yawp.config.server.LoggingConfig.FLAG_LOGGER;
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
        EntityElytraEvents.ALLOW.register(PlayerFlagHandler::onElytraFlight);

        UseItemCallback.EVENT.register(PlayerFlagHandler::onUseItem);
        UseBlockCallback.EVENT.register(PlayerFlagHandler::onUseBlock);
        UseEntityCallback.EVENT.register(PlayerFlagHandler::onUseEntity);
        
        AttackBlockCallback.EVENT.register(PlayerFlagHandler::onAttackBlock);
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
            //FLAG_LOGGER.info("[onUseItem] Player={} ({}), at=[{}], Hand={}, Item={}", player.getName().getString(), player.getUuidAsString(), player.getBlockPos().toShortString(), hand, player.getStackInHand(hand));
            
            FlagCheckEvent checkEvent = new FlagCheckEvent(player.getBlockPos(), USE_ITEMS, getDimKey(player), player);
            if (post(checkEvent)) {
                return TypedActionResult.pass(stackInHand);
            }
            FlagState flagState = processCheck(checkEvent, MessageSender::sendFlagMsg);
            if (flagState == FlagState.DENIED) {
                return TypedActionResult.fail(stackInHand);
            }
        }
        return TypedActionResult.pass(stackInHand);
    }


    /**
     * This event is fired whenever the player right clicks while targeting a block. <br>
     * This event controls which of
     * {@link net.minecraft.block.BlockState#onUse(World, PlayerEntity, Hand, BlockHitResult)}, and  <br>
     * {@link net.minecraft.item.ItemStack#useOnBlock(ItemUsageContext)}  <br>
     * will be called. <br>
     * Canceling the event will cause none of the above to be called. <br>
     * <br>
     * Let result be the first non-pass return value of the above methods, or pass, if they all pass. <br>
     * If result equals {@link ActionResult#PASS}, we proceed to {@link UseItemCallback}.  <br>
     */
    private static ActionResult onUseBlock(PlayerEntity player, World world, Hand hand, BlockHitResult blockHitResult) {
        if (isServerSide(world)) {
            ItemUsageContext useOnContext = new ItemUsageContext(player, hand, blockHitResult);
            BlockPos targetPos = useOnContext.getBlockPos();
            BlockPos placeBlockTarget = targetPos.offset(useOnContext.getSide().getOpposite());
            BlockEntity targetEntity = world.getBlockEntity(targetPos);
            boolean hasEmptyHand = hasEmptyHand(player, hand);
            ItemStack stackInHand = useOnContext.getStack();
            boolean isBlock = stackInHand.getItem() instanceof BlockItem;
            boolean isSneakingWithEmptyHands = player.isSneaking() && hasEmptyHand;
            boolean isBlockEntity = targetEntity instanceof BlockEntity;
            boolean isLockableTileEntity = targetEntity instanceof LockableContainerBlockEntity;
            boolean isEnderChest = targetEntity instanceof EnderChestBlockEntity;
            boolean isContainer = targetEntity instanceof LecternBlockEntity || isLockableTileEntity;

            //FLAG_LOGGER.info("[onUseBlock] Player={} ({}), Target=[{}], PLaceOn=[{}], BlockEntity={}, Hand={}, Item={} (isBlock={})", player.getName().getString(), player.getUuidAsString(), targetPos.toShortString(), placeBlockTarget.toShortString(), isBlockEntity, useOnContext.getHand(), player.getStackInHand(useOnContext.getHand()), isBlock);
            
            // allow player to place blocks when shift clicking usable bock
            if ((isSneakingWithEmptyHands || !player.isSneaking())) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(targetPos, USE_BLOCKS, getDimKey(player), player);
                if (post(checkEvent))
                    return ActionResult.PASS;
                FlagState flagState = processCheck(checkEvent, MessageSender::sendFlagMsg);
                if (flagState == FlagState.DENIED)
                    return ActionResult.FAIL;


                if (isEnderChest) {
                    // check allows player to place blocks when shift clicking container
                    if (player.isSneaking() && hasEmptyHand || !player.isSneaking()) {
                        checkEvent = new FlagCheckEvent(targetPos, ENDER_CHEST_ACCESS, getDimKey(player), player);
                        if (post(checkEvent))
                            return ActionResult.PASS;
                        flagState = processCheck(checkEvent, MessageSender::sendFlagMsg);
                        if (flagState == FlagState.DENIED)
                            return ActionResult.FAIL;
                    }
                }
                if (isContainer) {
                    // check allows player to place blocks when shift clicking container
                    if (player.isSneaking() && hasEmptyHand || !player.isSneaking()) {
                        checkEvent = new FlagCheckEvent(targetPos, CONTAINER_ACCESS, getDimKey(player), player);
                        if (post(checkEvent))
                            return ActionResult.PASS;
                        flagState = processCheck(checkEvent, MessageSender::sendFlagMsg);
                        if (flagState == FlagState.DENIED)
                            return ActionResult.FAIL;
                    }
                }
            }

            if (!hasEmptyHand) {
                boolean targetsContainerWhileNotSneaking = (isContainer || isEnderChest) && !player.isSneaking();
                if (targetsContainerWhileNotSneaking) { // should be allowed to access container in this case
                    FLAG_LOGGER.info("### targetsContainerWhileNotSneaking ###");
                }

                Identifier itemRl = Registries.ITEM.getId(stackInHand.getItem());
                Set<String> entities = FlagConfig.getCoveredBlockEntities();
                Set<String> entityTags = FlagConfig.getCoveredBlockEntityTags();
                boolean isCoveredByTag = entityTags.stream().anyMatch(tag -> {
                    Identifier tagRl = new Identifier(tag);
                    return stackInHand.streamTags().anyMatch(itemTagKey -> itemTagKey.id().equals(tagRl));
                });
                boolean isBlockCovered = entities.stream().anyMatch(entity -> {
                    Identifier entityRl = new Identifier(entity);
                    return itemRl.equals(entityRl);
                });
                if (isBlockCovered || isCoveredByTag) {
                    FlagCheckEvent checkEvent = new FlagCheckEvent(placeBlockTarget, PLACE_BLOCKS, getDimKey(player), player);
                    if (post(checkEvent)) {
                        return ActionResult.PASS;
                    }
                    FlagState flagState = processCheck(checkEvent, MessageSender::sendFlagMsg);
                    if (flagState == FlagState.DENIED) {
                        return ActionResult.FAIL;
                    }
                }

                boolean isBerry = stackInHand.isOf(Items.GLOW_BERRIES) || stackInHand.isOf(Items.SWEET_BERRIES);
                UseAction useAction = stackInHand.getUseAction();
                if (isBlock || (isBerry && useAction == UseAction.EAT)) {
                    FlagCheckEvent checkEvent = new FlagCheckEvent(placeBlockTarget, PLACE_BLOCKS, getDimKey(player), player);
                    if (post(checkEvent))
                        return ActionResult.PASS;
                    FlagState flagState = processCheck(checkEvent, MessageSender::sendFlagMsg);
                    if (flagState == FlagState.DENIED)
                        return ActionResult.FAIL;
                }

                FlagCheckEvent checkEvent = new FlagCheckEvent(targetPos, USE_ITEMS, getDimKey(player), player);
                if (post(checkEvent))
                    return ActionResult.PASS;
                FlagState flagState = processCheck(checkEvent, MessageSender::sendFlagMsg);
                if (flagState == FlagState.DENIED)
                    return ActionResult.FAIL;
            }
        }
        player.getInventory().markDirty();
        return ActionResult.PASS;
    }

    /**
     * This event is fired on both sides when the player right-clicks an entity.
     * It is responsible for all general entity interactions.
     * This event's state affects whether {@link Entity#interact(PlayerEntity, Hand)} and
     * {@link net.minecraft.item.Item#useOnEntity(ItemStack, PlayerEntity, LivingEntity, Hand)} are called.
     * Let result be {@link ActionResult#SUCCESS} if {@link Entity#interact(PlayerEntity, Hand)} or
     * {@link net.minecraft.item.Item#useOnEntity(ItemStack, PlayerEntity, LivingEntity, Hand)} return true,
     * or FAIL if the event is cancelled.
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
            // present entity hit result acts the same as EntityInteractSpecificEvent in Forge - only used for ArmorStands in Vanilla
            ActionResult actionResult = onUseEntitySpecific(player, world, hand, entity, entityHitResult);
            if (actionResult == ActionResult.SUCCESS) {
                return actionResult;
            }
        }

        if (isServerSide(world)) {
            //FLAG_LOGGER.info("[onUseEntity] Player={} ({}), Target={} ({}), at=[{}], Hand={}, Item={}", player.getName().getString(), player.getUuidAsString(), entity.getName().getString(), entity.getEntityName(), entity.getBlockPos().toShortString(), hand, player.getStackInHand(hand));
            
            FlagCheckEvent checkEvent = new FlagCheckEvent(entity.getBlockPos(), USE_ENTITIES, getDimKey(player), player);
            if (post(checkEvent))
                return ActionResult.PASS;
            FlagState flagState = processCheck(checkEvent, MessageSender::sendFlagMsg);
            if (flagState == FlagState.DENIED)
                return ActionResult.FAIL;

            if (!hasEmptyHand(player, hand)) {
                checkEvent = new FlagCheckEvent(entity.getBlockPos(), USE_ITEMS, getDimKey(player), player);
                if (post(checkEvent))
                    return ActionResult.PASS;
                flagState = processCheck(checkEvent, MessageSender::sendFlagMsg);
                if (flagState == FlagState.DENIED)
                    return ActionResult.FAIL;
            }
            if (entity instanceof Inventory || entity instanceof NamedScreenHandlerFactory) {
                checkEvent = new FlagCheckEvent(player.getBlockPos(), CONTAINER_ACCESS, getDimKey(player), player);
                if (post(checkEvent))
                    return ActionResult.PASS;
                flagState = processCheck(checkEvent, MessageSender::sendFlagMsg);
                if (flagState == FlagState.DENIED)
                    return ActionResult.FAIL;
            }
        }
        return ActionResult.PASS;
    }

    private static ActionResult onUseEntitySpecific(PlayerEntity player, World world, Hand hand, Entity entity, EntityHitResult entityHitResult) {
        return ActionResult.PASS;
    }

    private static ActionResult onAttackBlock(PlayerEntity player, World world, Hand hand, BlockPos blockPos, Direction direction) {
        if (isServerSide(world)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(blockPos, BREAK_BLOCKS, getDimKey(player), player);
            if (post(checkEvent)) {
                return ActionResult.PASS;
            }
            FlagState flagState = processCheck(checkEvent, MessageSender::sendFlagMsg);
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
            FlagState flagState = processCheck(checkEvent, MessageSender::sendFlagMsg);
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
            FlagState flagState = processCheck(checkEvent, MessageSender::sendFlagMsg);
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
                FlagState flagState = processCheck(checkEvent, MessageSender::sendFlagMsg);
                return flagState == FlagState.DENIED;
            }
        }
        return true;
    }

    private static boolean hasEmptyHands(PlayerEntity player) {
        return player.getStackInHand(Hand.MAIN_HAND).getItem().equals(Items.AIR)
                && player.getStackInHand(Hand.OFF_HAND).getItem().equals(Items.AIR);
    }

    private static boolean hasEmptyHand(PlayerEntity player, Hand hand) {
        return player.getStackInHand(hand).getItem().equals(Items.AIR);
    }
}