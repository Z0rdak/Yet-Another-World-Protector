package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.api.FlagEvaluator;
import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
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
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.InteractionResultHolder;
import net.minecraft.world.MenuProvider;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.HasCustomInventoryScreen;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.entity.vehicle.ContainerEntity;
import net.minecraft.world.item.*;
import net.minecraft.world.item.context.UseOnContext;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BaseContainerBlockEntity;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.entity.EnderChestBlockEntity;
import net.minecraft.world.level.block.entity.LecternBlockEntity;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraft.world.phys.EntityHitResult;
import org.jetbrains.annotations.Nullable;

import java.util.Set;

import static de.z0rdak.yawp.core.flag.RegionFlag.*;
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
        // EntityElytraEvents.ALLOW.register(PlayerFlagHandler::onElytraFlight);

        UseItemCallback.EVENT.register(PlayerFlagHandler::onUseItem);
        UseBlockCallback.EVENT.register(PlayerFlagHandler::onUseBlock);
        UseEntityCallback.EVENT.register(PlayerFlagHandler::onUseEntity);

        AttackBlockCallback.EVENT.register(PlayerFlagHandler::onAttackBlock);
    }

    /**
     * This event is fired before the player triggers {@link Item#use(Level, Player, InteractionHand)}.
     * Note that this is NOT fired if the player is targeting a block {@link UseBlockCallback} or entity {@link UseEntityCallback}.
     */
    private static InteractionResultHolder<ItemStack> onUseItem(Player player, Level world, InteractionHand hand) {
        /* Vanilla code - START
        This is in place to ensure same behaviour of flags across fabric and forge - check this on each update! */
        ItemStack stackInHand = player.getItemInHand(hand);
        if (player.isSpectator() || player.getCooldowns().isOnCooldown(stackInHand.getItem())) {
            return InteractionResultHolder.pass(stackInHand);
        }
        /* Vanilla code - END */
        if (isServerSide(world)) {
            //FLAG_LOGGER.info("[onUseItem] Player={} ({}), at=[{}], Hand={}, Item={}", player.getName().getString(), player.getUuidAsString(), player.getBlockPos().toShortString(), hand, player.getStackInHand(hand));
            FlagCheckEvent checkEvent = new FlagCheckEvent(player.blockPosition(), USE_ITEMS, getDimKey(player), player);
            if (Services.EVENT.post(checkEvent)) {
                return InteractionResultHolder.pass(stackInHand);
            }
            FlagState flagState = FlagEvaluator.processCheck(checkEvent, MessageSender::sendFlagMsg);
            if (flagState == FlagState.DENIED) {
                return InteractionResultHolder.fail(stackInHand);
            }
        }
        return InteractionResultHolder.pass(stackInHand);
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
            BlockPos placeBlockTarget = targetPos.relative(useOnContext.getClickedFace());
            BlockEntity targetEntity = world.getBlockEntity(targetPos);
            boolean hasEmptyHand = hasEmptyHand(player, hand);
            ItemStack stackInHand = useOnContext.getItemInHand();
            boolean isBlock = stackInHand.getItem() instanceof BlockItem;
            boolean isSneakingWithEmptyHands = player.isShiftKeyDown() && hasEmptyHand;
            boolean isBlockEntity = targetEntity instanceof BlockEntity;
            boolean isLockableTileEntity = targetEntity instanceof BaseContainerBlockEntity;
            boolean isEnderChest = targetEntity instanceof EnderChestBlockEntity;
            boolean isContainer = targetEntity instanceof LecternBlockEntity || isLockableTileEntity;

            //FLAG_LOGGER.info("[onUseBlock] Player={} ({}), Target=[{}], PLaceOn=[{}], BlockEntity={}, Hand={}, Item={} (isBlock={})", player.getName().getString(), player.getUuidAsString(), targetPos.toShortString(), placeBlockTarget.toShortString(), isBlockEntity, useOnConComponent.getHand(), player.getStackInHand(useOnConComponent.getHand()), isBlock);

            // allow player to place blocks when shift clicking usable bock
            if ((isSneakingWithEmptyHands || !player.isShiftKeyDown())) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(targetPos, USE_BLOCKS, getDimKey(player), player);
                if (Services.EVENT.post(checkEvent))
                    return InteractionResult.PASS;
                FlagState flagState = FlagEvaluator.processCheck(checkEvent, MessageSender::sendFlagMsg);
                if (flagState == FlagState.DENIED)
                    return InteractionResult.FAIL;


                if (isEnderChest) {
                    // check allows player to place blocks when shift clicking container
                    if (player.isShiftKeyDown() && hasEmptyHand || !player.isShiftKeyDown()) {
                        checkEvent = new FlagCheckEvent(targetPos, ENDER_CHEST_ACCESS, getDimKey(player), player);
                        if (Services.EVENT.post(checkEvent))
                            return InteractionResult.PASS;
                        flagState = FlagEvaluator.processCheck(checkEvent, MessageSender::sendFlagMsg);
                        if (flagState == FlagState.DENIED)
                            return InteractionResult.FAIL;
                    }
                }
                if (isContainer) {
                    // check allows player to place blocks when shift clicking container
                    if (player.isShiftKeyDown() && hasEmptyHand || !player.isShiftKeyDown()) {
                        checkEvent = new FlagCheckEvent(targetPos, CONTAINER_ACCESS, getDimKey(player), player);
                        if (Services.EVENT.post(checkEvent))
                            return InteractionResult.PASS;
                        flagState = FlagEvaluator.processCheck(checkEvent, MessageSender::sendFlagMsg);
                        if (flagState == FlagState.DENIED)
                            return InteractionResult.FAIL;
                    }
                }
            }

            if (!hasEmptyHand) {
                boolean targetsContainerWhileNotSneaking = (isContainer || isEnderChest) && !player.isShiftKeyDown();
                if (targetsContainerWhileNotSneaking) { // should be allowed to access container in this case
                    //FLAG_LOGGER.info("### targetsContainerWhileNotSneaking ###");
                }

                ResourceLocation itemRl = BuiltInRegistries.ITEM.getKey(stackInHand.getItem());
                Set<String> entities = FlagConfig.getCoveredBlockEntities();
                Set<String> entityTags = FlagConfig.getCoveredBlockEntityTags();
                boolean isCoveredByTag = entityTags.stream().anyMatch(tag -> {
                    ResourceLocation tagRl = ResourceLocation.parse(tag);
                    return stackInHand.getTags().anyMatch(itemTagKey -> itemTagKey.location().equals(tagRl));
                });
                boolean isBlockCovered = entities.stream().anyMatch(entity -> {
                    ResourceLocation entityRl = ResourceLocation.parse(entity);
                    return itemRl.equals(entityRl);
                });
                if (isBlockCovered || isCoveredByTag) {
                    FlagCheckEvent checkEvent = new FlagCheckEvent(placeBlockTarget, PLACE_BLOCKS, getDimKey(player), player);
                    if (Services.EVENT.post(checkEvent)) {
                        return InteractionResult.PASS;
                    }
                    FlagState flagState = FlagEvaluator.processCheck(checkEvent, MessageSender::sendFlagMsg);
                    if (flagState == FlagState.DENIED) {
                        return InteractionResult.FAIL;
                    }
                }


                boolean isBerry = ItemStack.isSameItem(stackInHand, Items.GLOW_BERRIES.getDefaultInstance()) || ItemStack.isSameItem(stackInHand, Items.GLOW_BERRIES.getDefaultInstance());
                UseAnim useAction = stackInHand.getUseAnimation();
                if (isBlock || (isBerry && useAction == UseAnim.EAT)) {
                    FlagCheckEvent checkEvent = new FlagCheckEvent(placeBlockTarget, PLACE_BLOCKS, getDimKey(player), player);
                    if (Services.EVENT.post(checkEvent))
                        return InteractionResult.PASS;
                    FlagState flagState = FlagEvaluator.processCheck(checkEvent, MessageSender::sendFlagMsg);
                    if (flagState == FlagState.DENIED)
                        return InteractionResult.FAIL;
                }

                FlagCheckEvent checkEvent = new FlagCheckEvent(targetPos, USE_ITEMS, getDimKey(player), player);
                if (Services.EVENT.post(checkEvent))
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
     * This event's state affects whether {@link Entity#interact(Player, InteractionHand)} and
     * {@link Item#interactLivingEntity(ItemStack, Player, LivingEntity, InteractionHand)} are called.
     * Let result be {@link InteractionResult#SUCCESS} if {@link Entity#interact(Player, InteractionHand)} or
     * {@link Item#interactLivingEntity(ItemStack, Player, LivingEntity, InteractionHand)} return true,
     * or FAIL if the event is cancelled.
     */
    private static InteractionResult onUseEntity(Player player, Level world, InteractionHand hand, Entity entity, @Nullable EntityHitResult entityHitResult) {
        /* Vanilla code - START
        This is in place to ensure same behaviour of flags across fabric and forge - check this on each update! */
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
            //FLAG_LOGGER.info("[onUseEntity] Player={} ({}), Target={} ({}), at=[{}], Hand={}, Item={}", player.getName().getString(), player.getUuidAsString(), entity.getName().getString(), entity.getScoreboardName(), entity.getBlockPos().toShortString(), hand, player.getStackInHand(hand));

            FlagCheckEvent checkEvent = new FlagCheckEvent(entity.blockPosition(), USE_ENTITIES, getDimKey(player), player);
            if (Services.EVENT.post(checkEvent))
                return InteractionResult.PASS;
            FlagState flagState = FlagEvaluator.processCheck(checkEvent, MessageSender::sendFlagMsg);
            if (flagState == FlagState.DENIED)
                return InteractionResult.FAIL;

            if (!hasEmptyHand(player, hand)) {
                checkEvent = new FlagCheckEvent(entity.blockPosition(), USE_ITEMS, getDimKey(player), player);
                if (Services.EVENT.post(checkEvent))
                    return InteractionResult.PASS;
                flagState = FlagEvaluator.processCheck(checkEvent, MessageSender::sendFlagMsg);
                if (flagState == FlagState.DENIED)
                    return InteractionResult.FAIL;
            }
            if (entity instanceof ContainerEntity || entity instanceof HasCustomInventoryScreen) {
                checkEvent = new FlagCheckEvent(player.blockPosition(), CONTAINER_ACCESS, getDimKey(player), player);
                if (Services.EVENT.post(checkEvent))
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
            FlagCheckEvent checkEvent = new FlagCheckEvent(blockPos, BREAK_BLOCKS, getDimKey(player), player);
            if (Services.EVENT.post(checkEvent)) {
                return InteractionResult.PASS;
            }
            FlagState flagState = FlagEvaluator.processCheck(checkEvent, MessageSender::sendFlagMsg);
            return flagState == FlagState.DENIED ? InteractionResult.FAIL : InteractionResult.PASS;
        }
        return InteractionResult.PASS;
    }

    private static boolean onSettingSpawn(Player player, BlockPos blockPos) {
        if (isServerSide(player)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(blockPos, SET_SPAWN, getDimKey(player), player);
            if (Services.EVENT.post(checkEvent)) {
                return ALLOW;
            }
            FlagState flagState = FlagEvaluator.processCheck(checkEvent, MessageSender::sendFlagMsg);
            return flagState != FlagState.DENIED;
        }
        return ALLOW;
    }

    private static Player.BedSleepingProblem onAllowSleeping(Player player, BlockPos blockPos) {
        if (isServerSide(player)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(blockPos, SLEEP, getDimKey(player), player);
            if (Services.EVENT.post(checkEvent)) {
                return null;
            }
            FlagState flagState = FlagEvaluator.processCheck(checkEvent, MessageSender::sendFlagMsg);
            if (flagState == FlagState.DENIED) {
                return Player.BedSleepingProblem.NOT_POSSIBLE_HERE;
            }
        }
        return null;
    }

    private static boolean onElytraFlight(LivingEntity livingEntity) {
        if (isServerSide(livingEntity.level())) {
            if (livingEntity instanceof Player player) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(player.blockPosition(), USE_ELYTRA, getDimKey(player), player);
                if (Services.EVENT.post(checkEvent)) {
                    return ALLOW;
                }
                FlagState flagState = FlagEvaluator.processCheck(checkEvent, MessageSender::sendFlagMsg);
                return flagState != FlagState.DENIED;
            }
        }
        return ALLOW;
    }

    private static boolean hasEmptyHands(Player player) {
        return player.getItemInHand(InteractionHand.MAIN_HAND).getItem().equals(Items.AIR)
                && player.getItemInHand(InteractionHand.OFF_HAND).getItem().equals(Items.AIR);
    }

    private static boolean hasEmptyHand(Player player, InteractionHand hand) {
        return player.getItemInHand(hand).getItem().equals(Items.AIR);
    }
}