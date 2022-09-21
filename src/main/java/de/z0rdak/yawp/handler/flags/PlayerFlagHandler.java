package de.z0rdak.yawp.handler.flags;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.config.server.FlagConfig;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.network.chat.contents.TranslatableContents;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.animal.Animal;
import net.minecraft.world.entity.npc.Villager;
import net.minecraft.world.entity.npc.WanderingTrader;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.entity.vehicle.AbstractMinecartContainer;
import net.minecraft.world.item.Items;
import net.minecraft.world.level.Explosion;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.*;
import net.minecraft.world.level.block.entity.*;
import net.minecraft.world.phys.AABB;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraft.world.phys.HitResult;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.common.ToolActions;
import net.minecraftforge.event.CommandEvent;
import net.minecraftforge.event.ServerChatEvent;
import net.minecraftforge.event.entity.EntityMountEvent;
import net.minecraftforge.event.entity.EntityTeleportEvent;
import net.minecraftforge.event.entity.item.ItemTossEvent;
import net.minecraftforge.event.entity.living.*;
import net.minecraftforge.event.entity.player.*;
import net.minecraftforge.event.level.BlockEvent;
import net.minecraftforge.event.level.ExplosionEvent;
import net.minecraftforge.eventbus.api.Event;
import net.minecraftforge.eventbus.api.EventPriority;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;

import java.util.List;

import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.getEntityDim;
import static net.minecraftforge.fml.common.Mod.EventBusSubscriber.Bus.FORGE;

/**
 * Contains flag handler for events directly related/cause to/by players.
 */
@Mod.EventBusSubscriber(modid = YetAnotherWorldProtector.MODID, value = Dist.DEDICATED_SERVER, bus = FORGE)
public final class PlayerFlagHandler {

    private PlayerFlagHandler() {
    }

    @SubscribeEvent
    public static void onAttackPlayer(AttackEntityEvent event) {
        if (!event.getEntity().getCommandSenderWorld().isClientSide) {
            if (event.getTarget() instanceof Player target) {
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getEntity()));
                if (dimCache != null && dimCache.getDimensionalRegion().isActive()) {
                    DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                    if (dimRegion.containsFlag(RegionFlag.ATTACK_PLAYERS) && !dimCache.hasMember(event.getEntity())) {
                        event.setCanceled(true);
                        MessageUtil.sendStatusMessage(event.getEntity(), "flag.msg.event.player.pvp");
                    }

                }
            }
        }
    }

    /**
     * Idea: Flag for all entities?
     *
     * @param event
     */
    @SubscribeEvent
    public static void onAttackEntity(AttackEntityEvent event) {
        Player player = event.getEntity();
        Entity eventEntity = event.getTarget();
        DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getEntity()));
        if (dimCache != null && dimCache.getDimensionalRegion().isActive()) {
            DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
            if (!event.getTarget().getCommandSenderWorld().isClientSide) {
                if (isAnimal(eventEntity)) {
                    if (dimRegion.containsFlag(RegionFlag.ATTACK_ANIMALS) && !dimRegion.permits(player)) {
                        MessageUtil.sendStatusMessage(player, MutableComponent.create(new TranslatableContents("message.event.mobs.hurt_animal")));
                        event.setCanceled(true);
                    }
                }
                if (isMonster(eventEntity)) {
                    if (dimRegion.containsFlag(RegionFlag.ATTACK_MONSTERS) && !dimRegion.permits(player)) {
                        MessageUtil.sendStatusMessage(player, MutableComponent.create(new TranslatableContents("message.event.mobs.hurt_monster")));
                        event.setCanceled(true);
                    }
                }
                if (event.getTarget() instanceof Villager) {
                    if (dimRegion.containsFlag(RegionFlag.ATTACK_VILLAGERS) && !dimRegion.permits(player)) {
                        MessageUtil.sendStatusMessage(player, MutableComponent.create(new TranslatableContents("message.event.mobs.hurt_villager")));
                        event.setCanceled(true);
                    }
                }
                if (event.getTarget() instanceof WanderingTrader) {
                    if (dimRegion.containsFlag(RegionFlag.ATTACK_WANDERING_TRADER) && !dimRegion.permits(player)) {
                        MessageUtil.sendStatusMessage(player, MutableComponent.create(new TranslatableContents("message.event.mobs.hurt_villager")));
                        event.setCanceled(true);
                    }
                }
            }
        }
    }


    // unrelated: mobs pickup logic => MobEntity#livingTick
    @SubscribeEvent
    public static void onPickupItem(EntityItemPickupEvent event) {
        if (isServerSide(event)) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getEntity()));
            if (dimCache != null && dimCache.getDimensionalRegion().isActive()) {
                DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                if (dimRegion.containsFlag(RegionFlag.ITEM_PICKUP) && !dimRegion.permits(event.getEntity())) {
                    MessageUtil.sendStatusMessage(event.getEntity(), "message.event.player.pickup_item");
                    event.setCanceled(true);
                }
            }
        }
    }

    @SubscribeEvent
    public static void onBreedingAttempt(BabyEntitySpawnEvent event) {
        Player player = event.getCausedByPlayer();
        if (player != null) {
            if (!player.getCommandSenderWorld().isClientSide) {
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(player));
                if (dimCache != null && dimCache.getDimensionalRegion().isActive()) {
                    DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                    if (dimRegion.containsFlag(RegionFlag.ANIMAL_BREEDING) && !dimRegion.permits(player)) {
                        MessageUtil.sendStatusMessage(player, "message.event.mobs.breed_animals");
                        event.setCanceled(true);
                        return;
                    }

                    if (event.getParentA() instanceof Villager) {
                        // TODO: Test on Villagers and add extra flag
                    }
                }
            }
        } else {
            // TODO: test if this is fired when animals are bred without player interaction
        }
    }

    /**
     * Note: maybe add flag for different tamable animals
     *
     * @param event
     */
    @SubscribeEvent
    public static void onAnimalTameAttempt(AnimalTameEvent event) {
        Player player = event.getTamer();
        if (!player.getCommandSenderWorld().isClientSide) {
            Animal animal = event.getAnimal();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(player));
            if (dimCache != null && dimCache.getDimensionalRegion().isActive()) {
                DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                if (dimRegion.containsFlag(RegionFlag.ANIMAL_TAMING) && !dimRegion.permits(player)) {
                    event.setCanceled(true);
                    MessageUtil.sendStatusMessage(player, "message.event.mobs.tame_animal");
                }
            }
        }
    }

    @SubscribeEvent
    public static void onPlayerLevelChange(PlayerXpEvent.LevelChange event) {
        if (!event.getEntity().getCommandSenderWorld().isClientSide) {
            Player player = event.getEntity();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getEntity()));
            if (dimCache != null && dimCache.getDimensionalRegion().isActive()) {
                DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                if (dimRegion.containsFlag(RegionFlag.LEVEL_FREEZE) && !dimRegion.permits(player)) {
                    MessageUtil.sendStatusMessage(player, "message.event.player.level_freeze");
                    event.setCanceled(true);
                }
            }
        }
    }

    @SubscribeEvent
    public static void onPlayerXPChange(PlayerXpEvent.XpChange event) {
        if (!event.getEntity().getCommandSenderWorld().isClientSide) {
            Player player = event.getEntity();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getEntity()));
            if (dimCache != null && dimCache.getDimensionalRegion().isActive()) {
                DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                if (dimRegion.containsFlag(RegionFlag.XP_FREEZE) && !dimRegion.permits(player)) {
                    MessageUtil.sendStatusMessage(player, "message.protection.player.xp_freeze");
                    event.setCanceled(true);
                    event.setAmount(0);
                }
            }
        }
    }

    @SubscribeEvent
    public static void onPlayerXpPickup(PlayerXpEvent.PickupXp event) {
        if (!event.getEntity().getCommandSenderWorld().isClientSide) {
            Player player = event.getEntity();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getEntity()));
            if (dimCache != null && dimCache.getDimensionalRegion().isActive()) {
                DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                if (dimRegion.containsFlag(RegionFlag.XP_PICKUP) && !dimRegion.permits(player)) {
                    MessageUtil.sendStatusMessage(player, "message.protection.player.xp_pickup");
                    event.setCanceled(true);
                    event.getOrb().remove(Entity.RemovalReason.DISCARDED);
                }
            }
        }
    }


    // TODO: handle flags for Villagers, Animals, Monsters, Player separate
    @SubscribeEvent
    public static void onHurt(LivingHurtEvent event) {
        if (isServerSide(event)) {
            Entity dmgSourceEntity = event.getSource().getDirectEntity();
            Entity hurtEntity = event.getEntity();
            if (dmgSourceEntity != null) {
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(dmgSourceEntity));
                if (dimCache != null && dimCache.getDimensionalRegion().isActive()) {
                    DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                    if (hurtEntity instanceof Player && dmgSourceEntity instanceof Player) {
                        Player playerTarget = (Player) hurtEntity;
                        Player playerSource = (Player) dmgSourceEntity;
                        // another check for PVP - this does not prevent knochback? but prevents dmg
                        if (!dimRegion.permits(playerSource) && dimRegion.containsFlag(RegionFlag.ATTACK_PLAYERS)) {
                            event.setCanceled(true);
                            return;
                        }
                        if (dimRegion.permits(playerTarget) && dimRegion.containsFlag(RegionFlag.INVINCIBLE)) {
                            event.setCanceled(true);
                            return;
                        }
                    }
                }
            }
        }
    }

    @SubscribeEvent
    public static void onReceiveDmg(LivingDamageEvent event) {
        if (isServerSide(event)) {
            Entity dmgSourceEntity = event.getSource().getDirectEntity();
            if (dmgSourceEntity != null) {
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(dmgSourceEntity));
                if (dimCache != null && dimCache.getDimensionalRegion().isActive()) {
                    DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                    if (event.getEntity() instanceof Player) {
                        Player player = (Player) event.getEntity();
                        // another check for PVP - this does not prevent knochback? but prevents dmg
                        if (dmgSourceEntity instanceof Player && dimRegion.containsFlag(RegionFlag.ATTACK_PLAYERS)) {
                            event.setCanceled(true);
                            return;
                        }
                        if (dimRegion.containsFlag(RegionFlag.INVINCIBLE)) {
                            event.setCanceled(true);
                            return;
                        }
                    }
                }
            }
        }
    }

    @SubscribeEvent
    public static void onHurtKnockback(LivingKnockBackEvent event) {
        if (isServerSide(event)) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getEntity()));
            if (dimCache != null && dimCache.getDimensionalRegion().isActive()) {
                DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                if (event.getEntity() instanceof Player) {
                    Player dmgTargetEntity = (Player) event.getEntity();
                    // another check for PVP - Prevents knockback
                    if (dimRegion.containsFlag(RegionFlag.ATTACK_PLAYERS)) {
                        event.setCanceled(true);
                        return;
                    }
                }
            }
            // TODO: Flag for knockback?
        }
    }

    @SubscribeEvent
    public static void onPlayerBreakBlock(BlockEvent.BreakEvent event) {
        if (isServerSide(event)) {
            Player player = event.getPlayer();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getPlayer()));
            if (dimCache != null && dimCache.getDimensionalRegion().isActive()) {
                DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                if (dimRegion.containsFlag(RegionFlag.BREAK_BLOCKS) && !dimRegion.permits(player)) {
                    event.setCanceled(true);
                    MessageUtil.sendStatusMessage(player, MutableComponent.create(new TranslatableContents("message.event.protection.break_block")));
                    return;
                }
            }
        }
    }

    @SubscribeEvent
    public static void onPlayerPlaceBlock(BlockEvent.EntityPlaceEvent event) {
        if (isServerSide(event)) {
            if (event.getEntity() != null && event.getEntity() instanceof Player) {
                Player player = (Player) event.getEntity();
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(player));
                if (dimCache != null && dimCache.getDimensionalRegion().isActive()) {
                    DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                    if (dimRegion.containsFlag(RegionFlag.PLACE_BLOCKS) && !dimRegion.permits(player)) {
                        event.setCanceled(true);
                        MessageUtil.sendStatusMessage(player, MutableComponent.create(new TranslatableContents("message.event.protection.place_block")));
                    }
                }
            }
        }
    }

    @SubscribeEvent(priority = EventPriority.HIGHEST)
    public static void onEntityBreak(AttackEntityEvent event) {
        if (isServerSide(event)) {
            Entity target = event.getTarget();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getEntity()));
            if (dimCache != null && dimCache.getDimensionalRegion().isActive()) {
                DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                List<? extends String> entities = FlagConfig.BREAK_FLAG_ENTITIES.get();
                if (dimRegion.containsFlag(RegionFlag.BREAK_ENTITIES)) {
                    entities.forEach(entity -> {
                        ResourceLocation entityResourceLocation = new ResourceLocation(entity);
                        /*
                        if (target.getType().getRegistryName() != null
                                && target.getType().getRegistryName().equals(entityResourceLocation)) {
                            event.setCanceled(true);
                            return;
                        }
                         */
                    });
                }
            }
        }
    }

    @SubscribeEvent
    public static void onExplosionStarted(ExplosionEvent.Start event) {
        if (!event.getLevel().isClientSide) {
            Explosion explosion = event.getExplosion();
            if (explosion.getExploder() instanceof Player) {
                YetAnotherWorldProtector.LOGGER.info("@@@@@  @@@@@");
                YetAnotherWorldProtector.LOGGER.info(explosion.getSourceMob());
                YetAnotherWorldProtector.LOGGER.info("@@@@@  @@@@@");
                Player player = (Player) explosion.getExploder();
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(player));
                if (dimCache != null && dimCache.getDimensionalRegion().isActive()) {
                    DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                    if (dimRegion.containsFlag(RegionFlag.IGNITE_EXPLOSIVES) && !dimRegion.permits(player)) {
                        event.setCanceled(true);
                        MessageUtil.sendStatusMessage(player, "message.event.protection.ignite_tnt");
                    }
                }
            } else {
                // Only debugging for now here
                YetAnotherWorldProtector.LOGGER.info("#######    #######");
                YetAnotherWorldProtector.LOGGER.info(explosion.getSourceMob());
                YetAnotherWorldProtector.LOGGER.info("#######    #######");
                // TODO: Explosion triggered by projectile or other TNT, or [.?.] -> Griefing
            }
        }
    }

    @SubscribeEvent
    public static void onBonemealUse(BonemealEvent event) {
        if (!event.getLevel().isClientSide) {
            Player player = (Player) event.getEntity();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getEntity()));
            if (dimCache != null && dimCache.getDimensionalRegion().isActive()) {
                DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                if (dimRegion.containsFlag(RegionFlag.USE_BONEMEAL) && !dimRegion.permits(player)) {
                    event.setCanceled(true);
                    MessageUtil.sendStatusMessage(player, "message.event.world.use_bone_meal");
                    return;
                }
            }
        }
    }

    @SubscribeEvent
    public static void onPlayerUseEnderPearl(EntityTeleportEvent event) {
        Level world = event.getEntity().level;
        if (!world.isClientSide) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(world.dimension());
            if (dimCache != null && dimCache.getDimensionalRegion().isActive()) {
                DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                // handle player teleportation using ender pearls
                if (event instanceof EntityTeleportEvent.EnderPearl) {
                    EntityTeleportEvent.EnderPearl enderPearlEvent = (EntityTeleportEvent.EnderPearl) event;
                    ServerPlayer player = enderPearlEvent.getPlayer();
                    if ((dimRegion.containsFlag(RegionFlag.USE_ENDERPEARL_FROM_REGION)
                            || dimRegion.containsFlag(RegionFlag.USE_ENDERPEARL_TO_REGION))
                            && !dimRegion.permits(player)) {
                        event.setCanceled(true);
                        MessageUtil.sendStatusMessage(player, "message.event.teleport.ender_pearl.from_region");
                    /*
                    // refund pearl - duplication danger with e.g. origins mod
                    int count = player.getHeldItem(player.getActiveHand()).getCount();
                    player.getHeldItem(player.getActiveHand()).setCount(count + 1);
                    return;
                    */
                        return;
                    }
                }
            }
        }
    }

    /**
     * @param event
     */
    @SubscribeEvent
    public static void onPlayerUseToolSecondary(BlockEvent.BlockToolModificationEvent event) {
        if (!event.getLevel().isClientSide()) {
            if (!event.isSimulated()) {
                Player player = event.getPlayer();
                if (player != null) {
                    DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(player));
                    if (dimCache != null && dimCache.getDimensionalRegion().isActive()) {
                        DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                        boolean playerNotPermitted = !dimRegion.permits(player);
                        if (dimRegion.containsFlag(RegionFlag.TOOL_SECONDARY_USE) && playerNotPermitted) {
                            event.setCanceled(true);
                            MessageUtil.sendStatusMessage(player, "message.event.protection.tool_secondary_use");
                            return;
                        }
                        if (event.getToolAction().equals(ToolActions.AXE_STRIP) && dimRegion.containsFlag(RegionFlag.AXE_STRIP) && playerNotPermitted)
                        {
                            event.setCanceled(true);
                            MessageUtil.sendStatusMessage(player, "message.event.protection.strip_wood");
                            return;
                        }
                        if (event.getToolAction().equals(ToolActions.HOE_TILL) && dimRegion.containsFlag(RegionFlag.HOE_TILL) && playerNotPermitted) {
                            event.setCanceled(true);
                            MessageUtil.sendStatusMessage(player, "message.event.protection.till_farmland");
                            return;
                        }
                        if (event.getToolAction().equals(ToolActions.SHOVEL_FLATTEN) && dimRegion.containsFlag(RegionFlag.SHOVEL_PATH) && playerNotPermitted) {
                            event.setCanceled(true);
                            MessageUtil.sendStatusMessage(player, "message.event.protection.shovel_path");
                            return;
                        }
                    }
                }
            }
        }
    }

    public static void onFluidBlockGeneration(BlockEvent.FluidPlaceBlockEvent event) {

    }

    @SubscribeEvent
    public static void onPlayerRightClickBlock(PlayerInteractEvent.RightClickBlock event) {
        if (isServerSide(event)) {
            Player player = event.getEntity();
            BlockEntity targetEntity = event.getLevel().getBlockEntity(event.getPos());
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getEntity()));
            if (dimCache != null && dimCache.getDimensionalRegion().isActive()) {
                DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                boolean isLockableTileEntity = targetEntity instanceof BaseContainerBlockEntity;
                boolean isEnderChest = targetEntity instanceof EnderChestBlockEntity;
                boolean isContainer = targetEntity instanceof LecternBlockEntity || isLockableTileEntity;
                boolean isPlayerProhibited = !dimRegion.permits(player);

                // used to allow player to place blocks when shift clicking container or usable bock
                boolean playerHasNoBlocksToPlaceInHands = player.getItemInHand(InteractionHand.MAIN_HAND).getItem().equals(Items.AIR)
                        && player.getItemInHand(InteractionHand.OFF_HAND).getItem().equals(Items.AIR);

                BlockHitResult pos = event.getHitVec();
                if (pos != null && pos.getType() == HitResult.Type.BLOCK) {
                    BlockPos bPos = pos.getBlockPos();
                    Block target = event.getLevel().getBlockState(bPos).getBlock();
                    boolean isUsableBlock = target instanceof ButtonBlock ||
                            target instanceof DoorBlock ||
                            target instanceof TrapDoorBlock ||
                            target instanceof LeverBlock ||
                            target instanceof NoteBlock ||
                            target instanceof FenceGateBlock ||
                            target instanceof DaylightDetectorBlock ||
                            target instanceof DiodeBlock ||
                            target instanceof LecternBlock ||
                            target instanceof BeaconBlock ||
                            target instanceof BrewingStandBlock;

                    if (dimRegion.containsFlag(RegionFlag.USE) && isPlayerProhibited && isUsableBlock) {
                        if (player.isShiftKeyDown() && playerHasNoBlocksToPlaceInHands || !player.isShiftKeyDown()) {
                            event.setCanceled(true);
                            MessageUtil.sendStatusMessage(player, "message.event.interact.use");
                            return;
                        }
                    }
                }
                // check for ender chest access
                if (dimRegion.containsFlag(RegionFlag.ENDER_CHEST_ACCESS) && isEnderChest && isPlayerProhibited) {
                    if (player.isShiftKeyDown() && playerHasNoBlocksToPlaceInHands || !player.isShiftKeyDown()) {
                        event.setCanceled(true);
                        MessageUtil.sendStatusMessage(player, "message.event.interact.access_ender_chest");
                        return;
                    }
                }
                // check for container access
                if (dimRegion.containsFlag(RegionFlag.CONTAINER_ACCESS) && isContainer && isPlayerProhibited) {
                    if (player.isShiftKeyDown() && playerHasNoBlocksToPlaceInHands || !player.isShiftKeyDown()) {
                        event.setCanceled(true);
                        MessageUtil.sendStatusMessage(player, "message.event.interact.access_container");
                        return;
                    }
                }
            }
        }
    }

    @SubscribeEvent
    public static void onAccessMinecartChest(PlayerInteractEvent.EntityInteract event) {
        if (isServerSide(event)) {
            Player player = event.getEntity();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getEntity()));
            if (dimCache != null && dimCache.getDimensionalRegion().isActive()) {
                DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                boolean containsChestAccess = dimRegion.containsFlag(RegionFlag.CONTAINER_ACCESS);
                boolean playerHasPermission = dimRegion.permits(player);
                boolean isMinecartContainer = event.getTarget() instanceof AbstractMinecartContainer;

                if (containsChestAccess && !playerHasPermission && isMinecartContainer) {
                    event.setCanceled(true);
                    MessageUtil.sendStatusMessage(player, "message.event.interact.access_container");
                    return;
                }
            }
        }
    }

    @SubscribeEvent
    public static void onSteppedOnActivator(BlockEvent.NeighborNotifyEvent event) {
        if (isServerSide(event)) {
            Level world = (Level) event.getLevel();
            Block block = event.getLevel().getBlockState(event.getPos()).getBlock();
            BlockPos pos = event.getPos();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(world.dimension());
            if (dimCache != null && dimCache.getDimensionalRegion().isActive()) {
                DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                boolean cancelEvent = false;
                if (block instanceof BasePressurePlateBlock) {
                    if (dimRegion.containsFlag(RegionFlag.USE)) {
                        AABB areaAbovePressurePlate = new AABB(pos.getX() - 1, pos.getY(), pos.getZ() - 1, pos.getX() + 1, pos.getY() + 2, pos.getZ() + 1);
                        List<Player> players = ((Level) event.getLevel()).getEntities(EntityType.PLAYER, areaAbovePressurePlate, (player) -> true);
                        for (Player player : players) {
                            cancelEvent = cancelEvent || !dimRegion.permits(player);
                            MessageUtil.sendStatusMessage(player, "message.event.interact.use");
                            event.setCanceled(cancelEvent);
                        }
                    }
                }
            }
        }
    }

    /**
     * Note: Does not prevent from fluids generate additional blocks (cobble generator). Use BlockEvent.FluidPlaceBlockEvent for this
     *
     * @param event

    @SubscribeEvent
    public static void onBucketFill(FillBucketEvent event) {
        // Note: FilledBucket seems to always be null. use maxStackSize to determine bucket state (empty or filled)
        Player player = event.getEntity();
        if (!event.getLevel()ClientSide) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getEntity()));
            if (dimCache != null && dimCache.getDimensionalRegion().isActive()) {
                DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                if (event.getTarget() != null) {
                    // MaxStackSize: 1 -> full bucket so only placeable; >1 -> empty bucket, only fillable
                    int bucketItemMaxStackCount = event.getEmptyBucket().getMaxStackSize();
                    // placing fluid
                    if (bucketItemMaxStackCount == 1) {
                        if (dimRegion.containsFlag(RegionFlag.PLACE_FLUIDS) && !dimRegion.permits(player)) {
                            MessageUtil.sendStatusMessage(player, MutableComponent.create(new TranslatableContents()("message.event.protection.place_fluid")));
                            event.setCanceled(true);
                            return;
                        }
                    }
                    // scooping fluid (breaking fluid)
                    if (bucketItemMaxStackCount > 1) {
                        boolean isWaterlogged = false;
                        boolean isFluid = false;
                        HitResult pos = event.getTarget();
                        if (pos != null && pos.getType() == HitResult.Type.BLOCK) {
                            Vec3 absPos = pos.getLocation();
                            BlockState blockState = event.getLevel()tBlockState(new BlockPos(absPos));
                            // check for waterlogged block
                            if (blockState.getBlock() instanceof SimpleWaterloggedBlock) {
                                isWaterlogged = blockState.getValue(BlockStateProperties.WATERLOGGED);
                            }
                            // check if target has a fluid tag
                            for (ITag.INamedTag<Fluid> tag : FluidTags.getWrappers()) {
                                if (blockState.getFluidState().getFluidState().is(tag)) {
                                    isFluid = true;
                                    break;
                                }
                            }
                            if (isWaterlogged || isFluid) {
                                if (dimRegion.containsFlag(RegionFlag.SCOOP_FLUIDS) && !dimRegion.permits(player)) {
                                    MessageUtil.sendStatusMessage(player, MutableComponent.create(new TranslatableContents()("message.event.protection.scoop_fluid")));
                                    event.setCanceled(true);
                                    return;
                                }
                            }
                        }
                    }
                }
            }
        }
    }


    /**
     * Note: message send to server but not distributed to all clients
     *
     * @param event
     */
    @SubscribeEvent
    public static void onSendChat(ServerChatEvent event) {
        if (event.getPlayer() != null) {
            ServerPlayer player = event.getPlayer();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getPlayer()));
            if (dimCache != null && dimCache.getDimensionalRegion().isActive()) {
                DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                if (dimRegion.containsFlag(RegionFlag.SEND_MESSAGE) && !dimRegion.permits(player)) {
                    event.setCanceled(true);
                    MessageUtil.sendStatusMessage(player, MutableComponent.create(new TranslatableContents("message.event.player.speak")));
                }
            }
        }
    }

    // Should only be send on server side
    @SubscribeEvent
    public static void onCommandSend(CommandEvent event) {
        try {
            Player player = event.getParseResults().getContext().getSource().getPlayerOrException();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(player));
            if (dimCache != null && dimCache.getDimensionalRegion().isActive()) {
                DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                if (dimRegion.containsFlag(RegionFlag.EXECUTE_COMMAND) && !dimRegion.permits(player)) {
                    event.setCanceled(true);
                    MessageUtil.sendStatusMessage(player, "message.event.player.execute-commands");
                    return;
                }
            }
            // TODO: add command list to block only specific commands, regardless of mod and permission of command
            // event.getParseResults().getContext().getNodes().forEach(node -> WorldProtector.LOGGER.debug(node.getNode().getName()));
        } catch (CommandSyntaxException e) {
            // Most likely thrown because command was not send by a player.
            // This is fine because we don't want this flag to be triggered from non-players
        }
    }

    @SubscribeEvent
    public static void onPlayerSleep(SleepingTimeCheckEvent event) {
        if (!event.getEntity().getCommandSenderWorld().isClientSide) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getEntity()));
            if (dimCache != null && dimCache.getDimensionalRegion().isActive()) {
                DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                Player player = event.getEntity();
                if (dimRegion.containsFlag(RegionFlag.SLEEP) && !dimRegion.permits(player)) {
                    MessageUtil.sendStatusMessage(player, "message.event.player.sleep");
                    event.setResult(Event.Result.DENY);
                    return;
                }
            }
        }
    }

    @SubscribeEvent
    public static void onSetSpawn(PlayerSetSpawnEvent event) {
        if (!event.getEntity().getCommandSenderWorld().isClientSide) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getEntity()));
            if (dimCache != null && dimCache.getDimensionalRegion().isActive()) {
                DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                BlockPos newSpawn = event.getNewSpawn();
                Player player = event.getEntity();
                if (newSpawn != null) {
                    // attempt to set spawn
                    if (dimRegion.containsFlag(RegionFlag.SET_SPAWN) && !dimRegion.permits(player)) {
                        event.setCanceled(true);
                        MessageUtil.sendStatusMessage(player, "message.event.player.set_spawn");
                        return;
                    }
                }
            }
        }
    }
         /*
        else {
            // attempt to reset spawn
            for (IRegion region : regions) {
                // TODO: not working?
                if (region.containsFlag(RegionFlag.RESET_SPAWN.toString()) && region.forbids(player)) {
                    event.setCanceled(true);
                    MessageUtils.sendStatusMessage(player, "message.event.player.reset_spawn");
                    return;
                }
            }

        }

    }
    */

    /**
     * TODO: Check for duplication
     *
     * @param event
     */
    @SubscribeEvent
    public static void onPlayerDropItem(ItemTossEvent event) {
        if (!event.getEntity().getCommandSenderWorld().isClientSide) {

            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getEntity()));
            if (dimCache != null && dimCache.getDimensionalRegion().isActive()) {
                DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                Player player = event.getPlayer();
                if (dimRegion.containsFlag(RegionFlag.ITEM_DROP) && !dimRegion.permits(player)) {
                    event.setCanceled(true);
                    player.getInventory().add(event.getEntity().getItem());
                    MessageUtil.sendStatusMessage(player, "message.event.player.drop_item");
                    return;
                }
            }
        }
    }

    /**
     * Idea: Flags for different animals to mount
     *
     * @param event
     */
    @SubscribeEvent
    public static void onEntityMountAttempt(EntityMountEvent event) {
        if (!event.getLevel().isClientSide) {
            Entity entityBeingMounted = event.getEntityBeingMounted();
            // could be mob that dismounts because entity being mounted dies?
            boolean playerAttemptsMounting = event.getEntityMounting() instanceof Player;
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getEntityMounting()));
            if (dimCache != null && dimCache.getDimensionalRegion().isActive()) {
                DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                if (playerAttemptsMounting) {
                    Player player = (Player) event.getEntityMounting();
                    if (event.isDismounting() && dimRegion.containsFlag(RegionFlag.ANIMAL_UNMOUNTING) && !dimRegion.permits(player)) {
                        event.setCanceled(true); // Does not correctly unmount player
                        MessageUtil.sendStatusMessage(player, "message.event.player.unmount");
                    }
                    if (event.isMounting() && dimRegion.containsFlag(RegionFlag.ANIMAL_MOUNTING) && !dimRegion.permits(player)) {
                        event.setCanceled(true);
                        MessageUtil.sendStatusMessage(player, "message.event.player.mount");
                    }
                }
            }
        }
    }
}
