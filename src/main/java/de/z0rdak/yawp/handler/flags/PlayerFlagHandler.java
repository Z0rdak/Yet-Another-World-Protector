package de.z0rdak.yawp.handler.flags;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.config.server.FlagConfig;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.FlagMessageUtil;
import net.minecraft.block.AbstractPressurePlateBlock;
import net.minecraft.block.Block;
import net.minecraft.block.BlockState;
import net.minecraft.block.IWaterLoggable;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityType;
import net.minecraft.entity.item.TNTEntity;
import net.minecraft.entity.item.minecart.ContainerMinecartEntity;
import net.minecraft.entity.merchant.villager.VillagerEntity;
import net.minecraft.entity.merchant.villager.WanderingTraderEntity;
import net.minecraft.entity.monster.MonsterEntity;
import net.minecraft.entity.passive.AnimalEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.fluid.Fluid;
import net.minecraft.item.Items;
import net.minecraft.state.properties.BlockStateProperties;
import net.minecraft.tags.FluidTags;
import net.minecraft.tags.ITag;
import net.minecraft.tileentity.EnderChestTileEntity;
import net.minecraft.tileentity.LecternTileEntity;
import net.minecraft.tileentity.LockableTileEntity;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.Hand;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.math.AxisAlignedBB;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.BlockRayTraceResult;
import net.minecraft.util.math.RayTraceResult;
import net.minecraft.world.Explosion;
import net.minecraft.world.World;
import net.minecraftforge.event.CommandEvent;
import net.minecraftforge.event.ServerChatEvent;
import net.minecraftforge.event.TickEvent;
import net.minecraftforge.event.entity.EntityMountEvent;
import net.minecraftforge.event.entity.item.ItemTossEvent;
import net.minecraftforge.event.entity.living.*;
import net.minecraftforge.event.entity.player.*;
import net.minecraftforge.event.world.BlockEvent;
import net.minecraftforge.event.world.ExplosionEvent;
import net.minecraftforge.eventbus.api.Event;
import net.minecraftforge.eventbus.api.EventPriority;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;

import java.util.List;
import java.util.Set;

import static de.z0rdak.yawp.core.flag.RegionFlag.*;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;
import static net.minecraftforge.common.ToolType.*;
import static net.minecraftforge.fml.common.Mod.EventBusSubscriber.Bus.FORGE;

/**
 * Contains flag handler for events directly related/cause to/by players.
 */
@Mod.EventBusSubscriber(modid = YetAnotherWorldProtector.MODID, bus = FORGE)
public final class PlayerFlagHandler {

    private PlayerFlagHandler() {
    }

    @SubscribeEvent
    public static void onElytraFlying(TickEvent.PlayerTickEvent event) {
        if (isServerSide(event.player) && event.phase == TickEvent.Phase.END) {
            RegistryKey<World> entityDim = getEntityDim(event.player);
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(entityDim);
            if (dimCache != null) {
                if (event.player.isFallFlying()) {
                    FlagCheckEvent flagCheckEvent = checkEvent(event.player.blockPosition(), NO_FLIGHT, dimCache.getDimensionalRegion(), event.player);
                    if (flagCheckEvent.isDenied()) {
                        FlagMessageUtil.sendFlagMsg(flagCheckEvent);
                        event.player.stopFallFlying();
                    }
                }
            }
        }
    }

    /**
     * Prevents traditional attacks from players which use EntityPlayer.attackTargetEntityWithCurrentItem(Entity).
     */
    @SubscribeEvent
    public static void onAttackPlayer(AttackEntityEvent event) {
        if (isServerSide(event)) {
            if (event.getTarget() instanceof PlayerEntity) {
                PlayerEntity attacker = event.getPlayer();
                PlayerEntity target = (PlayerEntity) event.getTarget();
                RegistryKey<World> entityDim = getEntityDim(attacker);
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(entityDim);
                if (dimCache != null) {
                    FlagCheckEvent flagCheckEvent = checkEvent(target.blockPosition(), MELEE_PLAYERS, dimCache.getDimensionalRegion(), attacker);
                    handleAndSendMsg(event, flagCheckEvent);
                }
            }
        }
    }

    /**
     * Prevents various entities from been attacked from a player. <br>
     * TODO: Flag for all entities
     */
    @SubscribeEvent
    public static void onAttackEntity(AttackEntityEvent event) {
        if (isServerSide(event)) {
            PlayerEntity player = event.getPlayer();
            Entity eventEntity = event.getTarget();
            RegistryKey<World> entityDim = getEntityDim(event.getPlayer());
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(entityDim);
            if (dimCache != null) {
                if (isAnimal(eventEntity)) {
                    FlagCheckEvent flagCheckEvent = checkEvent(eventEntity.blockPosition(), MELEE_ANIMALS, dimCache.getDimensionalRegion(), player);
                    handleAndSendMsg(event, flagCheckEvent);
                    return;
                }
                if (isMonster(eventEntity)) {
                    FlagCheckEvent flagCheckEvent = checkEvent(eventEntity.blockPosition(), MELEE_MONSTERS, dimCache.getDimensionalRegion(), player);
                    handleAndSendMsg(event, flagCheckEvent);
                    return;
                }
                if (event.getTarget() instanceof VillagerEntity) {
                    FlagCheckEvent flagCheckEvent = checkEvent(eventEntity.blockPosition(), MELEE_VILLAGERS, dimCache.getDimensionalRegion(), player);
                    handleAndSendMsg(event, flagCheckEvent);
                    return;
                }
                if (event.getTarget() instanceof WanderingTraderEntity) {
                    FlagCheckEvent flagCheckEvent = checkEvent(eventEntity.blockPosition(), MELEE_WANDERING_TRADER, dimCache.getDimensionalRegion(), player);
                    handleAndSendMsg(event, flagCheckEvent);
                }
            }
        }
    }

    // unrelated: mobs pickup logic => MobEntity#livingTick
    @SubscribeEvent
    public static void onPickupItem(EntityItemPickupEvent event) {
        if (isServerSide(event)) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getPlayer()));
            if (dimCache != null) {
                FlagCheckEvent flagCheckEvent = checkEvent(event.getEntity().blockPosition(), ITEM_PICKUP, dimCache.getDimensionalRegion(), event.getPlayer());
                handleAndSendMsg(event, flagCheckEvent);
            }
        }
    }

    @SubscribeEvent
    public static void onBreedingAttempt(BabyEntitySpawnEvent event) {
        PlayerEntity player = event.getCausedByPlayer();
        if (player != null) {
            if (!player.getCommandSenderWorld().isClientSide) {
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(player));
                if (dimCache != null) {
                    if (event.getParentA() instanceof VillagerEntity) {
                        // TODO: Test on Villagers and add extra flag
                    }
                    FlagCheckEvent flagCheckEvent = checkEvent(event.getParentB().blockPosition(), ANIMAL_BREEDING, dimCache.getDimensionalRegion(), event.getCausedByPlayer());
                    handleAndSendMsg(event, flagCheckEvent);
                }
            }
        } else {
            // TODO: test if this is fired when animals are bred without player interaction (with mods?)
        }
    }

    /**
     * Note: maybe add flag for different tamable animals / non vanilla / etc
     */
    @SubscribeEvent
    public static void onAnimalTameAttempt(AnimalTameEvent event) {
        PlayerEntity player = event.getTamer();
        if (player != null) {
            if (!player.getCommandSenderWorld().isClientSide) {
                AnimalEntity animal = event.getAnimal();
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(player));
                if (dimCache != null) {
                    FlagCheckEvent flagCheckEvent = checkEvent(event.getAnimal().blockPosition(), ANIMAL_TAMING, dimCache.getDimensionalRegion(), player);
                    handleAndSendMsg(event, flagCheckEvent);
                }
            }
        }
    }

    @SubscribeEvent
    public static void onPlayerLevelChange(PlayerXpEvent.LevelChange event) {
        if (isServerSide(event)) {
            PlayerEntity player = event.getPlayer();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getPlayer()));
            if (dimCache != null) {
                FlagCheckEvent flagCheckEvent = checkEvent(event.getPlayer().blockPosition(), LEVEL_FREEZE, dimCache.getDimensionalRegion(), player);
                handleAndSendMsg(event, flagCheckEvent);
            }
        }
    }

    @SubscribeEvent
    public static void onPlayerXPChange(PlayerXpEvent.XpChange event) {
        if (!event.getPlayer().getCommandSenderWorld().isClientSide) {
            PlayerEntity player = event.getPlayer();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getPlayer()));
            if (dimCache != null) {
                FlagCheckEvent flagCheckEvent = checkEvent(event.getPlayer().blockPosition(), XP_FREEZE, dimCache.getDimensionalRegion(), player);
                handleAndSendMsg(event, flagCheckEvent);
                if (flagCheckEvent.isDenied()) {
                    // TODO: Test whether this is needed?
                    event.setAmount(0);
                }
            }
        }
    }

    @SubscribeEvent
    public static void onPlayerXpPickup(PlayerXpEvent.PickupXp event) {
        if (!event.getPlayer().getCommandSenderWorld().isClientSide) {
            PlayerEntity player = event.getPlayer();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getPlayer()));
            if (dimCache != null) {
                FlagCheckEvent flagCheckEvent = checkEvent(event.getPlayer().blockPosition(), XP_PICKUP, dimCache.getDimensionalRegion(), player);
                handleAndSendMsg(event, flagCheckEvent);
                if (flagCheckEvent.isDenied()) {
                    event.getOrb().remove();
                }
            }
        }
    }

    @SubscribeEvent
    public static void onPvpAction(LivingHurtEvent event) {
        if (isServerSide(event)) {
            Entity dmgSourceEntity = event.getSource().getDirectEntity();
            Entity hurtEntity = event.getEntityLiving();
            if (hurtEntity instanceof PlayerEntity && dmgSourceEntity instanceof PlayerEntity) {
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(dmgSourceEntity));
                if (dimCache != null) {
                    PlayerEntity playerTarget = (PlayerEntity) hurtEntity;
                    PlayerEntity playerSource = (PlayerEntity) dmgSourceEntity;
                    FlagCheckEvent flagCheckEvent = HandlerUtil.checkEvent(playerTarget.blockPosition(), NO_PVP, dimCache.getDimensionalRegion());
                    if (flagCheckEvent.isDenied()) {
                        FlagMessageUtil.sendFlagMsg(new PlayerFlagEvent(flagCheckEvent, playerSource));
                        event.setAmount(0f);
                        event.setCanceled(true);
                    }
                }
            }
        }
    }

    @SubscribeEvent
    public static void onPlayerHurt(LivingHurtEvent event) {
        if (isServerSide(event)) {
            Entity hurtEntity = event.getEntityLiving();
            if (hurtEntity instanceof PlayerEntity) {
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(hurtEntity));
                if (dimCache != null) {
                    PlayerEntity playerTarget = (PlayerEntity) hurtEntity;
                    FlagCheckEvent flagCheckEvent = HandlerUtil.checkEvent(playerTarget.blockPosition(), INVINCIBLE, dimCache.getDimensionalRegion());
                    if (flagCheckEvent.isDenied()) {
                        event.setAmount(0f);
                        event.setCanceled(true);
                    }
                }
            }
        }
    }


    /* TODO: Is this test even necessary anymore?
     *   - There is already a PVP flag for onHurt in place
     * */
    @SubscribeEvent
    public static void onReceiveDmg(LivingDamageEvent event) {
        if (isServerSide(event)) {
            Entity dmgSourceEntity = event.getSource().getDirectEntity();
            if (dmgSourceEntity instanceof PlayerEntity) {
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(dmgSourceEntity));
                if (dimCache != null) {
                    if (event.getEntityLiving() instanceof PlayerEntity) {
                        PlayerEntity dmgTarget = (PlayerEntity) event.getEntityLiving();
                        PlayerEntity dmgSource = ((PlayerEntity) dmgSourceEntity);
                        // another check for PVP - this does not prevent knock-back? but prevents dmg
                        FlagCheckEvent flagCheckEvent = HandlerUtil.checkEvent(dmgTarget.blockPosition(), MELEE_PLAYERS, dimCache.getDimensionalRegion());
                        if (flagCheckEvent.isDenied()) {
                            sendFlagMsg(new PlayerFlagEvent(flagCheckEvent, dmgSource));
                            event.setAmount(0f);
                            event.setCanceled(true);
                        }
                    }
                }
            }
        }
    }

    @SubscribeEvent
    public static void onPlayerKnockback(LivingKnockBackEvent event) {
        if (isServerSide(event)) {
            if (event.getEntityLiving() instanceof PlayerEntity) {
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getEntity()));
                if (dimCache != null) {
                    PlayerEntity dmgTarget = (PlayerEntity) event.getEntityLiving();
                    FlagCheckEvent flagCheckEvent = HandlerUtil.checkEvent(dmgTarget.blockPosition(), KNOCKBACK_PLAYERS, dimCache.getDimensionalRegion());
                    if (flagCheckEvent.isDenied()) {
                        event.setCanceled(true);
                        event.setStrength(0);
                    }
                    flagCheckEvent = HandlerUtil.checkEvent(dmgTarget.blockPosition(), INVINCIBLE, dimCache.getDimensionalRegion());
                    if (flagCheckEvent.isDenied()) {
                        event.setCanceled(true);
                        event.setStrength(0);
                    }
                }
            }
        }
    }

    @SubscribeEvent
    public static void onPlayerBreakBlock(BlockEvent.BreakEvent event) {
        if (isServerSide(event)) {
            PlayerEntity player = event.getPlayer();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getPlayer()));
            if (dimCache != null) {
                FlagCheckEvent flagCheckEvent = checkEvent(event.getPos(), BREAK_BLOCKS, dimCache.getDimensionalRegion(), player);
                handleAndSendMsg(event, flagCheckEvent);
            }
        }
    }

    @SubscribeEvent
    public static void onPlayerPlaceBlock(BlockEvent.EntityPlaceEvent event) {
        if (isServerSide(event)) {
            if (event.getEntity() != null && event.getEntity() instanceof PlayerEntity) {
                PlayerEntity player = (PlayerEntity) event.getEntity();
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(player));
                if (dimCache != null) {
                    FlagCheckEvent flagCheckEvent = checkEvent(event.getPos(), PLACE_BLOCKS, dimCache.getDimensionalRegion(), player);
                    handleAndSendMsg(event, flagCheckEvent);
                }
            }
        }
    }

    @SubscribeEvent(priority = EventPriority.HIGHEST)
    public static void onEntityBreak(AttackEntityEvent event) {
        if (isServerSide(event)) {
            Entity target = event.getTarget();
            PlayerEntity player = event.getPlayer();
            RegistryKey<World> entityDim = getEntityDim(event.getPlayer());
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(entityDim);
            if (dimCache != null) {
                // FIXME: Tags not considered yet
                Set<String> entityTags = FlagConfig.getBreakFlagEntityTags();
                Set<String> entities = FlagConfig.getBreakFlagEntities();
                boolean isBlockEntityCovered = entities.stream().anyMatch(entity -> {
                    ResourceLocation entityResourceLocation = new ResourceLocation(entity);
                    return target.getType().getRegistryName() != null && target.getType().getRegistryName().equals(entityResourceLocation);
                });
                if (isBlockEntityCovered) {
                    FlagCheckEvent flagCheckEvent = checkEvent(event.getTarget().blockPosition(), BREAK_BLOCKS, dimCache.getDimensionalRegion(), player);
                    handleAndSendMsg(event, flagCheckEvent);
                }
            }
        }
    }

    @SubscribeEvent
    public static void onExplosionStarted(ExplosionEvent.Start event) {
        if (!event.getWorld().isClientSide) {
            Explosion explosion = event.getExplosion();
            // Extra check for player initiated explosion here. Should normally be prevented by not allowing
            // the ignition to begin with.
            if (event.getExplosion().getExploder() instanceof TNTEntity) {
                TNTEntity tntEntity = (TNTEntity) event.getExplosion().getExploder();
                if (tntEntity.getOwner() instanceof PlayerEntity) {
                    PlayerEntity player = (PlayerEntity) tntEntity.getOwner();
                    DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(player));
                    if (dimCache != null) {
                        FlagCheckEvent flagCheckEvent = checkEvent(new BlockPos(explosion.getPosition()), IGNITE_EXPLOSIVES, dimCache.getDimensionalRegion(), player);
                        handleAndSendMsg(event, flagCheckEvent);
                    }
                }
            }
            if (explosion.getSourceMob() instanceof MonsterEntity) {
                MonsterEntity monster = (MonsterEntity) explosion.getSourceMob();
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(monster));
                FlagCheckEvent flagCheck = HandlerUtil.checkEvent(new BlockPos(explosion.getPosition()), MOB_GRIEFING, dimCache.getDimensionalRegion());
                if (flagCheck.isDenied()) {
                    event.setCanceled(flagCheck.isDenied());
                }
            }
        }
    }

    @SubscribeEvent
    public static void onBonemealUse(BonemealEvent event) {
        if (isServerSide(event)) {
            PlayerEntity player = (PlayerEntity) event.getEntity();
            RegistryKey<World> entityDim = getEntityDim(event.getPlayer());
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(entityDim);
            if (dimCache != null) {

                FlagCheckEvent flagCheckEvent = checkEvent(event.getPos(), USE_BONEMEAL, dimCache.getDimensionalRegion(), player);
                handleAndSendMsg(event, flagCheckEvent);
            }
        }
    }

    @SubscribeEvent
    public static void onPlayerUseEnderPearl(EntityTeleportEvent event) {
        if (isServerSide(event)) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getEntity()));
            if (dimCache != null) {
                // handle player teleportation using ender pearls
                if (event instanceof EntityTeleportEvent.EnderPearl) {
                    EntityTeleportEvent.EnderPearl enderPearlEvent = (EntityTeleportEvent.EnderPearl) event;
                    PlayerEntity player = enderPearlEvent.getPlayer();

                    FlagCheckEvent enderPearlToRegionFlagCheck = checkEvent(new BlockPos(event.getTarget()), USE_ENDERPEARL_TO_REGION, dimCache.getDimensionalRegion(), player);
                    if (handleAndSendMsg(event, enderPearlToRegionFlagCheck)) {
                        return;
                    }

                    FlagCheckEvent enderPearlFromRegionFlagCheck = checkEvent(player.blockPosition(), USE_ENDERPEARL_FROM_REGION, dimCache.getDimensionalRegion(), player);
                    if (handleAndSendMsg(event, enderPearlFromRegionFlagCheck)) {
                    }

                    /* FIXME: refund pearl - duplication bug with e.g. origins mod
                    int count = player.getHeldItem(player.getActiveHand()).getCount();
                    player.getHeldItem(player.getActiveHand()).setCount(count + 1);
                    return;
                    */
                }
            }
        }
    }

    @SubscribeEvent
    public static void onPlayerUseToolSecondary(BlockEvent.BlockToolInteractEvent event) {
        if (!event.getWorld().isClientSide()) {
            PlayerEntity player = event.getPlayer();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getPlayer()));
            if (dimCache != null) {
                FlagCheckEvent flagCheckEvent = checkEvent(event.getPos(), TOOL_SECONDARY_USE, dimCache.getDimensionalRegion(), player);
                if (handleAndSendMsg(event, flagCheckEvent)) {
                    // FIXME: [next update]: how about all TOOL_SECONDARY_USE is denied but one of the following is allowed?
                    // this kind of check is not uncommon. See onPlayerRightClickBlock e.g.
                    return;
                }

                if (event.getToolType().equals(AXE)) {
                    flagCheckEvent = checkEvent(event.getPos(), AXE_STRIP, dimCache.getDimensionalRegion(), player);
                    handleAndSendMsg(event, flagCheckEvent);
                }
                if (event.getToolType().equals(HOE)) {
                    flagCheckEvent = checkEvent(event.getPos(), HOE_TILL, dimCache.getDimensionalRegion(), player);
                    handleAndSendMsg(event, flagCheckEvent);
                }
                if (event.getToolType().equals(SHOVEL)) {
                    flagCheckEvent = checkEvent(event.getPos(), SHOVEL_PATH, dimCache.getDimensionalRegion(), player);
                    handleAndSendMsg(event, flagCheckEvent);
                }
            }
        }
    }

    @SubscribeEvent
    public static void onPlayerRightClickBlock(PlayerInteractEvent.RightClickBlock event) {
        if (isServerSide(event)) {
            PlayerEntity player = event.getPlayer();
            TileEntity targetEntity = event.getWorld().getBlockEntity(event.getPos());
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getPlayer()));
            if (dimCache != null) {
                boolean isLockableTileEntity = targetEntity instanceof LockableTileEntity;
                boolean isEnderChest = targetEntity instanceof EnderChestTileEntity;
                boolean isContainer = targetEntity instanceof LecternTileEntity || isLockableTileEntity;

                // used to allow player to place blocks when shift clicking container or usable bock
                boolean hasEmptyHands = hasEmptyHands(player);

                BlockRayTraceResult pos = event.getHitVec();
                boolean isBlock = pos != null && pos.getType() == RayTraceResult.Type.BLOCK;
                if (isBlock) {
                    if (player.isShiftKeyDown() && hasEmptyHands || !player.isShiftKeyDown()) {
                        FlagCheckEvent flagCheckEvent = checkEvent(pos.getBlockPos(), USE_BLOCKS, dimCache.getDimensionalRegion(), player);
                        handleAndSendMsg(event, flagCheckEvent);
                    }
                }
                if (!hasEmptyHands) {
                    FlagCheckEvent useItemCheck = checkEvent(event.getPos(), USE_ITEMS, dimCache.getDimensionalRegion(), player);
                    handleAndSendMsg(event, useItemCheck);
                }
                // Note: following flags are already covered with use_blocks
                // check for ender chest access
                if (isEnderChest) {
                    if (player.isShiftKeyDown() && hasEmptyHands || !player.isShiftKeyDown()) {
                        FlagCheckEvent flagCheckEvent = checkEvent(targetEntity.getBlockPos(), ENDER_CHEST_ACCESS, dimCache.getDimensionalRegion(), player);
                        handleAndSendMsg(event, flagCheckEvent);
                    }
                }
                // check for container access
                if (isContainer) {
                    if (player.isShiftKeyDown() && hasEmptyHands || !player.isShiftKeyDown()) {
                        FlagCheckEvent flagCheckEvent = checkEvent(targetEntity.getBlockPos(), CONTAINER_ACCESS, dimCache.getDimensionalRegion(), player);
                        handleAndSendMsg(event, flagCheckEvent);
                    }
                }
                event.getWorld().updateNeighborsAt(pos.getBlockPos(), event.getWorld().getBlockState(pos.getBlockPos()).getBlock());
            }
        }
    }

    @SubscribeEvent
    public static void onAccessMinecartChest(PlayerInteractEvent.EntityInteract event) {
        if (isServerSide(event)) {
            PlayerEntity player = event.getPlayer();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getPlayer()));
            if (dimCache != null) {
                boolean isMinecartContainer = event.getTarget() instanceof ContainerMinecartEntity;
                if (isMinecartContainer) {
                    FlagCheckEvent flagCheckEvent = checkEvent(event.getTarget().blockPosition(), CONTAINER_ACCESS, dimCache.getDimensionalRegion(), player);
                    handleAndSendMsg(event, flagCheckEvent);
                }
            }
        }
    }


    @SubscribeEvent
    public static void onEntityInteraction(PlayerInteractEvent.EntityInteractSpecific event) {
        if (isServerSide(event)) {
            PlayerEntity player = event.getPlayer();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getEntity()));
            if (dimCache != null) {
                FlagCheckEvent flagCheckEvent = checkEvent(event.getTarget().blockPosition(), USE_ENTITIES, dimCache.getDimensionalRegion(), player);
                handleAndSendMsg(event, flagCheckEvent);

                if (!hasEmptyHands(player)) {
                    FlagCheckEvent useItemCheck = checkEvent(event.getPos(), USE_ITEMS, dimCache.getDimensionalRegion(), player);
                    handleAndSendMsg(event, useItemCheck);
                }
            }
        }
    }

    private static boolean hasEmptyHands(PlayerEntity player) {
        return player.getItemInHand(Hand.MAIN_HAND).getItem().equals(Items.AIR)
                && player.getItemInHand(Hand.OFF_HAND).getItem().equals(Items.AIR);
    }

    @SubscribeEvent
    public static void onEntityInteraction(PlayerInteractEvent.EntityInteract event) {
        if (isServerSide(event)) {
            PlayerEntity player = event.getPlayer();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getEntity()));
            if (dimCache != null) {
                FlagCheckEvent flagCheckEvent = checkEvent(event.getTarget().blockPosition(), USE_ENTITIES, dimCache.getDimensionalRegion(), player);
                handleAndSendMsg(event, flagCheckEvent);

                if (!hasEmptyHands(player)) {
                    FlagCheckEvent useItemCheck = checkEvent(event.getPos(), USE_ENTITIES, dimCache.getDimensionalRegion(), player);
                    handleAndSendMsg(event, useItemCheck);
                }
            }
        }
    }

    @SubscribeEvent
    public static void onEntityInteraction(PlayerInteractEvent.RightClickItem event) {
        if (isServerSide(event)) {
            PlayerEntity player = event.getPlayer();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getEntity()));
            if (dimCache != null) {
                if (!hasEmptyHands(player)) {
                    FlagCheckEvent flagCheckEvent = checkEvent(event.getPos(), USE_ENTITIES, dimCache.getDimensionalRegion(), player);
                    handleAndSendMsg(event, flagCheckEvent);
                }

                FlagCheckEvent flagCheckEvent = checkEvent(event.getPos(), USE_ITEMS, dimCache.getDimensionalRegion(), player);
                handleAndSendMsg(event, flagCheckEvent);
            }
        }
    }

    /**
     * TODO: This is difficult to test. Do it.
     *
     * @param event
     */
    @SubscribeEvent
    public static void onSteppedOnActivator(BlockEvent.NeighborNotifyEvent event) {
        if (isServerSide(event)) {
            World world = (World) event.getWorld();
            Block block = event.getWorld().getBlockState(event.getPos()).getBlock();
            BlockPos pos = event.getPos();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(world.dimension());
            if (dimCache != null) {
                if (block instanceof AbstractPressurePlateBlock) {
                    AxisAlignedBB areaAbovePressurePlate = new AxisAlignedBB(pos.getX() - 1, pos.getY(), pos.getZ() - 1, pos.getX() + 1, pos.getY() + 2, pos.getZ() + 1);
                    List<PlayerEntity> players = ((World) event.getWorld()).getEntities(EntityType.PLAYER, areaAbovePressurePlate, (player) -> true);
                    boolean isCanceledForOne = false;
                    for (PlayerEntity player : players) {
                        FlagCheckEvent flagCheckEvent = checkEvent(event.getPos(), USE_BLOCKS, dimCache.getDimensionalRegion(), player);
                        isCanceledForOne = isCanceledForOne || handleAndSendMsg(event, flagCheckEvent);
                        event.setCanceled(isCanceledForOne);
                    }

                }
            }
        }
    }

    /**
     * Note: Does not prevent from fluids generate additional blocks (cobble generator). Use BlockEvent.FluidPlaceBlockEvent for this
     */
    @SubscribeEvent
    public static void onBucketFill(FillBucketEvent event) {
        // Note: FilledBucket seems to always be null. use maxStackSize to determine bucket state (empty or filled)
        if (isServerSide(event)) {
            PlayerEntity player = event.getPlayer();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getPlayer()));
            if (dimCache != null && event.getTarget() != null) {
                RayTraceResult pos = event.getTarget();
                BlockPos targetPos = new BlockPos(event.getTarget().getLocation());
                // MaxStackSize: 1 -> full bucket so only placeable; >1 -> empty bucket, only fillable
                int bucketItemMaxStackCount = event.getEmptyBucket().getMaxStackSize();
                // placing fluid
                if (bucketItemMaxStackCount == 1) {
                    FlagCheckEvent flagCheckEvent = checkEvent(targetPos, PLACE_FLUIDS, dimCache.getDimensionalRegion(), player);
                    handleAndSendMsg(event, flagCheckEvent);
                }
                // scooping fluid (breaking fluid)
                if (bucketItemMaxStackCount > 1) {
                    boolean isWaterlogged = false;
                    boolean isFluid = false;
                    if (pos != null && pos.getType() == RayTraceResult.Type.BLOCK) {
                        BlockState blockState = event.getWorld().getBlockState(targetPos);
                        // check for waterlogged block
                        if (blockState.getBlock() instanceof IWaterLoggable) {
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
                            FlagCheckEvent flagCheckEvent = checkEvent(targetPos, SCOOP_FLUIDS, dimCache.getDimensionalRegion(), player);
                            handleAndSendMsg(event, flagCheckEvent);
                        }
                    }
                }
            }
        }
    }

    /**
     * TODO: Flag for team chat
     * Note: message received from server but not distributed to all clients
     */
    @SubscribeEvent
    public static void onSendChat(ServerChatEvent event) {
        if (event.getPlayer() != null) {
            ServerPlayerEntity player = event.getPlayer();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getPlayer()));
            if (dimCache != null) {
                FlagCheckEvent flagCheckEvent = checkEvent(player.blockPosition(), SEND_MESSAGE, dimCache.getDimensionalRegion(), player);
                handleAndSendMsg(event, flagCheckEvent);
            }
        }
    }

    /**
     * TODO: add command list to block only specific commands, regardless of mod and permission of command
     */
    @SubscribeEvent
    public static void onCommandSend(CommandEvent event) {
        try {
            PlayerEntity player = event.getParseResults().getContext().getSource().getPlayerOrException();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(player));
            if (dimCache != null) {
                FlagCheckEvent flagCheckEvent = checkEvent(player.blockPosition(), EXECUTE_COMMAND, dimCache.getDimensionalRegion(), player);
                handleAndSendMsg(event, flagCheckEvent);
            }
        } catch (CommandSyntaxException e) {
            // Most likely thrown because command was not send by a player.
            // This is fine because we don't want this flag to be triggered from non-players entities
        }
    }

    // TODO: Flag to allow sleeping at daytime, by using event.setResult(Event.Result.ALLOW);
    @SubscribeEvent
    public static void onPlayerAttemptSleep(SleepingTimeCheckEvent event) {
        if (isServerSide(event)) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getPlayer()));
            if (dimCache != null) {
                PlayerEntity player = event.getPlayer();
                event.getSleepingLocation().ifPresent((pos) -> {
                    FlagCheckEvent flagCheckEvent = checkEvent(pos, SLEEP, dimCache.getDimensionalRegion(), player);
                    // FIXME: Msg is default from sleep deny
                    if (flagCheckEvent.isDenied()) {
                        FlagMessageUtil.sendFlagMsg(flagCheckEvent);
                        event.setResult(Event.Result.DENY);
                    }
                });
            }
        }
    }

    @SubscribeEvent
    public static void onSetSpawn(PlayerSetSpawnEvent event) {
        if (isServerSide(event)) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getPlayer()));
            if (dimCache != null) {
                BlockPos newSpawn = event.getNewSpawn();
                PlayerEntity player = event.getPlayer();
                if (newSpawn != null) {
                    FlagCheckEvent flagCheckEvent = checkEvent(newSpawn, SET_SPAWN, dimCache.getDimensionalRegion(), player);
                    handleAndSendMsg(event, flagCheckEvent);
                }
            }
        }
    }

    /**
     * TODO: Check for duplication
     */
    @SubscribeEvent
    public static void onPlayerDropItem(ItemTossEvent event) {
        if (!event.getPlayer().getCommandSenderWorld().isClientSide) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getPlayer()));
            if (dimCache != null) {
                PlayerEntity player = event.getPlayer();
                FlagCheckEvent flagCheckEvent = checkEvent(event.getEntityItem().blockPosition(), ITEM_DROP, dimCache.getDimensionalRegion(), player);
                if (handleAndSendMsg(event, flagCheckEvent)) {
                    // FIXME: Does not proper refund items?
                    player.addItem(event.getEntityItem().getItem());
                }
            }
        }
    }

    /**
     * Idea: Flags for different animals to mount
     */
    @SubscribeEvent
    public static void onEntityMountAttempt(EntityMountEvent event) {
        if (isServerSide(event)) {
            Entity entityBeingMounted = event.getEntityBeingMounted();
            // TODO: could be mob that dismounts because entity being mounted dies?
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getEntityMounting()));
            if (dimCache != null) {
                if (event.getEntityMounting() instanceof PlayerEntity) {
                    PlayerEntity player = (PlayerEntity) event.getEntityMounting();
                    if (event.isMounting()) {
                        FlagCheckEvent flagCheckEvent = checkEvent(entityBeingMounted.blockPosition(), ANIMAL_MOUNTING, dimCache.getDimensionalRegion(), player);
                        handleAndSendMsg(event, flagCheckEvent);
                    }
                    if (event.isDismounting()) {
                        // FIXME: Canceling event breaks unmounting. Wait for 1.17 fix: https://bugs.mojang.com/browse/MC-202202
                        // FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player,  entityBeingMounted.blockPosition(), ANIMAL_UNMOUNTING, dimCache.getDimensionalRegion());
                        // handleAndSendMsg(event, flagCheckEvent);
                    }

                }
            }
        }
    }
}