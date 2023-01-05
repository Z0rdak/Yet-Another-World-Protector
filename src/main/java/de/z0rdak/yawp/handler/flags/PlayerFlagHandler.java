package de.z0rdak.yawp.handler.flags;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.config.server.FlagConfig;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.block.*;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityType;
import net.minecraft.entity.item.minecart.ContainerMinecartEntity;
import net.minecraft.entity.merchant.villager.VillagerEntity;
import net.minecraft.entity.merchant.villager.WanderingTraderEntity;
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
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.event.CommandEvent;
import net.minecraftforge.event.ServerChatEvent;
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

import static de.z0rdak.yawp.core.flag.RegionFlag.*;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;
import static net.minecraftforge.common.ToolType.*;
import static net.minecraftforge.fml.common.Mod.EventBusSubscriber.Bus.FORGE;

/**
 * Contains flag handler for events directly related/cause to/by players.
 */
@Mod.EventBusSubscriber(modid = YetAnotherWorldProtector.MODID, value = Dist.DEDICATED_SERVER, bus = FORGE)
public final class PlayerFlagHandler {

    private PlayerFlagHandler() {
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
                    FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(attacker, target.blockPosition(), MELEE_PLAYERS, dimCache.getDimensionalRegion());
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
                    FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, eventEntity.blockPosition(), MELEE_ANIMALS, dimCache.getDimensionalRegion());
                    handleAndSendMsg(event, flagCheckEvent);
                    return;
                }
                if (isMonster(eventEntity)) {
                    FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, eventEntity.blockPosition(), MELEE_MONSTERS, dimCache.getDimensionalRegion());
                    handleAndSendMsg(event, flagCheckEvent);
                    return;
                }
                if (event.getTarget() instanceof VillagerEntity) {
                    FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, eventEntity.blockPosition(), MELEE_VILLAGERS, dimCache.getDimensionalRegion());
                    handleAndSendMsg(event, flagCheckEvent);
                    return;
                }
                if (event.getTarget() instanceof WanderingTraderEntity) {
                    FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, eventEntity.blockPosition(), MELEE_WANDERING_TRADER, dimCache.getDimensionalRegion());
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
                FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(event.getPlayer(), event.getEntity().blockPosition(), ITEM_PICKUP, dimCache.getDimensionalRegion());
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
                    FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(event.getCausedByPlayer(), event.getParentB().blockPosition(), ANIMAL_BREEDING, dimCache.getDimensionalRegion());
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
                    FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, event.getAnimal().blockPosition(), ANIMAL_TAMING, dimCache.getDimensionalRegion());
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
                FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, event.getPlayer().blockPosition(), LEVEL_FREEZE, dimCache.getDimensionalRegion());
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
                FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, event.getPlayer().blockPosition(), XP_FREEZE, dimCache.getDimensionalRegion());
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
                FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, event.getPlayer().blockPosition(), XP_PICKUP, dimCache.getDimensionalRegion());
                handleAndSendMsg(event, flagCheckEvent);
                if (flagCheckEvent.isDenied()) {
                    event.getOrb().remove();
                }
            }
        }
    }

    /**
     * TODO: Separate flags for Villagers, Animals, Monsters, Player
     * TODO: These flags needs further testing
     */
    @SubscribeEvent
    @TargetFocusedFlag(flag = INVINCIBLE)
    public static void onHurt(LivingHurtEvent event) {
        if (isServerSide(event)) {
            Entity dmgSourceEntity = event.getSource().getDirectEntity();
            Entity hurtEntity = event.getEntityLiving();
            if (hurtEntity instanceof PlayerEntity && dmgSourceEntity instanceof PlayerEntity) {
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(dmgSourceEntity));
                if (dimCache != null) {
                    PlayerEntity playerTarget = (PlayerEntity) hurtEntity;
                    PlayerEntity playerSource = (PlayerEntity) dmgSourceEntity;

                    // another check for PVP - this does not prevent knock-back? but prevents dmg
                    FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(playerSource, playerTarget.blockPosition(), MELEE_PLAYERS, dimCache.getDimensionalRegion());
                    handleAndSendMsg(event, flagCheckEvent);

                    FlagCheckEvent.PlayerFlagEvent invincibleFlagCheckEvent = checkPlayerEvent(playerTarget, playerTarget.blockPosition(), INVINCIBLE, dimCache.getDimensionalRegion());
                    if (invincibleFlagCheckEvent.isDenied()) {
                        event.setCanceled(false);
                    }
                }
            }
        }
    }

    @SubscribeEvent
    @TargetFocusedFlag(flag = INVINCIBLE)
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
                        FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(dmgSource, dmgTarget.blockPosition(), MELEE_PLAYERS, dimCache.getDimensionalRegion());
                        handleAndSendMsg(event, flagCheckEvent);

                        FlagCheckEvent.PlayerFlagEvent invincibleFlagCheckEvent = checkPlayerEvent(dmgTarget, dmgTarget.blockPosition(), INVINCIBLE, dimCache.getDimensionalRegion());
                        if (invincibleFlagCheckEvent.isDenied()) {
                            event.setCanceled(false);
                        }
                    }
                }
            }
        }
    }

    @SubscribeEvent
    @TargetFocusedFlag(flag = KNOCKBACK_PLAYERS)
    public static void onPlayerKnockback(LivingKnockBackEvent event) {
        if (isServerSide(event)) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getEntity()));
            if (dimCache != null) {
                if (event.getEntityLiving() instanceof PlayerEntity) {
                    PlayerEntity dmgTarget = (PlayerEntity) event.getEntityLiving();

                    FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(dmgTarget, dmgTarget.blockPosition(), KNOCKBACK_PLAYERS, dimCache.getDimensionalRegion());
                    handleAndSendMsg(event, flagCheckEvent);

                    // another check for PVP - Prevents knockback
                    if (flagCheckEvent.isDenied()) {
                        event.setCanceled(false);
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
                FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, event.getPos(), BREAK_BLOCKS, dimCache.getDimensionalRegion());
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
                    FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, event.getPos(), PLACE_BLOCKS, dimCache.getDimensionalRegion());
                    handleAndSendMsg(event, flagCheckEvent);
                }
            }
        }
    }

    /**
     * TODO: Compound Flags for combining common flags? E.g. BREAK_BLOCKS && BREAK_ENTITIES
     */
    @SubscribeEvent(priority = EventPriority.HIGHEST)
    public static void onEntityBreak(AttackEntityEvent event) {
        if (isServerSide(event)) {
            Entity target = event.getTarget();
            PlayerEntity player = event.getPlayer();
            RegistryKey<World> entityDim = getEntityDim(event.getPlayer());
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(entityDim);
            if (dimCache != null) {
                List<? extends String> entities = FlagConfig.BREAK_FLAG_ENTITIES.get();
                boolean isBlockEntityCovered = entities.stream().anyMatch(entity -> {
                    ResourceLocation entityResourceLocation = new ResourceLocation(entity);
                    return target.getType().getRegistryName() != null && target.getType().getRegistryName().equals(entityResourceLocation);
                });
                if (isBlockEntityCovered) {
                    FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, event.getTarget().blockPosition(), BREAK_ENTITIES, dimCache.getDimensionalRegion());
                    handleAndSendMsg(event, flagCheckEvent);
                }
            }
        }
    }

    @SubscribeEvent
    public static void onExplosionStarted(ExplosionEvent.Start event) {
        if (!event.getWorld().isClientSide) {
            Explosion explosion = event.getExplosion();
            if (explosion.getSourceMob() instanceof PlayerEntity) {
                PlayerEntity player = (PlayerEntity) explosion.getSourceMob();
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(player));
                if (dimCache != null) {
                    FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, new BlockPos(explosion.getPosition()), IGNITE_EXPLOSIVES, dimCache.getDimensionalRegion());
                    handleAndSendMsg(event, flagCheckEvent);
                }
            } else {
                if (explosion.getSourceMob() == null) {
                    // ignited by e.g. dispenser
                    // TODO: Griefing/dedicated dispenser flag
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
                FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, event.getPos(), USE_BONEMEAL, dimCache.getDimensionalRegion());
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

                    FlagCheckEvent.PlayerFlagEvent enderPearlToRegionFlagCheck = checkPlayerEvent(player, new BlockPos(event.getTarget()), USE_ENDERPEARL_TO_REGION, dimCache.getDimensionalRegion());
                    if (handleAndSendMsg(event, enderPearlToRegionFlagCheck)) {
                        return;
                    }

                    FlagCheckEvent.PlayerFlagEvent enderPearlFromRegionFlagCheck = checkPlayerEvent(player, player.blockPosition(), USE_ENDERPEARL_FROM_REGION, dimCache.getDimensionalRegion());
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
                FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, event.getPos(), TOOL_SECONDARY_USE, dimCache.getDimensionalRegion());
                if (handleAndSendMsg(event, flagCheckEvent)) {
                    // FIXME: [next update]: how about all TOOL_SECONDARY_USE is denied but one of the following is allowed?
                    // this kind of check is not uncommon. See onPlayerRightClickBlock e.g.
                    return;
                }

                if (event.getToolType().equals(AXE)) {
                    flagCheckEvent = checkPlayerEvent(player, event.getPos(), AXE_STRIP, dimCache.getDimensionalRegion());
                    handleAndSendMsg(event, flagCheckEvent);
                }
                if (event.getToolType().equals(HOE)) {
                    flagCheckEvent = checkPlayerEvent(player, event.getPos(), HOE_TILL, dimCache.getDimensionalRegion());
                    handleAndSendMsg(event, flagCheckEvent);
                }
                if (event.getToolType().equals(SHOVEL)) {
                    flagCheckEvent = checkPlayerEvent(player, event.getPos(), SHOVEL_PATH, dimCache.getDimensionalRegion());
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
                boolean playerHasNoBlocksToPlaceInHands = player.getItemInHand(Hand.MAIN_HAND).getItem().equals(Items.AIR)
                        && player.getItemInHand(Hand.OFF_HAND).getItem().equals(Items.AIR);

                BlockRayTraceResult pos = event.getHitVec();
                if (pos != null && pos.getType() == RayTraceResult.Type.BLOCK) {
                    BlockPos bPos = pos.getBlockPos();
                    boolean isInteractableBlock = isInteractableBlock(event.getWorld().getBlockState(bPos).getBlock());
                    if (isInteractableBlock) {
                        if (player.isShiftKeyDown() && playerHasNoBlocksToPlaceInHands || !player.isShiftKeyDown()) {
                            FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, bPos, USE, dimCache.getDimensionalRegion());
                            handleAndSendMsg(event, flagCheckEvent);
                        }
                    }
                }
                // check for ender chest access
                if (isEnderChest) {
                    if (player.isShiftKeyDown() && playerHasNoBlocksToPlaceInHands || !player.isShiftKeyDown()) {
                        FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, targetEntity.getBlockPos(), ENDER_CHEST_ACCESS, dimCache.getDimensionalRegion());
                        handleAndSendMsg(event, flagCheckEvent);
                    }
                }
                // check for container access
                if (isContainer) {
                    if (player.isShiftKeyDown() && playerHasNoBlocksToPlaceInHands || !player.isShiftKeyDown()) {
                        FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, targetEntity.getBlockPos(), CONTAINER_ACCESS, dimCache.getDimensionalRegion());
                        handleAndSendMsg(event, flagCheckEvent);
                    }
                }
            }
        }
    }

    private static boolean isInteractableBlock(Block target) {
        return target instanceof AbstractButtonBlock ||
                target instanceof DoorBlock ||
                target instanceof TrapDoorBlock ||
                target instanceof LeverBlock ||
                target instanceof NoteBlock ||
                target instanceof FenceGateBlock ||
                target instanceof DaylightDetectorBlock ||
                target instanceof RedstoneDiodeBlock ||
                target instanceof LecternBlock ||
                target instanceof BeaconBlock ||
                target instanceof BrewingStandBlock;
    }

    @SubscribeEvent
    public static void onAccessMinecartChest(PlayerInteractEvent.EntityInteract event) {
        if (isServerSide(event)) {
            PlayerEntity player = event.getPlayer();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getPlayer()));
            if (dimCache != null) {
                boolean isMinecartContainer = event.getTarget() instanceof ContainerMinecartEntity;
                if (isMinecartContainer) {
                    FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, event.getTarget().blockPosition(), CONTAINER_ACCESS, dimCache.getDimensionalRegion());
                    handleAndSendMsg(event, flagCheckEvent);
                }
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
                        FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, event.getPos(), USE, dimCache.getDimensionalRegion());
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
                    FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, targetPos, PLACE_FLUIDS, dimCache.getDimensionalRegion());
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
                            FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, targetPos, SCOOP_FLUIDS, dimCache.getDimensionalRegion());
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
                FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, player.blockPosition(), SEND_MESSAGE, dimCache.getDimensionalRegion());
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
                FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, player.blockPosition(), EXECUTE_COMMAND, dimCache.getDimensionalRegion());
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
                    FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, pos, SLEEP, dimCache.getDimensionalRegion());
                    // FIXME: Msg is default from sleep deny
                    if (sendFlagDeniedMsg(flagCheckEvent)) {
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
                    FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, newSpawn, SET_SPAWN, dimCache.getDimensionalRegion());
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
                FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, event.getEntityItem().blockPosition(), ITEM_DROP, dimCache.getDimensionalRegion());
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
                        FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, entityBeingMounted.blockPosition(), ANIMAL_MOUNTING, dimCache.getDimensionalRegion());
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