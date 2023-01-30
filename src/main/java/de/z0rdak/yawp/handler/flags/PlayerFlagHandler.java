package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.fabricmc.fabric.api.entity.event.v1.EntitySleepEvents;
import net.fabricmc.fabric.api.entity.event.v1.ServerLivingEntityEvents;
import net.fabricmc.fabric.api.event.player.PlayerBlockBreakEvents;
import net.minecraft.block.BlockState;
import net.minecraft.block.entity.BlockEntity;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.damage.DamageSource;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.util.ActionResult;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

import static de.z0rdak.yawp.core.flag.RegionFlag.BREAK_BLOCKS;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;

/**
 * Contains flag handler for events directly related/cause to/by players.
 */
public final class PlayerFlagHandler {

 private PlayerFlagHandler() {
 }

 public static void registerEventHandler() {
  PlayerBlockBreakEvents.BEFORE.register(PlayerFlagHandler::onBreakBlock);
  EntitySleepEvents.ALLOW_SLEEPING.register(PlayerFlagHandler::onAllowSleeping); // ALLOW_BED
  // ALLOW_SLEEP_TIME
  // ALLOW_RESETTING_TIME
  EntitySleepEvents.ALLOW_NEARBY_MONSTERS.register(PlayerFlagHandler::onSleepingWithStrangers);
  EntitySleepEvents.ALLOW_SETTING_SPAWN.register(PlayerFlagHandler::onSettingSpawn);


  ServerLivingEntityEvents.ALLOW_DAMAGE.register(PlayerFlagHandler::onReceiveDmg);
  ServerLivingEntityEvents.ALLOW_DEATH.register(PlayerFlagHandler::onDeathblow);
  ServerLivingEntityEvents.AFTER_DEATH.register(PlayerFlagHandler::onDeath);

  // ServerEntityEvents.EQUIPMENT_CHANGE

 }

 private static void onDeath(LivingEntity entity, DamageSource damageSource) {

 }

 private static boolean onDeathblow(LivingEntity entity, DamageSource damageSource, float v) {
  return false;
 }

 private static boolean onReceiveDmg(LivingEntity entity, DamageSource dmgSource, float amount) {
  return false;
 }

 private static ActionResult onSleepingWithStrangers(PlayerEntity player, BlockPos blockPos, boolean vanillaResult) {
  return null;
 }

 private static boolean onSettingSpawn(PlayerEntity player, BlockPos blockPos) {
  return false;
 }

 private static PlayerEntity.SleepFailureReason onAllowSleeping(PlayerEntity player, BlockPos blockPos) {
  return null;
 }

 private static boolean onBreakBlock(World world, PlayerEntity player, BlockPos blockPos, BlockState blockState, BlockEntity blockEntity) {
  if (isServerSide(world)) {
   DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(world.getRegistryKey());
   if (dimCache != null) {
    FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, blockPos, BREAK_BLOCKS, dimCache.getDimensionalRegion());
    handleAndSendMsg(flagCheckEvent);
    return !flagCheckEvent.isDenied();
   } else {
    return true;
   }
  }
  return true;
 }

 /**
  * Prevents traditional attacks from players which use EntityPlayer.attackTargetEntityWithCurrentItem(Entity).

  public static void onAttackPlayer(AttackEntityEvent event) {
  if (isServerSide(event)) {
  if (event.getTarget() instanceof PlayerEntity target) {
  PlayerEntity attacker = event.getPlayer();
  RegistryKey<World> entityDim = getEntityDim(attacker);
     DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(entityDim);
     if (dimCache != null) {
     FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(attacker, target.getBlockPos(), MELEE_PLAYERS, dimCache.getDimensionalRegion());
     handleAndSendMsg(event, flagCheckEvent);
     }
     }
     }
     }

     /**
     * Prevents various entities from been attacked from a player. <br>
     * TODO: Flag for all entities

     public static void onAttackEntity(AttackEntityEvent event) {
     if (isServerSide(event)) {
     PlayerEntity player = event.getPlayer();
     Entity eventEntity = event.getTarget();
     RegistryKey<World> entityDim = getEntityDim(event.getPlayer());
     DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(entityDim);
     if (dimCache != null) {
     if (isAnimal(eventEntity)) {
     FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, eventEntity.getBlockPos(), MELEE_ANIMALS, dimCache.getDimensionalRegion());
     handleAndSendMsg(event, flagCheckEvent);
     return;
     }
     if (isMonster(eventEntity)) {
     FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, eventEntity.getBlockPos(), MELEE_MONSTERS, dimCache.getDimensionalRegion());
     handleAndSendMsg(event, flagCheckEvent);
     return;
     }
     if (event.getTarget() instanceof VillagerEntity) {
     FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, eventEntity.getBlockPos(), MELEE_VILLAGERS, dimCache.getDimensionalRegion());
     handleAndSendMsg(event, flagCheckEvent);
     return;
     }
     if (event.getTarget() instanceof WanderingTraderEntity) {
     FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, eventEntity.getBlockPos(), MELEE_WANDERING_TRADER, dimCache.getDimensionalRegion());
     handleAndSendMsg(event, flagCheckEvent);
     }
     }
     }
     }

     public static void onPickupItem(EntityItemPickupEvent event) {
     if (isServerSide(event)) {
     DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getPlayer()));
     if (dimCache != null) {
     FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(event.getPlayer(), event.getEntity().getBlockPos(), ITEM_PICKUP, dimCache.getDimensionalRegion());
     handleAndSendMsg(event, flagCheckEvent);
     }
     }
     }

     public static void onBreedingAttempt(BabyEntitySpawnEvent event) {
     PlayerEntity player = event.getCausedByPlayer();
     if (player != null) {
     if (!player.getWorld().isClient) {
     DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(player));
     if (dimCache != null) {
     if (event.getParentA() instanceof VillagerEntity) {
     // TODO: Test on Villagers and add extra flag
     }
     FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(event.getCausedByPlayer(), event.getParentB().getBlockPos(), ANIMAL_BREEDING, dimCache.getDimensionalRegion());
     handleAndSendMsg(event, flagCheckEvent);
     }
     }
     } else {
     // TODO: test if this is fired when animals are bred without player interaction (with mods?)
     }
     }

     /**
     * Note: maybe add flag for different tamable animals / non vanilla / etc

     public static void onAnimalTameAttempt(AnimalTameEvent event) {
     PlayerEntity player = event.getTamer();
     if (player != null) {
     if (!player.getWorld().isClient) {
     AnimalEntity animal = event.getAnimal();
     DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(player));
     if (dimCache != null) {
     FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, event.getAnimal().getBlockPos(), ANIMAL_TAMING, dimCache.getDimensionalRegion());
     handleAndSendMsg(event, flagCheckEvent);
     }
     }
     }
     }

     public static void onPlayerLevelChange(PlayerXpEvent.LevelChange event) {
     if (isServerSide(event)) {
     PlayerEntity player = event.getPlayer();
     DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getPlayer()));
     if (dimCache != null) {
     FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, event.getPlayer().getBlockPos(), LEVEL_FREEZE, dimCache.getDimensionalRegion());
     handleAndSendMsg(event, flagCheckEvent);
     }
     }
     }

     public static void onPlayerXPChange(PlayerXpEvent.XpChange event) {
     if (!event.getPlayer().getCommandSenderWorld().isClientSide) {
     PlayerEntity player = event.getPlayer();
     DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getPlayer()));
     if (dimCache != null) {
     FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, event.getPlayer().getBlockPos(), XP_FREEZE, dimCache.getDimensionalRegion());
     handleAndSendMsg(event, flagCheckEvent);
     if (flagCheckEvent.isDenied()) {
     // TODO: Test whether this is needed?
     event.setAmount(0);
     }
     }
     }
     }

     public static void onPlayerXpPickup(PlayerXpEvent.PickupXp event) {
     if (!event.getPlayer().getCommandSenderWorld().isClientSide) {
     PlayerEntity player = event.getPlayer();
     DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getPlayer()));
     if (dimCache != null) {
     FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, event.getPlayer().getBlockPos(), XP_PICKUP, dimCache.getDimensionalRegion());
     handleAndSendMsg(event, flagCheckEvent);
     if (flagCheckEvent.isDenied()) {
     event.getOrb().remove(Entity.RemovalReason.DISCARDED);
     }
     }
     }
     }

     public static void onPvpAction(LivingHurtEvent event) {
     if (isServerSide(event)) {
     Entity dmgSourceEntity = event.getSource().getDirectEntity();
     Entity hurtEntity = event.getEntityLiving();
     if (hurtEntity instanceof PlayerEntity && dmgSourceEntity instanceof PlayerEntity) {
     DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(dmgSourceEntity));
     if (dimCache != null) {
     PlayerEntity playerTarget = (PlayerEntity) hurtEntity;
     PlayerEntity playerSource = (PlayerEntity) dmgSourceEntity;
     FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(playerSource, playerTarget.getBlockPos(), NO_PVP, dimCache.getDimensionalRegion());
     handleAndSendMsg(event, flagCheckEvent);
     }
     }
     }
     }

     @TargetFocusedFlag(flag = INVINCIBLE)
     public static void onPlayerHurt(LivingHurtEvent event) {
     if (isServerSide(event)) {
     Entity hurtEntity = event.getEntityLiving();
     if (hurtEntity instanceof PlayerEntity) {
     DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(hurtEntity));
     if (dimCache != null) {
     PlayerEntity playerTarget = (PlayerEntity) hurtEntity;
     FlagCheckEvent invincibleFlagCheckEvent = checkTargetEvent(playerTarget.getBlockPos(), INVINCIBLE, dimCache.getDimensionalRegion());
     if (invincibleFlagCheckEvent.isDenied()) {
     event.setCanceled(true);
     }
     }
     }
     }
     }


     /* TODO: Is this test even necessary anymore?
      *   - There is already a PVP flag for onHurt in place
      *
     public static void onReceiveDmg(LivingDamageEvent event) {
     if (isServerSide(event)) {
     Entity dmgSourceEntity = event.getSource().getDirectEntity();
     if (dmgSourceEntity instanceof PlayerEntity) {
     DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(dmgSourceEntity));
     if (dimCache != null) {
     if (event.getEntityLiving() instanceof PlayerEntity dmgTarget) {
     PlayerEntity dmgSource = ((PlayerEntity) dmgSourceEntity);
     // another check for PVP - this does not prevent knock-back? but prevents dmg
     FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(dmgSource, dmgTarget.getBlockPos(), MELEE_PLAYERS, dimCache.getDimensionalRegion());
     handleAndSendMsg(event, flagCheckEvent);
     }
     }
     }
     }
     }

     @TargetFocusedFlag(flag = KNOCKBACK_PLAYERS)
     public static void onPlayerKnockback(LivingKnockBackEvent event) {
     if (isServerSide(event)) {
     if (event.getEntityLiving() instanceof PlayerEntity) {
     DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getEntity()));
     if (dimCache != null) {
     PlayerEntity dmgTarget = (PlayerEntity) event.getEntityLiving();
     FlagCheckEvent flagCheckEvent = checkTargetEvent(dmgTarget.getBlockPos(), KNOCKBACK_PLAYERS, dimCache.getDimensionalRegion());
     if (flagCheckEvent.isDenied()) {
     event.setCanceled(true);
     }
     }
     }
     }
     }

     public static void onPlayerPlaceBlock(BlockEvent.EntityPlaceEvent event) {
     if (isServerSide(event)) {
     if (event.getEntity() != null && event.getEntity() instanceof PlayerEntity player) {
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

     public static void onEntityBreak(AttackEntityEvent event) {
     if (isServerSide(event)) {
     Entity target = event.getTarget();
     PlayerEntity player = event.getPlayer();
     RegistryKey<World> entityDim = getEntityDim(event.getPlayer());
     DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(entityDim);
     if (dimCache != null) {
     List<? extends String> entities = FlagConfig.BREAK_FLAG_ENTITIES.get();
     boolean isBlockEntityCovered = entities.stream().anyMatch(entity -> {
     Identifier entityResourceLocation = new Identifier(entity);
     return target.getType().getRegistryName() != null && target.getType().getRegistryName().equals(entityResourceLocation);
     });
     if (isBlockEntityCovered) {
     FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, event.getTarget().getBlockPos(), BREAK_ENTITIES, dimCache.getDimensionalRegion());
     handleAndSendMsg(event, flagCheckEvent);
     }
     }
     }
     }

     public static void onExplosionStarted(ExplosionEvent.Start event) {
     if (!event.getWorld().isClientSide) {
     Explosion explosion = event.getExplosion();
     if (explosion.getSourceMob() instanceof PlayerEntity player) {
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

     public static void onPlayerUseEnderPearl(EntityTeleportEvent event) {
     if (isServerSide(event)) {
     DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getEntity()));
     if (dimCache != null) {
     // handle player teleportation using ender pearls
     if (event instanceof EntityTeleportEvent.EnderPearl enderPearlEvent) {
     PlayerEntity player = enderPearlEvent.getPlayer();

     FlagCheckEvent.PlayerFlagEvent enderPearlToRegionFlagCheck = checkPlayerEvent(player, new BlockPos(event.getTarget()), USE_ENDERPEARL_TO_REGION, dimCache.getDimensionalRegion());
     if (handleAndSendMsg(event, enderPearlToRegionFlagCheck)) {
     return;
     }

     FlagCheckEvent.PlayerFlagEvent enderPearlFromRegionFlagCheck = checkPlayerEvent(player, player.getBlockPos(), USE_ENDERPEARL_FROM_REGION, dimCache.getDimensionalRegion());
     if (handleAndSendMsg(event, enderPearlFromRegionFlagCheck)) {
     }

     /* FIXME: refund pearl - duplication bug with e.g. origins mod
     int count = player.getHeldItem(player.getActiveHand()).getCount();
     player.getHeldItem(player.getActiveHand()).setCount(count + 1);
     return;

     }
     }
     }
     }

     public static void onPlayerUseToolSecondary(BlockEvent.BlockToolModificationEvent event) {
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
     // TODO: Events for ToolActions
     if (event.getToolAction().equals(ToolActions.AXE_STRIP)) {
     flagCheckEvent = checkPlayerEvent(player, event.getPos(), AXE_STRIP, dimCache.getDimensionalRegion());
     handleAndSendMsg(event, flagCheckEvent);
     }
     if (event.getToolAction().equals(ToolActions.HOE_TILL)) {
     flagCheckEvent = checkPlayerEvent(player, event.getPos(), HOE_TILL, dimCache.getDimensionalRegion());
     handleAndSendMsg(event, flagCheckEvent);
     }
     if (event.getToolAction().equals(ToolActions.SHOVEL_FLATTEN)) {
     flagCheckEvent = checkPlayerEvent(player, event.getPos(), SHOVEL_PATH, dimCache.getDimensionalRegion());
     handleAndSendMsg(event, flagCheckEvent);
     }
     }
     }
     }

     /*
     TODO: separate
     flag for block interaction (without preventing block placement)
     flag for entity interaction

     @SubscribeEvent public static void onPlayerRightClickBlock(PlayerInteractEvent.RightClickBlock event) {
     if (isServerSide(event)) {
     PlayerEntity player = event.getPlayer();
     BlockEntity targetEntity = event.getWorld().getBlockEntity(event.getPos());
     DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getPlayer()));
     if (dimCache != null) {
     boolean isLockableTileEntity = targetEntity instanceof BaseContainerBlockEntity;
     boolean isEnderChest = targetEntity instanceof EnderChestBlockEntity;
     boolean isContainer = targetEntity instanceof LecternBlockEntity || isLockableTileEntity;

     // used to allow player to place blocks when shift clicking container or usable bock
     boolean playerHasNoBlocksToPlaceInHands = player.getItemInHand(InteractionHand.MAIN_HAND).getItem().equals(Items.AIR)
     && player.getItemInHand(InteractionHand.OFF_HAND).getItem().equals(Items.AIR);

     // TODO: Test with various modded chests
     if (targetEntity != null) {
     if (player.isShiftKeyDown() && playerHasNoBlocksToPlaceInHands || !player.isShiftKeyDown()) {
     FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player,  targetEntity.getBlockPos(), USE, dimCache.getDimensionalRegion());
     if (handleAndSendMsg(event, flagCheckEvent)){
     return;
     }
     }
     } else {
     BlockHitResult pos = event.getHitVec();
     if (pos != null && pos.getType() == HitResult.Type.BLOCK) {
     BlockPos bPos = pos.getBlockPos();
     if (player.isShiftKeyDown() && playerHasNoBlocksToPlaceInHands || !player.isShiftKeyDown()) {
     FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, bPos, USE, dimCache.getDimensionalRegion());
     if (handleAndSendMsg(event, flagCheckEvent)){
     return;
     }
     }
     }
     }

     BlockHitResult pos = event.getHitVec();
     if (pos != null && pos.getType() == HitResult.Type.BLOCK) {
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
     return target instanceof ButtonBlock ||
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
     }

     @SubscribeEvent public static void onAccessMinecartChest(PlayerInteractEvent.EntityInteract event) {
     if (isServerSide(event)) {
     PlayerEntity player = event.getPlayer();
     DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getPlayer()));
     if (dimCache != null) {
     boolean isMinecartContainer = event.getTarget() instanceof AbstractMinecartContainer;
     if (isMinecartContainer) {
     FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, event.getTarget().getBlockPos(), CONTAINER_ACCESS, dimCache.getDimensionalRegion());
     handleAndSendMsg(event, flagCheckEvent);
     }
     }
     }
     }

     /**
      * TODO: This is difficult to test. Do it.
      *
      * @param event

     @SubscribeEvent public static void onSteppedOnActivator(BlockEvent.NeighborNotifyEvent event) {
     if (isServerSide(event)) {
     Level world = (Level) event.getWorld();
     Block block = event.getWorld().getBlockState(event.getPos()).getBlock();
     BlockPos pos = event.getPos();
     DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(world.dimension());
     if (dimCache != null) {
     if (block instanceof BasePressurePlateBlock) {
     AABB areaAbovePressurePlate = new AABB(pos.getX() - 1, pos.getY(), pos.getZ() - 1, pos.getX() + 1, pos.getY() + 2, pos.getZ() + 1);
     List<PlayerEntity> players = event.getWorld().getEntities(EntityType.PLAYER, areaAbovePressurePlate, (player) -> true);
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

     @SubscribeEvent public static void onBucketFill(FillBucketEvent event) {
     // Note: FilledBucket seems to always be null. use maxStackSize to determine bucket state (empty or filled)
     if (isServerSide(event)) {
     PlayerEntity player = event.getPlayer();
     DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getPlayer()));
     if (dimCache != null && event.getTarget() != null) {
     HitResult pos = event.getTarget();
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
     if (pos != null && pos.getType() == HitResult.Type.BLOCK) {
     BlockState blockState = event.getWorld().getBlockState(targetPos);
     // check for waterlogged block
     if (blockState.getBlock() instanceof SimpleWaterloggedBlock) {
     isWaterlogged = blockState.getValue(BlockStateProperties.WATERLOGGED);
     }
     if (ForgeRegistries.FLUIDS.tags() != null) {
     isFluid = ForgeRegistries.FLUIDS.tags().getTagNames().anyMatch(tag -> blockState.getFluidState().is(tag));
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

     @SubscribeEvent public static void onSendChat(ServerChatEvent event) {
     if (event.getPlayer() != null) {
     ServerPlayerEntity player = event.getPlayer();
     DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getPlayer()));
     if (dimCache != null) {
     FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, player.getBlockPos(), SEND_MESSAGE, dimCache.getDimensionalRegion());
     handleAndSendMsg(event, flagCheckEvent);
     }
     }
     }

     /**
      * TODO: add command list to block only specific commands, regardless of mod and permission of command

     @SubscribeEvent public static void onCommandSend(CommandEvent event) {
     try {
     PlayerEntity player = event.getParseResults().getContext().getSource().getPlayerOrException();
     DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(player));
     if (dimCache != null) {
     FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, player.getBlockPos(), EXECUTE_COMMAND, dimCache.getDimensionalRegion());
     handleAndSendMsg(event, flagCheckEvent);
     }
     } catch (CommandSyntaxException e) {
     // Most likely thrown because command was not send by a player.
     // This is fine because we don't want this flag to be triggered from non-players entities
     }
     }

     // TODO: Flag to allow sleeping at daytime, by using event.setResult(Event.Result.ALLOW);
     @SubscribeEvent public static void onPlayerAttemptSleep(SleepingTimeCheckEvent event) {
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

     @SubscribeEvent public static void onSetSpawn(PlayerSetSpawnEvent event) {
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

     @SubscribeEvent public static void onPlayerDropItem(ItemTossEvent event) {
     if (!event.getPlayer().getCommandSenderWorld().isClientSide) {
     DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getPlayer()));
     if (dimCache != null) {
     PlayerEntity player = event.getPlayer();
     FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, event.getEntityItem().getBlockPos(), ITEM_DROP, dimCache.getDimensionalRegion());
     if (handleAndSendMsg(event, flagCheckEvent)) {
     // FIXME: Does not proper refund items?
     player.addItem(event.getEntityItem().getItem());
     }
     }
     }
     }

     /**
      * Idea: Flags for different animals to mount

     @SubscribeEvent public static void onEntityMountAttempt(EntityMountEvent event) {
     if (isServerSide(event)) {
     Entity entityBeingMounted = event.getEntityBeingMounted();
     // TODO: could be mob that dismounts because entity being mounted dies?
     DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getEntityMounting()));
     if (dimCache != null) {
     if (event.getEntityMounting() instanceof PlayerEntity player) {
     if (event.isMounting()) {
     FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, entityBeingMounted.getBlockPos(), ANIMAL_MOUNTING, dimCache.getDimensionalRegion());
     handleAndSendMsg(event, flagCheckEvent);
     }
     if (event.isDismounting()) {
     FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, entityBeingMounted.getBlockPos(), ANIMAL_UNMOUNTING, dimCache.getDimensionalRegion());
     handleAndSendMsg(event, flagCheckEvent);
     }
     }
     }
     }
     }
     */

}