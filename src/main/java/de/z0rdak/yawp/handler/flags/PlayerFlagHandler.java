package de.z0rdak.yawp.handler.flags;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.block.BlockState;
import net.minecraft.block.IWaterLoggable;
import net.minecraft.entity.Entity;
import net.minecraft.entity.merchant.villager.VillagerEntity;
import net.minecraft.entity.merchant.villager.WanderingTraderEntity;
import net.minecraft.entity.monster.CreeperEntity;
import net.minecraft.entity.passive.AnimalEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.fluid.Fluid;
import net.minecraft.state.properties.BlockStateProperties;
import net.minecraft.tags.FluidTags;
import net.minecraft.tags.ITag;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.RayTraceResult;
import net.minecraft.util.math.vector.Vector3d;
import net.minecraft.util.text.TranslationTextComponent;
import net.minecraft.world.World;
import net.minecraftforge.common.ToolType;
import net.minecraftforge.event.CommandEvent;
import net.minecraftforge.event.ServerChatEvent;
import net.minecraftforge.event.entity.EntityMountEvent;
import net.minecraftforge.event.entity.item.ItemTossEvent;
import net.minecraftforge.event.entity.living.AnimalTameEvent;
import net.minecraftforge.event.entity.living.BabyEntitySpawnEvent;
import net.minecraftforge.event.entity.living.EntityTeleportEvent;
import net.minecraftforge.event.entity.player.*;
import net.minecraftforge.event.world.BlockEvent;
import net.minecraftforge.event.world.ExplosionEvent;
import net.minecraftforge.eventbus.api.Event;
import net.minecraftforge.eventbus.api.SubscribeEvent;

import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;

/**
 * Contains flag handler for events directly related/cause to/by players.
 */
public final class PlayerFlagHandler {

    private PlayerFlagHandler(){}

    @SubscribeEvent
    public static void onAttackPlayer(AttackEntityEvent event) {
        if (!event.getPlayer().getCommandSenderWorld().isClientSide) {
            if (event.getTarget() instanceof PlayerEntity) {
                PlayerEntity target = (PlayerEntity) event.getTarget();
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getPlayer()));
                if (dimCache.getDimensionalRegion().containsFlag(RegionFlag.ATTACK_PLAYERS) && !dimCache.hasMember(event.getPlayer())){
                    event.setCanceled(true);
                    MessageUtil.sendMessage(event.getPlayer(), "flag.msg.event.player.pvp");
                }
            }
        }
    }

    /**
     *
     * Idea: Flag for all entities?
     * @param event
     */
    @SubscribeEvent
    public static void onAttackEntity(AttackEntityEvent event) {
        PlayerEntity player = event.getPlayer();
        Entity eventEntity = event.getTarget();
        DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getPlayer()));
        DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
        if (!event.getTarget().getCommandSenderWorld().isClientSide) {
            if (isAnimal(eventEntity)) {
                if (dimRegion.containsFlag(RegionFlag.ATTACK_ANIMALS) && !dimRegion.permits(player)) {
                    MessageUtil.sendMessage(player, new TranslationTextComponent("message.event.mobs.hurt_animal"));
                    event.setCanceled(true);
                }
            }
            if (isMonster(eventEntity)) {
                if (dimRegion.containsFlag(RegionFlag.ATTACK_MONSTERS) && !dimRegion.permits(player)) {
                    MessageUtil.sendMessage(player, new TranslationTextComponent("message.event.mobs.hurt_monster"));
                    event.setCanceled(true);
                }
            }
            if (event.getTarget() instanceof VillagerEntity) {
                if (dimRegion.containsFlag(RegionFlag.ATTACK_VILLAGERS)  && !dimRegion.permits(player)) {
                    MessageUtil.sendMessage(player, new TranslationTextComponent("message.event.mobs.hurt_villager"));
                    event.setCanceled(true);
                }
            }
            if (event.getTarget() instanceof WanderingTraderEntity) {
                if (dimRegion.containsFlag(RegionFlag.ATTACK_WANDERING_TRADER)  && !dimRegion.permits(player)) {
                    MessageUtil.sendMessage(player, new TranslationTextComponent("message.event.mobs.hurt_villager"));
                    event.setCanceled(true);
                }
            }
        }
    }

/*
    // unrelated: mobs pickup logic => MobEntity#livingTick
    @SubscribeEvent
    public static void onPickupItem(EntityItemPickupEvent event) {
        if (!event.getPlayer().getCommandSenderWorld().isClientSide) {
            List<IRegion> regions = RegionUtils.getHandlingRegionsFor(event.getPlayer().getPosition(), event.getPlayer().world);
            for (IRegion region : regions) {
                if (region.containsFlag(RegionFlag.ITEM_PICKUP.toString()) && region.forbids(event.getPlayer())) {
                    if (!region.isMuted()) {
                        sendStatusMessage(event.getPlayer(), "message.event.player.pickup_item");
                    }
                    event.setCanceled(true);
                }
            }
        }
    }



 */

    @SubscribeEvent
    public static void onBreedingAttempt(BabyEntitySpawnEvent event) {
        PlayerEntity player = event.getCausedByPlayer();
        if (player != null) {
            if (!player.getCommandSenderWorld().isClientSide) {
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(player));
                DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                if (dimRegion.containsFlag(RegionFlag.ANIMAL_BREEDING) && !dimRegion.permits(player)) {
                    MessageUtil.sendMessage(player, "message.event.mobs.breed_animals");
                    event.setCanceled(true);
                    return;
                }

                if (event.getParentA() instanceof VillagerEntity) {
                    // TODO: Test on Villagers and add extra flag
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
        PlayerEntity player = event.getTamer();
        if (!player.getCommandSenderWorld().isClientSide) {
            AnimalEntity animal = event.getAnimal();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(player));
            DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
            if (dimRegion.containsFlag(RegionFlag.ANIMAL_TAMING) && !dimRegion.permits(player)) {
                event.setCanceled(true);
                MessageUtil.sendMessage(player, "message.event.mobs.tame_animal");
            }
        }
    }

    @SubscribeEvent
    public static void onPlayerLevelChange(PlayerXpEvent.LevelChange event) {
        if (!event.getPlayer().getCommandSenderWorld().isClientSide) {
            PlayerEntity player = event.getPlayer();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(player));
            DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
            if (dimRegion.containsFlag(RegionFlag.LEVEL_FREEZE) && !dimRegion.permits(player)) {
                MessageUtil.sendMessage(player, "message.event.player.level_freeze");
                event.setCanceled(true);
            }
        }
    }

    @SubscribeEvent
    public static void onPlayerXPChange(PlayerXpEvent.XpChange event) {
        if (!event.getPlayer().getCommandSenderWorld().isClientSide) {
            PlayerEntity player = event.getPlayer();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(player));
            DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
            if (dimRegion.containsFlag(RegionFlag.XP_FREEZE) && !dimRegion.permits(player)) {
                MessageUtil.sendMessage(player, "message.protection.player.xp_freeze");
                event.setCanceled(true);
                event.setAmount(0);
            }
        }
    }

    @SubscribeEvent
    public static void onPlayerXpPickup(PlayerXpEvent.PickupXp event) {
        if (!event.getPlayer().getCommandSenderWorld().isClientSide) {
            PlayerEntity player = event.getPlayer();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(player));
            DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
            if (dimRegion.containsFlag(RegionFlag.XP_PICKUP) && !dimRegion.permits(player)) {
                MessageUtil.sendMessage(player, "message.protection.player.xp_pickup");
                event.setCanceled(true);
                event.getOrb().remove();
            }
        }
    }


/*
    // TODO: handle flags for Villagers, Animals, Monsters, Player separate
    @SubscribeEvent
    public static void onHurt(LivingHurtEvent event) {
        Entity dmgSourceEntity = event.getSource().getTrueSource();
        if (event.getEntityLiving() instanceof PlayerEntity) {
            PlayerEntity dmgTargetEntity = (PlayerEntity) event.getEntityLiving();
            List<IRegion> regions = RegionUtils.getHandlingRegionsFor(dmgTargetEntity.getPosition(), dmgTargetEntity.world);
            for (IRegion region : regions) {
                // another check for PVP - this does not prevent knochback? but prevents dmg
                if (dmgSourceEntity instanceof PlayerEntity && region.containsFlag(RegionFlag.ATTACK_PLAYERS)){
                    event.setCanceled(true);
                    return;
                }
                if (region.containsFlag(RegionFlag.INVINCIBLE.toString())) {
                    event.setCanceled(true);
                    return;
                }

            }
        }
    }

    @SubscribeEvent
    public static void onReceiveDmg(LivingDamageEvent event) {
        Entity dmgSourceEntity = event.getSource().getTrueSource();
        if (event.getEntityLiving() instanceof PlayerEntity) {
            PlayerEntity player = (PlayerEntity) event.getEntityLiving();
            List<IRegion> regions = RegionUtils.getHandlingRegionsFor(player.getPosition(), player.world);
            for (IRegion region : regions) {
                // another check for PVP - this does not prevent knochback? but prevents dmg
                if (dmgSourceEntity instanceof PlayerEntity && region.containsFlag(RegionFlag.ATTACK_PLAYERS)){
                    event.setCanceled(true);
                    return;
                }
                if (region.containsFlag(RegionFlag.INVINCIBLE.toString())) {
                    event.setCanceled(true);
                    return;
                }
            }
        }
    }

    @SubscribeEvent
    public static void onHurtKnockback(LivingKnockBackEvent event) {
        if (event.getEntityLiving() instanceof PlayerEntity) {
            PlayerEntity dmgTargetEntity = (PlayerEntity) event.getEntityLiving();
            List<IRegion> regions = RegionUtils.getHandlingRegionsFor(dmgTargetEntity.getPosition(), dmgTargetEntity.world);
            for (IRegion region : regions) {
                // another check for PVP - Prevents knockback
                if (region.containsFlag(RegionFlag.ATTACK_PLAYERS.toString())) {
                    event.setCanceled(true);
                    return;
                }
            }
            // TODO: Flag for knockback?
        }
    }

    @SubscribeEvent
    public static void onPlayerBreakBlock(BlockEvent.BreakEvent event) {
        if (!event.getWorld().isRemote()) {
            PlayerEntity player = event.getPlayer();
            List<IRegion> regions = RegionUtils.getHandlingRegionsFor(event.getPos(), (World) event.getWorld());
            for (IRegion region : regions) {
                if (region.containsFlag(RegionFlag.BREAK) && region.forbids(player)) {
                    event.setCanceled(true);
                    if (!region.isMuted()) {
                        sendStatusMessage(player, new TranslationTextComponent("message.event.protection.break_block"));
                    }
                    return;
                }
            }
        }
    }
/*
    @SubscribeEvent
    public static void onPlayerPlaceBlock(BlockEvent.EntityPlaceEvent event) {
        if (!event.getWorld().isRemote()) {
            List<IRegion> regions = RegionUtils.getHandlingRegionsFor(event.getPos(), (World) event.getWorld());
            if (event.getEntity() instanceof PlayerEntity) {
                PlayerEntity player = (PlayerEntity) event.getEntity();
                for (IRegion region : regions) {
                    if (region.containsFlag(RegionFlag.PLACE) && region.forbids(player)) {
                        event.setCanceled(true);
                        if (!region.isMuted()) {
                            sendStatusMessage(player, new TranslationTextComponent("message.event.protection.place_block"));
                        }
                    }
                }
            }
            // TODO: Test and move to griefing
            if (event.getEntity() instanceof EndermanEntity) {
                boolean endermanPlacingProhibited = regions.stream()
                        .anyMatch(region -> region.containsFlag(RegionFlag.ENTITY_PLACE));
                if (endermanPlacingProhibited) {
                    event.setCanceled(true);
                    WorldProtector.LOGGER.debug("Block placed by enderman denied!");
                }
            }
        }
    }

    @SubscribeEvent
    public static void onExplosionStarted(ExplosionEvent.Start event) {
        if (!event.getWorld().isRemote) {
            List<IRegion> regions = RegionUtils.getHandlingRegionsFor(new BlockPos(event.getExplosion().getPosition()), event.getWorld());
            if (event.getExplosion().getExplosivePlacedBy() instanceof PlayerEntity) {
                PlayerEntity player = (PlayerEntity) event.getExplosion().getExplosivePlacedBy();
                for (IRegion region : regions) {
                    boolean cancelEvent = region.containsFlag(RegionFlag.IGNITE_EXPLOSIVES) && !region.permits(player);
                    event.setCanceled(cancelEvent);
                    if (cancelEvent) {
                        boolean isPlayerExploder = event.getExplosion().getExploder() instanceof PlayerEntity;
                        if (!region.isMuted() && isPlayerExploder) {
                            sendStatusMessage(player, "message.event.protection.ignite_tnt");
                        }
                    }
                }
            } else {
                // TODO: Explosion triggered by projectile or other TNT, or [.?.] -> Griefing
            }
        }
    }
*/

    @SubscribeEvent
    public static void onBonemealUse(BonemealEvent event) {
        if (!event.getWorld().isClientSide) {
            PlayerEntity player = (PlayerEntity) event.getEntity();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(player));
            DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
            if (dimRegion.containsFlag(RegionFlag.USE_BONEMEAL) && !dimRegion.permits(player)) {
                event.setCanceled(true);
                MessageUtil.sendMessage(player, "message.event.world.use_bone_meal");
                return;
            }
        }
    }

    @SubscribeEvent
    public static void onPlayerUseEnderPearl(EntityTeleportEvent event) {
        World world = event.getEntity().level;
        if (!world.isClientSide) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getEntity()));
            DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
            // handle player teleportation using ender pearls
            if (event instanceof EntityTeleportEvent.EnderPearl) {
                EntityTeleportEvent.EnderPearl enderPearlEvent = (EntityTeleportEvent.EnderPearl) event;
                ServerPlayerEntity player = enderPearlEvent.getPlayer();
                if ((dimRegion.containsFlag(RegionFlag.USE_ENDERPEARL_FROM_REGION)
                        || dimRegion.containsFlag(RegionFlag.ENDERMAN_TELEPORT_TO_REGION))
                        && !dimRegion.permits(player)) {
                    event.setCanceled(true);
                    MessageUtil.sendMessage(player, "message.event.teleport.ender_pearl.from_region");
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

    /**
     * TODO: Rework for dimensions
     * TODO: Not pure player event - relocate
     * Removes affected entities and/or blocks from the event list to protect them
     * @param event -
     */
    @SubscribeEvent
    public static void onExplosion(ExplosionEvent.Detonate event) {
        if (!event.getWorld().isClientSide) {
            event.getAffectedBlocks().removeAll(filterExplosionAffectedBlocks(event, RegionFlag.EXPLOSION_BLOCK.flag));
            event.getAffectedEntities().removeAll(filterAffectedEntities(event, RegionFlag.EXPLOSION_ENTITY.flag));

            if (event.getExplosion().getSourceMob() != null) {
                boolean explosionTriggeredByCreeper = (event.getExplosion().getSourceMob() instanceof CreeperEntity);
                if (!explosionTriggeredByCreeper) {
                    event.getAffectedBlocks().removeAll(filterExplosionAffectedBlocks(event, RegionFlag.EXPLOSION_OTHER_BLOCKS.flag));
                    event.getAffectedEntities().removeAll(filterAffectedEntities(event, RegionFlag.EXPLOSION_OTHER_ENTITY.flag));
                }
                if (explosionTriggeredByCreeper) {
                    event.getAffectedBlocks().removeAll(filterExplosionAffectedBlocks(event, RegionFlag.EXPLOSION_CREEPER_BLOCK.flag));
                    event.getAffectedEntities().removeAll(filterAffectedEntities(event, RegionFlag.EXPLOSION_OTHER_ENTITY.flag));
                }
            }
        }
    }

    /**
     *
     * @param event
     */
    @SubscribeEvent
    public static void onPlayerUseToolSecondary(BlockEvent.BlockToolInteractEvent event) {
        if (!event.getWorld().isClientSide()) {
            PlayerEntity player = event.getPlayer();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(player));
            DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
            boolean playerNotPermitted = !dimRegion.permits(player);
            if (dimRegion.containsFlag(RegionFlag.TOOL_SECONDARY_USE) && playerNotPermitted) {
                event.setCanceled(true);
                MessageUtil.sendMessage(player, "message.event.protection.tool_secondary_use");
                return;
            }
            if (event.getToolType() == ToolType.AXE && dimRegion.containsFlag(RegionFlag.AXE_STRIP) && playerNotPermitted) {
                event.setCanceled(true);
                MessageUtil.sendMessage(player, "message.event.protection.strip_wood");
                return;
            }
            if (event.getToolType() == ToolType.HOE && dimRegion.containsFlag(RegionFlag.HOE_TILL) && playerNotPermitted) {
                event.setCanceled(true);
                MessageUtil.sendMessage(player, "message.event.protection.till_farmland");
                return;
            }
            if (event.getToolType() == ToolType.SHOVEL && dimRegion.containsFlag(RegionFlag.SHOVEL_PATH) && playerNotPermitted) {
                event.setCanceled(true);
                MessageUtil.sendMessage(player, "message.event.protection.shovel_path");
                return;
            }
        }
    }

    public static void onFluidBlockGeneration(BlockEvent.FluidPlaceBlockEvent event) {

    }

    /**
     * TODO: USe FillBucketEvent for buckets
     * Note: Does not prevent from fluids generate additional blocks (cobble generator). Use BlockEvent.FluidPlaceBlockEvent for this
     * @param event
     */
    @SubscribeEvent
    public static void onBucketFill(FillBucketEvent event) {
        // Note: FilledBucket seems to always be null. use maxStackSize to determine bucket state (empty or filled)
        PlayerEntity player = event.getPlayer();
        if (!event.getWorld().isClientSide) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(player));
            DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
            if (event.getTarget() != null) {
                // MaxStackSize: 1 -> full bucket so only placeable; >1 -> empty bucket, only fillable
                int bucketItemMaxStackCount = event.getEmptyBucket().getMaxStackSize();

                // placing fluid
                if (bucketItemMaxStackCount == 1) {
                    if (dimRegion.containsFlag(RegionFlag.PLACE_BLOCKS) && !dimRegion.permits(player)) {
                        MessageUtil.sendMessage(player, new TranslationTextComponent("message.event.protection.place_fluid"));
                        event.setCanceled(true);
                        return;
                    }
                }

                // scooping fluid (breaking fluid)
                if (bucketItemMaxStackCount > 1) {
                    boolean isWaterlogged = false;
                    boolean isFluid = false;
                    RayTraceResult pos = event.getTarget();
                    if (pos != null && pos.getType() == RayTraceResult.Type.BLOCK) {
                        Vector3d absPos = pos.getLocation();
                        BlockState blockState = event.getWorld().getBlockState(new BlockPos(absPos));
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
                            if (dimRegion.containsFlag(RegionFlag.BREAK_BLOCKS) && !dimRegion.permits(player)) {
                                MessageUtil.sendMessage(player, new TranslationTextComponent("message.event.protection.scoop_fluid"));
                                event.setCanceled(true);
                                return;
                            }
                        }
                    }
                }
            }
        }
    }



    /**
     * Note: message send to server but not distributed to all clients
     * @param event
     */
    @SubscribeEvent
    public static void onSendChat(ServerChatEvent event) {
        if (event.getPlayer() != null) {
            ServerPlayerEntity player = event.getPlayer();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(player));
            DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
            if (dimRegion.containsFlag(RegionFlag.SEND_MESSAGE) && !dimRegion.permits(player)) {
                event.setCanceled(true);
                MessageUtil.sendMessage(player, new TranslationTextComponent("message.event.player.speak"));
            }
        }
    }

    // Should only be send on server side
    @SubscribeEvent
    public static void onCommandSend(CommandEvent event) {
        try {
            PlayerEntity player = event.getParseResults().getContext().getSource().getPlayerOrException();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(player));
            DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
            if (dimRegion.containsFlag(RegionFlag.EXECUTE_COMMAND) && !dimRegion.permits(player)) {
                event.setCanceled(true);
                MessageUtil.sendMessage(player, "message.event.player.execute-commands");
                return;
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
        if (!event.getPlayer().getCommandSenderWorld().isClientSide) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getPlayer()));
            DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
            PlayerEntity player = event.getPlayer();
            if (dimRegion.containsFlag(RegionFlag.SLEEP) && !dimRegion.permits(player)) {
                MessageUtil.sendMessage(player, "message.event.player.sleep");
                event.setResult(Event.Result.DENY);
                return;
            }
        }
    }

    @SubscribeEvent
    public static void onSetSpawn(PlayerSetSpawnEvent event) {
        if (!event.getPlayer().getCommandSenderWorld().isClientSide) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getPlayer()));
            DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
            BlockPos newSpawn = event.getNewSpawn();
            PlayerEntity player = event.getPlayer();
            if (newSpawn != null) {
                // attempt to set spawn
                if (dimRegion.containsFlag(RegionFlag.SET_SPAWN) && !dimRegion.permits(player)) {
                    event.setCanceled(true);
                    MessageUtil.sendMessage(player, "message.event.player.set_spawn");
                    return;
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
     * @param event
     */
    @SubscribeEvent
    public static void onPlayerDropItem(ItemTossEvent event) {
        if (!event.getPlayer().getCommandSenderWorld().isClientSide){

        DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getPlayer()));
        DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
        PlayerEntity player = event.getPlayer();
            if (dimRegion.containsFlag(RegionFlag.ITEM_DROP) && !dimRegion.permits(player)) {
                event.setCanceled(true);
                player.inventory.add(event.getEntityItem().getItem());
                MessageUtil.sendMessage(player, "message.event.player.drop_item");
                return;
            }
    }}

    /**
     * Idea: Flags for different animals to mount
     *
     * @param event
     */
    @SubscribeEvent
    public static void onEntityMountAttempt(EntityMountEvent event) {
        if (!event.getWorldObj().isClientSide) {
            Entity entityBeingMounted = event.getEntityBeingMounted();
            // could be mob that dismounts because entity being mounted dies?
            boolean playerAttemptsMounting = event.getEntityMounting() instanceof PlayerEntity;
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getEntityMounting()));
            DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
            if (playerAttemptsMounting) {
                PlayerEntity player = (PlayerEntity) event.getEntityMounting();
					/*
					TODO: Wait for 1.17: https://bugs.mojang.com/browse/MC-202202
					if (event.isDismounting() && region.containsFlag(RegionFlag.ANIMAL_UNMOUNTING) && region.forbids(player)) {
						event.setCanceled(true); // Does not correctly unmount player
						MessageUtil.sendMessage(player, "message.event.player.unmount");
					}
                    */
                if (event.isMounting() && dimRegion.containsFlag(RegionFlag.ANIMAL_MOUNTING) && !dimRegion.permits(player)) {
                    event.setCanceled(true);
                    MessageUtil.sendMessage(player, "message.event.player.mount");
                }
            }
        }
    }
}
