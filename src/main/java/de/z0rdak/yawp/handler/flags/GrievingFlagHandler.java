package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.entity.Entity;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.boss.WitherEntity;
import net.minecraft.entity.boss.dragon.EnderDragonEntity;
import net.minecraft.entity.monster.CreeperEntity;
import net.minecraft.entity.monster.EndermanEntity;
import net.minecraft.entity.monster.ZombieEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraftforge.event.entity.living.LivingDestroyBlockEvent;
import net.minecraftforge.event.entity.living.LivingDropsEvent;
import net.minecraftforge.event.entity.living.LivingExperienceDropEvent;
import net.minecraftforge.event.world.BlockEvent;
import net.minecraftforge.event.world.ExplosionEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;

import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;

public class GrievingFlagHandler {

    @SubscribeEvent
    public static void onFarmLandTrampled(BlockEvent.FarmlandTrampleEvent event) {
        if (isServerSide(event.getEntity())) {
            Entity trampler = event.getEntity();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(trampler));
            DimensionalRegion dimRegion = dimCache.getDimensionalRegion();

            // cancel all trampling
            if (dimRegion.containsFlag(RegionFlag.TRAMPLE_FARMLAND)) {
                event.setCanceled(true);
                return;
            }
            // cancel only player trampling
            if (trampler instanceof PlayerEntity) {
                PlayerEntity player = (PlayerEntity) trampler;
                if (dimRegion.containsFlag(RegionFlag.TRAMPLE_FARMLAND_PLAYER) && !dimRegion.permits(player)) {
                    event.setCanceled(true);
                    MessageUtil.sendMessage(player, "message.event.world.trample_farmland");
                }
            } else {
                // cancel trampling by other entities
                if (dimRegion.containsFlag(RegionFlag.TRAMPLE_FARMLAND_OTHER)) {
                    event.setCanceled(true);
                }
            }
        }
    }

    @SubscribeEvent
    public static void onEntityDestroyBlock(LivingDestroyBlockEvent event){
        if (isServerSide(event)) {
            LivingEntity destroyer = event.getEntityLiving();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(destroyer));
            DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                if (dimRegion.containsFlag(RegionFlag.DRAGON_BLOCK_PROT) && destroyer instanceof EnderDragonEntity) {
                    event.setCanceled(true);
                    return;
                }
                if (dimRegion.containsFlag(RegionFlag.WITHER_BLOCK_PROT) && destroyer instanceof WitherEntity) {
                    event.setCanceled(true);
                    return;
                }
                if (dimRegion.containsFlag(RegionFlag.ZOMBIE_DOOR_PROT) && destroyer instanceof ZombieEntity) {
                    event.setCanceled(true);
                    return;
                }
        }
    }

    /**
     * TODO: Does this trigger for players?
     * Idea: Flag for player not dropping loot as member/owner?
     * @param event
     */
    @SubscribeEvent
    public static void onEntityDropLoot(LivingDropsEvent event){
        if (isServerSide(event)) {
            LivingEntity lootEntity = event.getEntityLiving();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(lootEntity));
            DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
            if (dimRegion.containsFlag(RegionFlag.LOOT_DROP)) {
                event.setCanceled(true);
            }
        }
    }


    @SubscribeEvent
    public static void onEntityXpDrop(LivingExperienceDropEvent event){
        if (isServerSide(event)) {
            PlayerEntity player = event.getAttackingPlayer();
            Entity entity = event.getEntity();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(player));
            DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
            boolean entityDroppingXpIsPlayer = event.getEntityLiving() instanceof PlayerEntity;

                // prevent all xp drops
                if (dimRegion.containsFlag(RegionFlag.XP_DROP_ALL)) {
                    if (entityDroppingXpIsPlayer) {
                        event.setCanceled(true);
                        return;
                    }
                    if (!dimRegion.permits(player)) {
                        event.setCanceled(true);
                        MessageUtil.sendMessage(player, "message.event.world.exp_drop.all");
                        return;
                    }
                }
                // prevent monster xp drop
                if (dimRegion.containsFlag(RegionFlag.XP_DROP_MONSTER) && isMonster(entity) && !dimRegion.permits(player)) {
                    event.setCanceled(true);
                        MessageUtil.sendMessage(player, "message.event.world.exp_drop.monsters");
                    return;
                }
                // prevent other entity xp drop (villagers, animals, ..)
                if (dimRegion.containsFlag(RegionFlag.XP_DROP_OTHER) && !isMonster(entity) && !entityDroppingXpIsPlayer) {
                    if (!dimRegion.permits(player)) {
                        event.setCanceled(true);
                            MessageUtil.sendMessage(player, "message.event.world.exp_drop.non_hostile");
                        return;
                }
            }
        }
    }

    @SubscribeEvent
    public static void onEndermanPlacingBlock(BlockEvent.EntityPlaceEvent event) {
        if (isServerSide(event)) {
            if (event.getEntity() != null && event.getEntity() instanceof EndermanEntity) {
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getEntity()));
                DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                if (dimRegion.containsFlag(RegionFlag.ENTITY_PLACE)) {
                    event.setCanceled(true);
                    YetAnotherWorldProtector.LOGGER.debug("Block placed by enderman denied!");
                }
            }
        }
    }


    /**
     *
     * Removes affected entities and/or blocks from the event list to protect them
     * @param event -
     */
    @SubscribeEvent
    public static void onExplosion(ExplosionEvent.Detonate event) {
        if (!event.getWorld().isClientSide) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(event.getWorld().dimension());
            DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
            if (dimRegion.containsFlag(RegionFlag.EXPLOSION_BLOCK)){
                event.getAffectedBlocks().clear();
            }
            if (dimRegion.containsFlag(RegionFlag.EXPLOSION_ENTITY)) {
                event.getAffectedEntities().clear();
            }
            //event.getAffectedBlocks().removeAll(filterExplosionAffectedBlocks(event, RegionFlag.EXPLOSION_BLOCK.flag));
            //event.getAffectedEntities().removeAll(filterAffectedEntities(event, RegionFlag.EXPLOSION_ENTITY.flag));

            if (event.getExplosion().getSourceMob() != null) {
                boolean explosionTriggeredByCreeper = (event.getExplosion().getSourceMob() instanceof CreeperEntity);
                if (!explosionTriggeredByCreeper) {
                    if (dimRegion.containsFlag(RegionFlag.EXPLOSION_OTHER_BLOCKS)){
                        event.getAffectedBlocks().clear();
                    }
                    if (dimRegion.containsFlag(RegionFlag.EXPLOSION_OTHER_ENTITY)) {
                        event.getAffectedEntities().clear();
                    }
                    //event.getAffectedBlocks().removeAll(filterExplosionAffectedBlocks(event, RegionFlag.EXPLOSION_OTHER_BLOCKS.flag));
                    //event.getAffectedEntities().removeAll(filterAffectedEntities(event, RegionFlag.EXPLOSION_OTHER_ENTITY.flag));

                }
                if (explosionTriggeredByCreeper) {
                    if (dimRegion.containsFlag(RegionFlag.EXPLOSION_CREEPER_BLOCK)){
                        event.getAffectedBlocks().clear();
                    }
                    if (dimRegion.containsFlag(RegionFlag.EXPLOSION_CREEPER_ENTITY)) {
                        event.getAffectedEntities().clear();
                    }
                    //event.getAffectedBlocks().removeAll(filterExplosionAffectedBlocks(event, RegionFlag.EXPLOSION_CREEPER_BLOCK.flag));
                    //event.getAffectedEntities().removeAll(filterAffectedEntities(event, RegionFlag.EXPLOSION_CREEPER_ENTITY.flag));
                }
            }
        }
    }

}
