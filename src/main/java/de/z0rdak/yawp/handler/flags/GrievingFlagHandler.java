package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.entity.Entity;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.boss.WitherEntity;
import net.minecraft.entity.boss.dragon.EnderDragonEntity;
import net.minecraft.entity.monster.ZombieEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraftforge.event.entity.living.LivingDestroyBlockEvent;
import net.minecraftforge.event.entity.living.LivingDropsEvent;
import net.minecraftforge.event.entity.living.LivingExperienceDropEvent;
import net.minecraftforge.event.world.BlockEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;

import static de.z0rdak.yawp.handler.flags.HandlerUtil.getEntityDim;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.isMonster;

public class GrievingFlagHandler {

    @SubscribeEvent
    public static void onFarmLandTrampled(BlockEvent.FarmlandTrampleEvent event) {
        if (!event.getWorld().isClientSide()) {
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
        if (!event.getEntityLiving().level.isClientSide) {
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
        LivingEntity lootEntity = event.getEntityLiving();
        if (!lootEntity.level.isClientSide()) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(lootEntity));
            DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
            if (dimRegion.containsFlag(RegionFlag.LOOT_DROP)) {
                event.setCanceled(true);
            }
        }
    }


    @SubscribeEvent
    public static void onEntityXpDrop(LivingExperienceDropEvent event){
        if (!event.getEntityLiving().level.isClientSide()) {
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


}
