package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.boss.enderdragon.EnderDragon;
import net.minecraft.world.entity.boss.wither.WitherBoss;
import net.minecraft.world.entity.monster.Creeper;
import net.minecraft.world.entity.monster.EnderMan;
import net.minecraft.world.entity.monster.Zombie;
import net.minecraft.world.entity.player.Player;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.event.entity.living.LivingDestroyBlockEvent;
import net.minecraftforge.event.entity.living.LivingDropsEvent;
import net.minecraftforge.event.entity.living.LivingExperienceDropEvent;
import net.minecraftforge.event.world.BlockEvent;
import net.minecraftforge.event.world.ExplosionEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;

import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;
import static net.minecraftforge.fml.common.Mod.EventBusSubscriber.Bus.FORGE;

@Mod.EventBusSubscriber(modid = YetAnotherWorldProtector.MODID, value = Dist.DEDICATED_SERVER, bus = FORGE)
public class GrievingFlagHandler {

    private GrievingFlagHandler() {
    }

    @SubscribeEvent
    public static void onFarmLandTrampled(BlockEvent.FarmlandTrampleEvent event) {
        if (isServerSide(event.getEntity())) {
            Entity trampler = event.getEntity();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(trampler));
            if (dimCache != null) {
                FlagCheckEvent flagCheckEvent = checkTargetEvent(event.getPos(), RegionFlag.TRAMPLE_FARMLAND, dimCache.getDimensionalRegion());
                event.setCanceled(flagCheckEvent.isDenied());
                if (event.isCanceled()) {
                    if (trampler instanceof Player) {
                        sendFlagDeniedMsg(flagCheckEvent, (Player) trampler);
                    }
                    return;
                }
                // cancel only player trampling
                if (trampler instanceof Player player) {
                    FlagCheckEvent.PlayerFlagEvent playerFlagCheckEvent = checkPlayerEvent(player, event.getPos(), RegionFlag.TRAMPLE_FARMLAND_PLAYER, dimCache.getDimensionalRegion());
                    handleAndSendMsg(event, playerFlagCheckEvent);
                } else {
                    // cancel for other entities
                    flagCheckEvent = checkTargetEvent(event.getPos(), RegionFlag.TRAMPLE_FARMLAND_OTHER, dimCache.getDimensionalRegion());
                    event.setCanceled(flagCheckEvent.isDenied());
                }
            }
        }
    }

    // TODO: Test
    @SubscribeEvent
    public static void onEntityDestroyBlock(LivingDestroyBlockEvent event) {
        if (isServerSide(event)) {
            LivingEntity destroyer = event.getEntityLiving();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(destroyer));
            if (dimCache != null) {
                DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                if (destroyer instanceof EnderDragon) {
                    FlagCheckEvent flagCheckEvent = checkTargetEvent(event.getPos(), RegionFlag.DRAGON_BLOCK_PROT, dimRegion);
                    event.setCanceled(flagCheckEvent.isDenied());
                    return;
                }
                if (destroyer instanceof WitherBoss) {
                    FlagCheckEvent flagCheckEvent = checkTargetEvent(event.getPos(), RegionFlag.WITHER_BLOCK_PROT, dimRegion);
                    event.setCanceled(flagCheckEvent.isDenied());
                    return;
                }
                if (destroyer instanceof Zombie) {
                    FlagCheckEvent flagCheckEvent = checkTargetEvent(event.getPos(), RegionFlag.ZOMBIE_DOOR_PROT, dimRegion);
                    event.setCanceled(flagCheckEvent.isDenied());
                }
            }
        }
    }

    /**
     * Idea: Flag for player not dropping loot as member/owner? -> local keepInventory
     */
    @SubscribeEvent
    public static void onEntityDropLoot(LivingDropsEvent event) {
        if (isServerSide(event)) {
            LivingEntity lootEntity = event.getEntityLiving();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(lootEntity));
            if (dimCache != null) {
                FlagCheckEvent flagCheckEvent = checkTargetEvent(lootEntity.blockPosition(), RegionFlag.DROP_LOOT_ALL, dimCache.getDimensionalRegion());
                event.setCanceled(flagCheckEvent.isDenied());
                if (event.isCanceled()) {
                    return;
                }
                if (isPlayer(event.getSource().getEntity())) {
                    FlagCheckEvent.PlayerFlagEvent playerFlagCheckEvent = checkPlayerEvent((Player) event.getSource().getEntity(), lootEntity.blockPosition(), RegionFlag.DROP_LOOT_PLAYER, dimCache.getDimensionalRegion());
                    handleAndSendMsg(event, playerFlagCheckEvent);
                }
            }
        }
    }

    @SubscribeEvent
    public static void onEntityXpDrop(LivingExperienceDropEvent event) {
        if (isServerSide(event)) {
            Player player = event.getAttackingPlayer();
            Entity xpDroppingEntity = event.getEntityLiving();
            if (player != null) {
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(player));
                if (dimCache != null) {
                    DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                    // prevent all xp drop
                    FlagCheckEvent flagCheckEvent = checkTargetEvent(xpDroppingEntity.blockPosition(), RegionFlag.XP_DROP_ALL, dimRegion);
                    event.setCanceled(flagCheckEvent.isDenied());
                    if (event.isCanceled()) {
                        return;
                    }
                    if (event.getAttackingPlayer() != null) {
                        // prevent non-member/owner players from dropping xp by killing mobs
                        FlagCheckEvent.PlayerFlagEvent playerFlagCheckEvent = checkPlayerEvent(player, xpDroppingEntity.blockPosition(), RegionFlag.XP_DROP_PLAYER, dimCache.getDimensionalRegion());
                        if (handleAndSendMsg(event, playerFlagCheckEvent)) {
                            return;
                        }
                    }
                    // prevent monster xp drop
                    if (isMonster(xpDroppingEntity)) {
                        flagCheckEvent = checkTargetEvent(xpDroppingEntity.blockPosition(), RegionFlag.XP_DROP_MONSTER, dimRegion);
                        event.setCanceled(flagCheckEvent.isDenied());
                    } else {
                        // prevent other entity xp drop (villagers, animals, ..)
                        flagCheckEvent = checkTargetEvent(xpDroppingEntity.blockPosition(), RegionFlag.XP_DROP_OTHER, dimRegion);
                        event.setCanceled(flagCheckEvent.isDenied());
                    }
                }
            }
        }
    }

    @SubscribeEvent
    public static void onEndermanPlacingBlock(BlockEvent.EntityPlaceEvent event) {
        if (isServerSide(event)) {
            if (event.getEntity() instanceof EnderMan) {
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getEntity()));
                if (dimCache != null) {
                    FlagCheckEvent flagCheckEvent = checkTargetEvent(event.getEntity().blockPosition(), RegionFlag.ENTITY_PLACE, dimCache.getDimensionalRegion());
                    event.setCanceled(flagCheckEvent.isDenied());
                }
            }
        }
    }

    /**
     * FIXME or disable for next update
     * Removes affected entities and/or blocks from the event list to protect them
     */
    @SubscribeEvent
    public static void onExplosion(ExplosionEvent.Detonate event) {
        if (!event.getWorld().isClientSide) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(event.getWorld().dimension());
            if (dimCache != null && dimCache.getDimensionalRegion().isActive()) {
                DimensionalRegion dimRegion = dimCache.getDimensionalRegion();

                if (dimRegion.containsFlag(RegionFlag.EXPLOSION_BLOCK)) {
                    event.getAffectedBlocks().clear();
                }
                if (dimRegion.containsFlag(RegionFlag.EXPLOSION_ENTITY)) {
                    event.getAffectedEntities().clear();
                }
                //event.getAffectedBlocks().removeAll(filterExplosionAffectedBlocks(event, RegionFlag.EXPLOSION_BLOCK));
                //event.getAffectedEntities().removeAll(filterAffectedEntities(event, RegionFlag.EXPLOSION_ENTITY));

                if (event.getExplosion().getSourceMob() != null) {
                    boolean explosionTriggeredByCreeper = (event.getExplosion().getSourceMob() instanceof Creeper);
                    if (!explosionTriggeredByCreeper) {
                        if (dimRegion.containsFlag(RegionFlag.EXPLOSION_OTHER_BLOCKS)) {
                            event.getAffectedBlocks().clear();
                        }
                        if (dimRegion.containsFlag(RegionFlag.EXPLOSION_OTHER_ENTITY)) {
                            event.getAffectedEntities().clear();
                        }
                        //event.getAffectedBlocks().removeAll(filterExplosionAffectedBlocks(event, RegionFlag.EXPLOSION_OTHER_BLOCKS));
                        //event.getAffectedEntities().removeAll(filterAffectedEntities(event, RegionFlag.EXPLOSION_OTHER_ENTITY));

                    }
                    if (explosionTriggeredByCreeper) {
                        if (dimRegion.containsFlag(RegionFlag.EXPLOSION_CREEPER_BLOCK)) {
                            event.getAffectedBlocks().clear();
                        }
                        if (dimRegion.containsFlag(RegionFlag.EXPLOSION_CREEPER_ENTITY)) {
                            event.getAffectedEntities().clear();
                        }
                        //event.getAffectedBlocks().removeAll(filterExplosionAffectedBlocks(event, RegionFlag.EXPLOSION_CREEPER_BLOCK));
                        //event.getAffectedEntities().removeAll(filterAffectedEntities(event, RegionFlag.EXPLOSION_CREEPER_ENTITY));
                    }
                }
            }
        }
    }

}
