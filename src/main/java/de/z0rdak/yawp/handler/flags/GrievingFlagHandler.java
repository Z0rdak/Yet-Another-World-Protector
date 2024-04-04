package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.api.events.region.FlagCheckResult;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.boss.enderdragon.EnderDragon;
import net.minecraft.world.entity.boss.wither.WitherBoss;
import net.minecraft.world.entity.monster.Creeper;
import net.minecraft.world.entity.monster.EnderMan;
import net.minecraft.world.entity.monster.Zombie;
import net.minecraft.world.entity.player.Player;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.entity.EntityMobGriefingEvent;
import net.minecraftforge.event.entity.living.LivingDestroyBlockEvent;
import net.minecraftforge.event.entity.living.LivingDropsEvent;
import net.minecraftforge.event.entity.living.LivingExperienceDropEvent;
import net.minecraftforge.event.world.BlockEvent;
import net.minecraftforge.event.world.ExplosionEvent;
import net.minecraftforge.eventbus.api.Event;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;

import java.util.Set;
import java.util.stream.Collectors;

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
                FlagCheckEvent flagCheckEvent = HandlerUtil.checkEvent(event.getPos(), RegionFlag.TRAMPLE_FARMLAND, dimCache.getDimensionalRegion());
                event.setCanceled(flagCheckEvent.isDenied());
                if (event.isCanceled()) {
                    if (trampler instanceof Player) {
                        sendFlagMsg(new PlayerFlagEvent(flagCheckEvent, (Player) trampler));
                    }
                    return;
                }
                // cancel only player trampling
                if (trampler instanceof Player player) {
                    FlagCheckEvent playerFlagCheckEvent = checkEvent(event.getPos(), RegionFlag.TRAMPLE_FARMLAND_PLAYER, dimCache.getDimensionalRegion(), player);
                    handleAndSendMsg(event, playerFlagCheckEvent);
                } else {
                    // cancel for other entities
                    flagCheckEvent = HandlerUtil.checkEvent(event.getPos(), RegionFlag.TRAMPLE_FARMLAND_OTHER, dimCache.getDimensionalRegion());
                    event.setCanceled(flagCheckEvent.isDenied());
                }
            }
        }
    }

    @SubscribeEvent
    public static void onEntityDestroyBlock(LivingDestroyBlockEvent event) {
        if (isServerSide(event)) {
            LivingEntity destroyer = event.getEntityLiving();
            BlockPos target = event.getPos();
            FlagCheckEvent checkEvent = null;
            if (destroyer instanceof EnderDragon) {
                checkEvent = new FlagCheckEvent(target, RegionFlag.DRAGON_BLOCK_PROT, getEntityDim(destroyer), null);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
            }
            if (destroyer instanceof WitherBoss) {
                checkEvent = new FlagCheckEvent(target, RegionFlag.WITHER_BLOCK_PROT, getEntityDim(destroyer), null);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
            }
            if (destroyer instanceof Zombie) {
                checkEvent = new FlagCheckEvent(target, RegionFlag.ZOMBIE_DOOR_PROT, getEntityDim(destroyer), null);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
            }
            if (checkEvent != null) {
                processCheck(checkEvent, null, denyResult -> {
                    event.setCanceled(true);
                });
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
            Player player = isPlayer(lootEntity) ? (Player) lootEntity : null;
            FlagCheckEvent checkEvent = new FlagCheckEvent(lootEntity.blockPosition(), RegionFlag.DROP_LOOT_ALL, event.getEntity().level.dimension(), player);
            if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                return;
            }
            FlagState flagState = processCheck(checkEvent, null, denyResult -> {
                event.setCanceled(true);
                sendFlagMsg(denyResult);
            });
            if (flagState == FlagState.DENIED)
                return;
            if (player != null) {
                FlagCheckEvent playerCheckEvent = new FlagCheckEvent(lootEntity.blockPosition(), RegionFlag.DROP_LOOT_PLAYER, player.level.dimension(), player);
                if (MinecraftForge.EVENT_BUS.post(playerCheckEvent)) {
                    return;
                }
                processCheck(playerCheckEvent, null, denyResult -> {
                    event.setCanceled(true);
                    sendFlagMsg(denyResult);
                });
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
                    FlagCheckEvent flagCheckEvent = HandlerUtil.checkEvent(xpDroppingEntity.blockPosition(), RegionFlag.XP_DROP_ALL, dimRegion);
                    event.setCanceled(flagCheckEvent.isDenied());
                    if (event.isCanceled()) {
                        return;
                    }
                    if (event.getAttackingPlayer() != null) {
                        // prevent non-member/owner players from dropping xp by killing mobs
                        FlagCheckEvent playerFlagCheckEvent = checkEvent(xpDroppingEntity.blockPosition(), RegionFlag.XP_DROP_PLAYER, dimCache.getDimensionalRegion(), player);
                        if (handleAndSendMsg(event, playerFlagCheckEvent)) {
                            return;
                        }
                    }
                    // prevent monster xp drop
                    if (isMonster(xpDroppingEntity)) {
                        flagCheckEvent = HandlerUtil.checkEvent(xpDroppingEntity.blockPosition(), RegionFlag.XP_DROP_MONSTER, dimRegion);
                        event.setCanceled(flagCheckEvent.isDenied());
                    } else {
                        // prevent other entity xp drop (villagers, animals, ..)
                        flagCheckEvent = HandlerUtil.checkEvent(xpDroppingEntity.blockPosition(), RegionFlag.XP_DROP_OTHER, dimRegion);
                        event.setCanceled(flagCheckEvent.isDenied());
                    }
                }
            }
        }
    }

    @SubscribeEvent
    public static void onMobGriefing(EntityMobGriefingEvent event) {
        if (!event.getEntity().level.isClientSide) {
            if (isServerSide(event.getEntity())) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(event.getEntity().blockPosition(), RegionFlag.MOB_GRIEFING, getEntityDim(event.getEntity()), null);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
                FlagState flagState = processCheck(checkEvent, null, denyResult -> {
                    event.setResult(Event.Result.DENY);
                });
                if (flagState == FlagState.DENIED)
                    return;
                if (event.getEntity() instanceof EnderMan) {
                    checkEvent = new FlagCheckEvent(event.getEntity().blockPosition(), RegionFlag.ENDERMAN_GRIEFING, getEntityDim(event.getEntity()), null);
                    if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                        return;
                    }
                    processCheck(checkEvent, null, denyResult -> {
                        event.setResult(Event.Result.DENY);
                    });
                }
            }
        }
    }

    // idea: differentiate between player and other entities (armor stand/mobs)
    /*
    TODO: Disabled this to enable compatibility with PLACE_BLOCKS again because they use the same event
    @SubscribeEvent
    public static void onFreezeWaterWithBoots(BlockEvent.EntityPlaceEvent event) {
        if (!event.getWorld().isClientSide()) {
            if (event.getEntity() != null) {
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(event.getEntity().level.dimension());
                FlagCheckEvent flagCheckEvent = HandlerUtil.checkTargetEvent(event.getPos(), NO_WALKER_FREEZE, dimCache.getDimensionalRegion());
                if (flagCheckEvent.isDenied()) {
                    event.setCanceled(true);
                }
            }
        }
    }
    */

    /**
     * TODO: Inverted flags would need to re-add allowed blocks/entites
     * Removes affected entities and/or blocks from the event list to protect them
     */
    @SubscribeEvent
    public static void onExplosion(ExplosionEvent.Detonate event) {
        if (!event.getWorld().isClientSide) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(event.getWorld().dimension());
            if (dimCache != null && dimCache.getDimensionalRegion().isActive()) {
                DimensionalRegion dimRegion = dimCache.getDimensionalRegion();

                Set<BlockPos> protectedBlocks = event.getAffectedBlocks().stream()
                        .filter(blockPos -> HandlerUtil.checkEvent(blockPos, RegionFlag.EXPLOSION_BLOCK, dimRegion).isDenied())
                        .collect(Collectors.toSet());
                Set<Entity> protectedEntities = event.getAffectedEntities().stream()
                        .filter(entity -> HandlerUtil.checkEvent(entity.blockPosition(), RegionFlag.EXPLOSION_ENTITY, dimRegion).isDenied())
                        .collect(Collectors.toSet());

                event.getAffectedBlocks().removeAll(protectedBlocks);
                event.getAffectedEntities().removeAll(protectedEntities);

                if (event.getExplosion().getSourceMob() != null) {
                    boolean explosionTriggeredByCreeper = (event.getExplosion().getSourceMob() instanceof Creeper);
                    if (explosionTriggeredByCreeper) {
                        protectedBlocks = event.getAffectedBlocks().stream()
                                .filter(blockPos -> HandlerUtil.checkEvent(blockPos, RegionFlag.EXPLOSION_CREEPER_BLOCK, dimRegion).isDenied())
                                .collect(Collectors.toSet());
                        protectedEntities = event.getAffectedEntities().stream()
                                .filter(entity -> HandlerUtil.checkEvent(entity.blockPosition(), RegionFlag.EXPLOSION_CREEPER_ENTITY, dimRegion).isDenied())
                                .collect(Collectors.toSet());
                    } else {
                        protectedBlocks = event.getAffectedBlocks().stream()
                                .filter(blockPos -> HandlerUtil.checkEvent(blockPos, RegionFlag.EXPLOSION_OTHER_BLOCKS, dimRegion).isDenied())
                                .collect(Collectors.toSet());
                        protectedEntities = event.getAffectedEntities().stream()
                                .filter(entity -> HandlerUtil.checkEvent(entity.blockPosition(), RegionFlag.EXPLOSION_OTHER_ENTITY, dimRegion).isDenied())
                                .collect(Collectors.toSet());
                    }
                    event.getAffectedBlocks().removeAll(protectedBlocks);
                    event.getAffectedEntities().removeAll(protectedEntities);
                }
            }
        }
    }
}
