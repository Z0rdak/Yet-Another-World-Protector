package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.api.FlagEvaluator;
import de.z0rdak.yawp.api.events.flag.FlagCheckRequest;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.core.BlockPos;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.boss.enderdragon.EnderDragon;
import net.minecraft.world.entity.boss.wither.WitherBoss;
import net.minecraft.world.entity.monster.Creeper;
import net.minecraft.world.entity.monster.EnderMan;
import net.minecraft.world.entity.monster.zombie.Zombie;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.neoforged.bus.api.SubscribeEvent;
import net.neoforged.fml.common.EventBusSubscriber;
import net.neoforged.neoforge.event.entity.EntityMobGriefingEvent;
import net.neoforged.neoforge.event.entity.living.LivingDestroyBlockEvent;
import net.neoforged.neoforge.event.entity.living.LivingDropsEvent;
import net.neoforged.neoforge.event.entity.living.LivingExperienceDropEvent;
import net.neoforged.neoforge.event.level.BlockEvent;
import net.neoforged.neoforge.event.level.ExplosionEvent;

import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.api.MessageSender.sendFlagMsg;
import static de.z0rdak.yawp.handler.HandlerUtil.*;

@EventBusSubscriber(modid = Constants.MOD_ID)
public class GrievingFlagHandler {

    private GrievingFlagHandler() {
    }

    @SubscribeEvent
    public static void onFarmLandTrampled(BlockEvent.FarmlandTrampleEvent event) {
        if (isServerSide(event.getEntity())) {
            Entity trampler = event.getEntity();
            ResourceKey<Level> dim = getDimKey(trampler);
            Player player = trampler instanceof Player ? (Player) trampler : null;
            FlagCheckRequest checkEvent = new FlagCheckRequest(event.getPos(), RegionFlag.TRAMPLE_FARMLAND, dim, player);
            if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent)) {
                return;
            }
            FlagState flagState = FlagEvaluator.processCheck(checkEvent, denyResult -> {
                event.setCanceled(true);
                sendFlagMsg(denyResult);
            });
            if (flagState == FlagState.DENIED)
                return;
            // cancel only player trampling
            if (trampler instanceof Player) {
                checkEvent = new FlagCheckRequest(event.getPos(), RegionFlag.TRAMPLE_FARMLAND_PLAYER, dim, player);
                if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent)) {
                    return;
                }
                FlagEvaluator.processCheck(checkEvent, denyResult -> {
                    event.setCanceled(true);
                    sendFlagMsg(denyResult);
                });
            }
        }
    }

    @SubscribeEvent
    public static void onEntityDestroyBlock(LivingDestroyBlockEvent event) {
        if (isServerSide(event.getEntity())) {
            LivingEntity destroyer = event.getEntity();
            BlockPos target = event.getPos();
            FlagCheckRequest checkEvent = null;
            if (destroyer instanceof EnderDragon) {
                checkEvent = new FlagCheckRequest(target, RegionFlag.DRAGON_BLOCK_PROT, getDimKey(destroyer));
                if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent)) {
                    return;
                }
            }
            if (destroyer instanceof WitherBoss) {
                checkEvent = new FlagCheckRequest(target, RegionFlag.WITHER_BLOCK_PROT, getDimKey(destroyer));
                if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent)) {
                    return;
                }
            }
            if (destroyer instanceof Zombie) {
                checkEvent = new FlagCheckRequest(target, RegionFlag.ZOMBIE_DOOR_PROT, getDimKey(destroyer));
                if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent)) {
                    return;
                }
            }
            if (checkEvent != null) {
                FlagEvaluator.processCheck(checkEvent, denyResult -> {
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
        if (NeoForgeHandlerUtil.isServerSide(event)) {
            LivingEntity lootEntity = event.getEntity();
            Player player = lootEntity instanceof Player ? (Player) lootEntity : null;
            FlagCheckRequest checkEvent = new FlagCheckRequest(lootEntity.blockPosition(), RegionFlag.DROP_LOOT_ALL, event.getEntity().level().dimension(), player);
            if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent)) {
                return;
            }
            FlagState flagState = FlagEvaluator.processCheck(checkEvent, denyResult -> {
                event.setCanceled(true);
                sendFlagMsg(denyResult);
            });
            if (flagState == FlagState.DENIED)
                return;
            if (player != null) {
                checkEvent = new FlagCheckRequest(lootEntity.blockPosition(), RegionFlag.DROP_LOOT_PLAYER, player.level().dimension(), player);
                if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent)) {
                    return;
                }
                FlagEvaluator.processCheck(checkEvent, denyResult -> {
                    event.setCanceled(true);
                    sendFlagMsg(denyResult);
                });
            }
        }
    }

    @SubscribeEvent
    public static void onEntityXpDrop(LivingExperienceDropEvent event) {
        if (NeoForgeHandlerUtil.isServerSide(event)) {
            Player player = event.getAttackingPlayer();
            Entity xpDroppingEntity = event.getEntity();
            BlockPos pos = xpDroppingEntity.blockPosition();
            if (player != null) {
                FlagCheckRequest checkEvent = new FlagCheckRequest(pos, RegionFlag.DROP_XP, getDimKey(xpDroppingEntity), player);
                if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent)) {
                    return;
                }
                FlagState flagState = FlagEvaluator.processCheck(checkEvent, denyResult -> {
                    event.setCanceled(true);
                    sendFlagMsg(denyResult);
                });
                if (flagState == FlagState.DENIED)
                    return;
            }

            if (xpDroppingEntity instanceof Player xpDroppingPlayer) {
                FlagCheckRequest checkEvent = new FlagCheckRequest(xpDroppingPlayer.blockPosition(), RegionFlag.KEEP_XP, getDimKey(xpDroppingPlayer), xpDroppingPlayer);
                if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent))
                    return;
                FlagEvaluator.process(checkEvent)
                        .onAllow( res -> event.setCanceled(true));
            }
        }
    }

    @SubscribeEvent
    public static void onMobGriefing(EntityMobGriefingEvent event) {
        if (event.getEntity() == null) {
            return;
        }
        if (isServerSide(event.getEntity())) {
            FlagCheckRequest checkEvent = new FlagCheckRequest(event.getEntity().blockPosition(), RegionFlag.MOB_GRIEFING, getDimKey(event.getEntity()));
            if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent)) {
                return;
            }
            FlagState flagState = FlagEvaluator.processCheck(checkEvent, denyResult -> {
                event.setCanGrief(false);
                //.setResult(Event.Result.DENY);
            });
            if (flagState == FlagState.DENIED)
                return;
            if (event.getEntity() instanceof EnderMan) {
                checkEvent = new FlagCheckRequest(event.getEntity().blockPosition(), RegionFlag.ENDERMAN_GRIEFING, getDimKey(event.getEntity()));
                if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent)) {
                    return;
                }
                FlagEvaluator.processCheck(checkEvent, denyResult -> {
                    event.setCanGrief(false);
                    //event.setResult(Event.Result.DENY);
                });
            }
        }
    }

    @SubscribeEvent
    public static void onExplosion(ExplosionEvent.Detonate event) {
        if (isServerSide(event.getLevel())) {
            ResourceKey<Level> dim = event.getLevel().dimension();

            Set<BlockPos> protectedBlocks = event.getAffectedBlocks().stream()
                    .filter(explosionBlockPosFilterPredicate(dim, RegionFlag.EXPLOSION_BLOCK))
                    .collect(Collectors.toSet());
            Set<Entity> protectedEntities = event.getAffectedEntities().stream()
                    .filter(explosionEntityPosFilterPredicate(dim, RegionFlag.EXPLOSION_ENTITY))
                    .collect(Collectors.toSet());
            preventDestructionFor(event, protectedBlocks, protectedEntities);

            if (event.getExplosion().getIndirectSourceEntity() != null) {
                boolean explosionTriggeredByCreeper = (event.getExplosion().getIndirectSourceEntity() instanceof Creeper);
                if (explosionTriggeredByCreeper) {
                    protectedBlocks = event.getAffectedBlocks().stream()
                            .filter(explosionBlockPosFilterPredicate(dim, RegionFlag.EXPLOSION_CREEPER_BLOCK))
                            .collect(Collectors.toSet());
                    protectedEntities = event.getAffectedEntities().stream()
                            .filter(explosionEntityPosFilterPredicate(dim, RegionFlag.EXPLOSION_CREEPER_ENTITY))
                            .collect(Collectors.toSet());
                }
                preventDestructionFor(event, protectedBlocks, protectedEntities);
            }
        }
    }

    /**
     * Removes affected entities and/or blocks from the event list to protect them
     *
     * @param event             the explosion event
     * @param protectedBlocks   the blocks to protect
     * @param protectedEntities the entities to protect
     */
    private static void preventDestructionFor(ExplosionEvent.Detonate event, Set<BlockPos> protectedBlocks, Set<Entity> protectedEntities) {
        event.getAffectedBlocks().removeAll(protectedBlocks);
        event.getAffectedEntities().removeAll(protectedEntities);
    }

    private static Predicate<Entity> explosionEntityPosFilterPredicate(ResourceKey<Level> dim, RegionFlag flag) {
        return entity -> {
            // TODO: Introduce a subtype for FlagCheckRequest which holds multiple blocks? This way only one event is fired
            // TODO: Make the event cancellable and have a mutable blockpos list
            FlagCheckRequest checkEvent = new FlagCheckRequest(entity.blockPosition(), flag, dim);
            if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent)) {
                return true;
            }
            // TODO: Same for check result, here we only need one result for all blocks
            FlagState flagState = FlagEvaluator.processCheck(checkEvent, null, null);
            return flagState == FlagState.DENIED;
        };
    }

    private static Predicate<BlockPos> explosionBlockPosFilterPredicate(ResourceKey<Level> dim, RegionFlag flag) {
        return pos -> {
            // TODO: Introduce a subtype for FlagCheckRequest which holds multiple blocks? This way only one event is fired
            // TODO: Make the event cancellable and have a mutable blockpos list
            FlagCheckRequest checkEvent = new FlagCheckRequest(pos, flag, dim);
            if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent)) {
                return true;
            }
            // TODO: Same for check result, here we only need one result for all blocks
            FlagState flagState = FlagEvaluator.processCheck(checkEvent, null, null);
            return flagState == FlagState.DENIED;
        };
    }
}
