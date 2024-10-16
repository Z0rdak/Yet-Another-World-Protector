package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.util.MessageSender;
import net.minecraft.entity.Entity;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.boss.WitherEntity;
import net.minecraft.entity.boss.dragon.EnderDragonEntity;
import net.minecraft.entity.monster.CreeperEntity;
import net.minecraft.entity.monster.EndermanEntity;
import net.minecraft.entity.monster.ZombieEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
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
import java.util.function.Predicate;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.api.events.region.RegionEvents.post;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;
import static net.minecraftforge.fml.common.Mod.EventBusSubscriber.Bus.FORGE;

@Mod.EventBusSubscriber(modid = YetAnotherWorldProtector.MODID, bus = FORGE)
public class GrievingFlagHandler {

    private GrievingFlagHandler() {
    }

    @SubscribeEvent
    public static void onFarmLandTrampled(BlockEvent.FarmlandTrampleEvent event) {
        if (isServerSide(event.getEntity())) {
            Entity trampler = event.getEntity();
            RegistryKey<World> dim = getDimKey(trampler);
            PlayerEntity player = trampler instanceof PlayerEntity ? (PlayerEntity) trampler : null;
            FlagCheckEvent checkEvent = new FlagCheckEvent(event.getPos(), RegionFlag.TRAMPLE_FARMLAND, dim, player);
            if (post(checkEvent)) {
                return;
            }
            FlagState flagState = processCheck(checkEvent, denyResult -> {
                event.setCanceled(true);
                MessageSender.sendFlagMsg(denyResult);
            });
            if (flagState == FlagState.DENIED)
                return;
            // cancel only player trampling
            if (trampler instanceof PlayerEntity) {
                FlagCheckEvent playerTrampleFlagCheck = new FlagCheckEvent(event.getPos(), RegionFlag.TRAMPLE_FARMLAND_PLAYER, dim, player);
                if (post(playerTrampleFlagCheck)) {
                    return;
                }
                processCheck(playerTrampleFlagCheck, denyResult -> {
                    event.setCanceled(true);
                    MessageSender.sendFlagMsg(denyResult);
                });
            } else {
                FlagCheckEvent entityTrampleFlagCheck = new FlagCheckEvent(event.getPos(), RegionFlag.TRAMPLE_FARMLAND_OTHER, dim, null);
                if (post(entityTrampleFlagCheck)) {
                    return;
                }
                processCheck(entityTrampleFlagCheck, denyResult -> {
                    event.setCanceled(true);
                });
            }

        }
    }

    @SubscribeEvent
    public static void onEntityDestroyBlock(LivingDestroyBlockEvent event) {
        if (isServerSide(event)) {
            LivingEntity destroyer = event.getEntityLiving();
            BlockPos target = event.getPos();
            FlagCheckEvent checkEvent = null;
            if (destroyer instanceof EnderDragonEntity) {
                checkEvent = new FlagCheckEvent(target, RegionFlag.DRAGON_BLOCK_PROT, getDimKey(destroyer), null);
                if (post(checkEvent)) {
                    return;
                }
            }
            if (destroyer instanceof WitherEntity) {
                checkEvent = new FlagCheckEvent(target, RegionFlag.WITHER_BLOCK_PROT, getDimKey(destroyer), null);
                if (post(checkEvent)) {
                    return;
                }
            }
            if (destroyer instanceof ZombieEntity) {
                checkEvent = new FlagCheckEvent(target, RegionFlag.ZOMBIE_DOOR_PROT, getDimKey(destroyer), null);
                if (post(checkEvent)) {
                    return;
                }
            }
            if (checkEvent != null) {
                processCheck(checkEvent, denyResult -> {
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
            PlayerEntity player = isPlayer(lootEntity) ? (PlayerEntity) lootEntity : null;
            FlagCheckEvent checkEvent = new FlagCheckEvent(lootEntity.blockPosition(), RegionFlag.DROP_LOOT_ALL, event.getEntity().level.dimension(), player);
            if (post(checkEvent)) {
                return;
            }
            FlagState flagState = processCheck(checkEvent, denyResult -> {
                event.setCanceled(true);
                MessageSender.sendFlagMsg(denyResult);
            });
            if (flagState == FlagState.DENIED)
                return;
            if (player != null) {
                FlagCheckEvent playerCheckEvent = new FlagCheckEvent(lootEntity.blockPosition(), RegionFlag.DROP_LOOT_PLAYER, player.level.dimension(), player);
                if (post(playerCheckEvent)) {
                    return;
                }
                processCheck(playerCheckEvent, denyResult -> {
                    event.setCanceled(true);
                    MessageSender.sendFlagMsg(denyResult);
                });
            }
        }
    }

    @SubscribeEvent
    public static void onEntityXpDrop(LivingExperienceDropEvent event) {
        if (isServerSide(event)) {
            PlayerEntity player = event.getAttackingPlayer();
            Entity xpDroppingEntity = event.getEntityLiving();
            RegistryKey<World> dim = getDimKey(xpDroppingEntity);
            BlockPos pos = xpDroppingEntity.blockPosition();
            if (player != null) {
                // prevent all xp drop
                FlagCheckEvent checkEvent = new FlagCheckEvent(pos, RegionFlag.XP_DROP_ALL, dim, null);
                if (post(checkEvent)) {
                    return;
                }
                FlagState flagState = processCheck(checkEvent, denyResult -> {
                    event.setCanceled(true);
                });
                if (flagState == FlagState.DENIED)
                    return;

                // prevent non-member/owner players from dropping xp by killing mobs
                checkEvent = new FlagCheckEvent(pos, RegionFlag.XP_DROP_PLAYER, dim, player);
                if (post(checkEvent)) {
                    return;
                }
                flagState = processCheck(checkEvent, denyResult -> {
                    event.setCanceled(true);
                    MessageSender.sendFlagMsg(denyResult);
                });
                if (flagState == FlagState.DENIED)
                    return;

                // prevent monster xp drop
                if (isMonster(xpDroppingEntity)) {
                    checkEvent = new FlagCheckEvent(pos, RegionFlag.XP_DROP_MONSTER, dim, null);
                    if (post(checkEvent)) {
                        return;
                    }
                    flagState = processCheck(checkEvent, denyResult -> {
                        event.setCanceled(true);
                        MessageSender.sendFlagMsg(denyResult);
                    });
                    if (flagState == FlagState.DENIED) {
                    }
                } else {
                    checkEvent = new FlagCheckEvent(pos, RegionFlag.XP_DROP_OTHER, dim, null);
                    if (post(checkEvent)) {
                        return;
                    }
                    processCheck(checkEvent, denyResult -> {
                        event.setCanceled(true);
                        MessageSender.sendFlagMsg(denyResult);
                    });
                }
            }
        }
    }

    @SubscribeEvent
    public static void onMobGriefing(EntityMobGriefingEvent event) {
        if (event.getEntity() == null) {
            return;
        }
        if (isServerSide(event.getEntity())) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(event.getEntity().blockPosition(), RegionFlag.MOB_GRIEFING, getDimKey(event.getEntity()), null);
            if (post(checkEvent)) {
                return;
            }
            FlagState flagState = processCheck(checkEvent, denyResult -> {
                event.setResult(Event.Result.DENY);
            });
            if (flagState == FlagState.DENIED)
                return;
            if (event.getEntity() instanceof EndermanEntity) {
                checkEvent = new FlagCheckEvent(event.getEntity().blockPosition(), RegionFlag.ENDERMAN_GRIEFING, getDimKey(event.getEntity()), null);
                if (post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, denyResult -> {
                    event.setResult(Event.Result.DENY);
                });
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
     * TODO: Inverted flags would need to re-add allowed blocks/entities to the event list
     */
    @SubscribeEvent
    public static void onExplosion(ExplosionEvent.Detonate event) {
        if (!event.getWorld().isClientSide) {
            RegistryKey<World> dim = event.getWorld().dimension();

            Set<BlockPos> protectedBlocks = event.getAffectedBlocks().stream()
                    .filter(explosionBlockPosFilterPredicate(dim, RegionFlag.EXPLOSION_BLOCK))
                    .collect(Collectors.toSet());
            Set<Entity> protectedEntities = event.getAffectedEntities().stream()
                    .filter(explosionEntityPosFilterPredicate(dim, RegionFlag.EXPLOSION_BLOCK))
                    .collect(Collectors.toSet());
            preventDestructionFor(event, protectedBlocks, protectedEntities);

            if (event.getExplosion().getSourceMob() != null) {
                boolean explosionTriggeredByCreeper = (event.getExplosion().getSourceMob() instanceof CreeperEntity);
                if (explosionTriggeredByCreeper) {
                    protectedBlocks = event.getAffectedBlocks().stream()
                            .filter(explosionBlockPosFilterPredicate(dim, RegionFlag.EXPLOSION_CREEPER_BLOCK))
                            .collect(Collectors.toSet());
                    protectedEntities = event.getAffectedEntities().stream()
                            .filter(explosionEntityPosFilterPredicate(dim, RegionFlag.EXPLOSION_CREEPER_ENTITY))
                            .collect(Collectors.toSet());
                } else {
                    protectedBlocks = event.getAffectedBlocks().stream()
                            .filter(explosionBlockPosFilterPredicate(dim, RegionFlag.EXPLOSION_OTHER_BLOCKS))
                            .collect(Collectors.toSet());
                    protectedEntities = event.getAffectedEntities().stream()
                            .filter(explosionEntityPosFilterPredicate(dim, RegionFlag.EXPLOSION_OTHER_ENTITY))
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

    private static Predicate<Entity> explosionEntityPosFilterPredicate(RegistryKey<World> dim, RegionFlag flag) {
        return entity -> {
            // TODO: Introduce a subtype for FlagCheckEvent which holds multiple blocks? This way only one event is fired
            // TODO: Make the event cancellable and have a mutable blockpos list
            FlagCheckEvent checkEvent = new FlagCheckEvent(entity.blockPosition(), flag, dim, null);
            if (post(checkEvent)) {
                return true;
            }
            // TODO: Same for check result, here we only need one result for all blocks
            FlagState flagState = processCheck(checkEvent, null, null);
            return flagState == FlagState.DENIED;
        };
    }

    private static Predicate<BlockPos> explosionBlockPosFilterPredicate(RegistryKey<World> dim, RegionFlag flag) {
        return pos -> {
            // TODO: Introduce a subtype for FlagCheckEvent which holds multiple blocks? This way only one event is fired
            // TODO: Make the event cancellable and have a mutable blockpos list
            FlagCheckEvent checkEvent = new FlagCheckEvent(pos, flag, dim, null);
            if (post(checkEvent)) {
                return true;
            }
            // TODO: Same for check result, here we only need one result for all blocks
            FlagState flagState = processCheck(checkEvent, null, null);
            return flagState == FlagState.DENIED;
        };
    }
}
