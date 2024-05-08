package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.YetAnotherWorldProtector;
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
import net.neoforged.api.distmarker.Dist;
import net.neoforged.bus.api.Event;
import net.neoforged.bus.api.EventPriority;
import net.neoforged.bus.api.SubscribeEvent;
import net.neoforged.fml.common.Mod;
import net.neoforged.neoforge.common.ToolActions;
import net.neoforged.neoforge.event.CommandEvent;
import net.neoforged.neoforge.event.ServerChatEvent;
import net.neoforged.neoforge.event.TickEvent;
import net.neoforged.neoforge.event.entity.EntityMobGriefingEvent;
import net.neoforged.neoforge.event.entity.EntityMountEvent;
import net.neoforged.neoforge.event.entity.EntityTeleportEvent;
import net.neoforged.neoforge.event.entity.item.ItemTossEvent;
import net.neoforged.neoforge.event.entity.living.*;
import net.neoforged.neoforge.event.entity.player.*;
import net.neoforged.neoforge.event.level.*;
import net.neoforged.neoforge.registries.NeoForgeRegistries;
import static net.neoforged.fml.common.Mod.EventBusSubscriber.Bus.FORGE;

import java.util.Set;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;


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
            LivingEntity destroyer = event.getEntity();
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
            LivingEntity lootEntity = event.getEntity();
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
            Entity xpDroppingEntity = event.getEntity();
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
    public static void onMobGriefing(EntityMobGriefingEvent event) {
        if (event.getEntity() != null && event.getEntity().getCommandSenderWorld() != null) {
            if (isServerSide(event)) {
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getEntity()));
                if (dimCache != null) {
                    FlagCheckEvent mobGriefingFlagCheck = checkTargetEvent(event.getEntity().blockPosition(), RegionFlag.MOB_GRIEFING, dimCache.getDimensionalRegion());
                    if (mobGriefingFlagCheck.isDenied()) {
                        event.setResult(Event.Result.DENY);
                    }

                    if (event.getEntity() instanceof EnderMan) {
                        FlagCheckEvent endermanGriefingFlagCheck = checkTargetEvent(event.getEntity().blockPosition(), RegionFlag.ENDERMAN_GRIEFING, dimCache.getDimensionalRegion());
                        if (endermanGriefingFlagCheck.isDenied()) {
                            event.setResult(Event.Result.DENY);
                        }
                    }
                }
            }
        }
    }

    // idea: differentiate between player and other entities (armor stand/mobs)
    /*
    TODO: Disabled this to enable compatibility with PLACE_BLOCKS again because they use the same event
    @SubscribeEvent(priority = EventPriority.NORMAL, receiveCanceled = true)
    public static void onFreezeWaterWithBoots(BlockEvent.EntityPlaceEvent event) {
        if (!event.getLevel().isClientSide()) {
            if (event.getEntity() != null) {
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(event.getEntity().level().dimension());
                FlagCheckEvent flagCheckEvent = HandlerUtil.checkTargetEvent(event.getPos(), NO_WALKER_FREEZE, dimCache.getDimensionalRegion());
                if (event.isCanceled()) {

                }
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
        if (!event.getLevel().isClientSide) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(event.getLevel().dimension());
            if (dimCache != null && dimCache.getDimensionalRegion().isActive()) {
                DimensionalRegion dimRegion = dimCache.getDimensionalRegion();

                Set<BlockPos> protectedBlocks = event.getAffectedBlocks().stream()
                        .filter(blockPos -> checkTargetEvent(blockPos, RegionFlag.EXPLOSION_BLOCK, dimRegion).isDenied())
                        .collect(Collectors.toSet());
                Set<Entity> protectedEntities = event.getAffectedEntities().stream()
                        .filter(entity -> checkTargetEvent(entity.blockPosition(), RegionFlag.EXPLOSION_ENTITY, dimRegion).isDenied())
                        .collect(Collectors.toSet());

                event.getAffectedBlocks().removeAll(protectedBlocks);
                event.getAffectedEntities().removeAll(protectedEntities);

                if (event.getExplosion().getDirectSourceEntity() != null) {
                    boolean explosionTriggeredByCreeper = (event.getExplosion().getDirectSourceEntity() instanceof Creeper);
                    if (explosionTriggeredByCreeper) {
                        protectedBlocks = event.getAffectedBlocks().stream()
                                .filter(blockPos -> checkTargetEvent(blockPos, RegionFlag.EXPLOSION_CREEPER_BLOCK, dimRegion).isDenied())
                                .collect(Collectors.toSet());
                        protectedEntities = event.getAffectedEntities().stream()
                                .filter(entity -> checkTargetEvent(entity.blockPosition(), RegionFlag.EXPLOSION_CREEPER_ENTITY, dimRegion).isDenied())
                                .collect(Collectors.toSet());
                    } else {
                        protectedBlocks = event.getAffectedBlocks().stream()
                                .filter(blockPos -> checkTargetEvent(blockPos, RegionFlag.EXPLOSION_OTHER_BLOCKS, dimRegion).isDenied())
                                .collect(Collectors.toSet());
                        protectedEntities = event.getAffectedEntities().stream()
                                .filter(entity -> checkTargetEvent(entity.blockPosition(), RegionFlag.EXPLOSION_OTHER_ENTITY, dimRegion).isDenied())
                                .collect(Collectors.toSet());
                    }
                    event.getAffectedBlocks().removeAll(protectedBlocks);
                    event.getAffectedEntities().removeAll(protectedEntities);
                }
            }
        }
    }
}
