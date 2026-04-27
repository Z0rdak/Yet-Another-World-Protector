package de.z0rdak.yawp.handler;

import de.z0rdak.yawp.api.events.flag.FlagCheckRequest;
import de.z0rdak.yawp.api.events.flag.FlagEvent;
import de.z0rdak.yawp.api.events.flag.FlagEvents;
import de.z0rdak.yawp.api.events.region.RegionEvent;
import de.z0rdak.yawp.api.events.region.RegionEvents;
import de.z0rdak.yawp.api.events.region.YawpEvents;
import de.z0rdak.yawp.api.visualization.VisualizationManager;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.core.region.RegionType;
import de.z0rdak.yawp.platform.Services;
import de.z0rdak.yawp.util.ChatComponentBuilder;
import de.z0rdak.yawp.util.text.TitleBuilder;
import net.minecraft.ChatFormatting;
import net.minecraft.core.registries.Registries;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.ComponentUtils;
import net.minecraft.resources.ResourceKey;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.ExperienceOrb;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.animal.equine.TraderLlama;
import net.minecraft.world.entity.animal.golem.IronGolem;
import net.minecraft.world.entity.animal.golem.SnowGolem;
import net.minecraft.world.entity.monster.Slime;
import net.minecraft.world.entity.npc.wanderingtrader.WanderingTrader;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.entity.EntityTypeTest;

import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.api.FlagEvaluator.processCheck;

public final class YawpEventHandler {

    private YawpEventHandler() {}
    private static MinecraftServer minecraftServer;

    public static void storeRef(MinecraftServer server) {
        minecraftServer = server;
    }

    public static boolean onAddFlag(FlagEvent.Add event) {
        if (event.getFlag().getName().contains("spawning") && Services.FLAG_CONFIG.removeEntitiesEnabled()) {
            removeInvolvedEntities(event.getRegion(), RegionFlag.fromId(event.getFlag().getName()));
        }
        return true;
    }
    public static boolean onUpdateRegion(RegionEvent.UpdateArea areaUpdate) {
        VisualizationManager.hide(areaUpdate.getRegion());
        return true;
    }

    public static boolean onRemoveRegion(RegionEvent.Remove regionRemove) {
        VisualizationManager.hide(regionRemove.getRegion());
        return true;
    }

    public static void enableRegionSpatialCache(){
        YawpEvents.ON_REGION_DATA_LOADED.register(RegionSpatialCache::initRegions);
    }

    public static void enablePlayerRegionMessages(){
        RegionEvents.ON_PLAYER_ENTER_REGION.register(YawpEventHandler::onPlayerEnterRegion);
        RegionEvents.ON_PLAYER_LEAVE_REGION.register(YawpEventHandler::onPlayerLeaveRegion);
    }

    public static void removeInvolvedEntities(IProtectedRegion region, RegionFlag flag) {
        ResourceKey<Level> dimKey = ResourceKey.create(Registries.DIMENSION, region.getDim().identifier());
        Predicate<? super Entity> entityFilter = getEntityFilterForFlag(flag);
        switch (region.getRegionType()) {
            case GLOBAL: {
                minecraftServer.getAllLevels().forEach(world -> {
                    List<Entity> entitiesToRemove = getEntitiesToRemove(world, entityFilter, flag);
                    entitiesToRemove.forEach(e -> e.setRemoved(Entity.RemovalReason.DISCARDED));
                });
            }
            break;
            case DIMENSION: {
                ServerLevel regionWorld = minecraftServer.getLevel(dimKey);
                if (regionWorld != null) {
                    List<Entity> entitiesToRemove = getEntitiesToRemove(regionWorld, entityFilter, flag);
                    entitiesToRemove.forEach(e -> e.setRemoved(Entity.RemovalReason.DISCARDED));
                }
            }
            break;
            case LOCAL: {
                ServerLevel regionWorld = minecraftServer.getLevel(dimKey);
                if (regionWorld != null) {
                    List<Entity> entitiesToRemove = getEntitiesToRemove(regionWorld, (IMarkableRegion) region, entityFilter);
                    entitiesToRemove.forEach(e -> e.setRemoved(Entity.RemovalReason.DISCARDED));
                }
            }
            break;
        }
    }

    private static Predicate<? super Entity> getEntityFilterForFlag(RegionFlag flag) {
        switch (flag) {
            case SPAWNING_ALL:
                return e -> e instanceof Mob;
            case SPAWNING_MONSTER:
                return e -> HandlerUtil.isMonster(e) || HandlerUtil.hasMonsterJockey(e);
            case SPAWNING_ANIMAL:
                return HandlerUtil::isAnimal;
            case SPAWNING_GOLEM:
                return e -> e instanceof SnowGolem || e instanceof IronGolem;
            case SPAWNING_TRADER:
                return e -> e instanceof WanderingTrader || e instanceof TraderLlama;
            case SPAWNING_SLIME:
                return e -> e instanceof Slime;
            case SPAWNING_VILLAGER:
                return HandlerUtil::isVillager;
            case SPAWNING_XP:
                return e -> e instanceof ExperienceOrb;
            default:
                return e -> false;
        }
    }

    /**
     * Get all entities in the region which are not persistent and match the entityFilter
     */
    private static List<Entity> getEntitiesToRemove(ServerLevel level, IMarkableRegion region, Predicate<? super Entity> entityFilter) {
        // TODO: could be optimized by getting the chunks around the area only to check
        List<? extends Entity> entities = level.getEntities(EntityTypeTest.forClass(Entity.class), entityFilter);
        return entities.stream()
                .filter(e -> region.getArea().containsOther(new CuboidArea(e.blockPosition(), e.blockPosition())))
                .filter(YawpEventHandler::isNotPersistent)
                .collect(Collectors.toList());
    }

    private static List<Entity> getEntitiesToRemove(ServerLevel level, Predicate<? super Entity> entityFilter, RegionFlag flag) {
        List<? extends Entity> entities = level.getEntities(EntityTypeTest.forClass(Entity.class), entityFilter);
        // TODO: EntityTypeTest static where possible, to reduce load
        // for monsters that could be Enemy.class i guess
        return entities.stream()
                .filter(e -> !isProtectedByRegion(level, flag, e)) // That's O(enemyCount * regionCount) complexity, not considering the recursion for the flag check
                .filter(YawpEventHandler::isNotPersistent)
                .collect(Collectors.toList());
    }

    private static boolean isNotPersistent(Entity e) {
        return !hasEnabledPersistenceFlag(e) && !e.hasCustomName();
    }

    private static boolean hasEnabledPersistenceFlag(Entity e) {
        if (e instanceof Mob mob) {
            return mob.isPersistenceRequired();
        }
        return false;
    }

    private static boolean isProtectedByRegion(ServerLevel level, RegionFlag flag, Entity e) {
        FlagCheckRequest checkEvent = new FlagCheckRequest(e.blockPosition(), flag, level.dimension());
        FlagState flagState = processCheck(checkEvent);
        return flagState == FlagState.ALLOWED;
    }

    public static boolean onPlayerEnterRegion(RegionEvent.PlayerEnter onEnter) {
        var titleText = ComponentUtils.wrapInSquareBrackets(
                Component.literal(onEnter.getRegion().getName()).withStyle(ChatFormatting.AQUA));
        var title = TitleBuilder.of(onEnter.getPlayer(), onEnter.getRegion())
                .title(titleText)
                .subtitleWelcome()
                .timings(10, 40, 15)
                .build();
        title.send();
        return true;
    }

    public static boolean onPlayerLeaveRegion(RegionEvent.PlayerLeave onLeave) {
        var titleText = ComponentUtils.wrapInSquareBrackets(
                Component.literal(onLeave.getRegion().getName()).withStyle(ChatFormatting.AQUA));
        var title = TitleBuilder.of(onLeave.getPlayer(), onLeave.getRegion())
                .title(titleText)
                .subtitleBye()
                .timings(10, 40, 15)
                .build();
        title.send();
        return true;
    }

    public static void enableDetailedEventLogger(){
        /* Debug loggers */
        RegionEvents.ON_CREATE.register(YawpEventHandler::logCreateRegion);
        RegionEvents.ON_REMOVE.register(YawpEventHandler::logRemoveRegion);
        RegionEvents.ON_UPDATE_AREA.register(YawpEventHandler::logUpdateRegion);
        RegionEvents.ON_RENAME.register(YawpEventHandler::logRenameRegion);
        RegionEvents.ON_PLAYER_ENTER_REGION.register(YawpEventHandler::logEnterRegion);
        RegionEvents.ON_PLAYER_LEAVE_REGION.register(YawpEventHandler::logLeaveRegion);
        FlagEvents.ON_ADD_FLAG.register(YawpEventHandler::logAddFlag);
        FlagEvents.ON_REMOVE_FLAG.register(YawpEventHandler::logRemoveFlag);
        FlagEvents.ON_UPDATE_FLAG_MESSAGE.register(YawpEventHandler::logUpdateFlagMsg);
    }

    private static boolean logRenameRegion(RegionEvent.Rename rename) {
        if (rename.isCanceled()) {
            Constants.LOGGER.debug("onRenameRegion was canceled.");
            return false;
        }
        ServerPlayer player = rename.getPlayer();
        var region = rename.getRegion();
        var oldName = rename.getOldName();
        var newName = region.getName();
        var regionLoc = region.getRegionType() == RegionType.LOCAL ? " (" + region.getDim().identifier() + ")" : "";
        if (player == null) {
            Constants.LOGGER.debug("Region '{}'{} was renamed to '{}'.", oldName, regionLoc, newName);
        } else {
            Constants.LOGGER.debug("Region '{}'{} was renamed to '{}' by '{}'.", oldName, regionLoc, newName, player.getScoreboardName());
        }
        return true;
    }

    private static FlagEvent.UpdateFlagMessage logUpdateFlagMsg(FlagEvent.UpdateFlagMessage updateFlagMessage) {
        ServerPlayer player = updateFlagMessage.getPlayer();
        var region = updateFlagMessage.getRegion();
        var regionName = region.getName();
        var regionLoc = region.getRegionType() == RegionType.LOCAL ? " (" + region.getDim().identifier() + ")" : "";
        var flagName = updateFlagMessage.getFlag().getName();
        var newMsg = updateFlagMessage.getNewMsg();
        if (player == null) {
            Constants.LOGGER.debug("Flag '{}' in region '{}'{} updated message to '{}'.", flagName, regionName, regionLoc, newMsg);
        } else {
            Constants.LOGGER.debug("Player '{}' updated flag '{}' in region '{}'{} message to '{}'.",
                    player.getScoreboardName(), flagName, regionName, regionLoc, newMsg);
        }
        return updateFlagMessage;
    }

    private static boolean logRemoveFlag(FlagEvent.Remove remove) {
        if (remove.isCanceled()) {
            Constants.LOGGER.debug("onRemoveFlag was canceled.");
            return false;
        }
        ServerPlayer player = remove.getPlayer();
        var region = remove.getRegion();
        var regionName = region.getName();
        var regionLoc = region.getRegionType() == RegionType.LOCAL ? " (" + region.getDim().identifier() + ")" : "";
        var flagName = remove.getFlag().getName();
        if (player == null) {
            Constants.LOGGER.debug("Flag '{}' removed from region '{}'{}.", flagName, regionName, regionLoc);
        } else {
            Constants.LOGGER.debug("Player '{}' removed flag '{}' from region '{}'{}.",
                    player.getScoreboardName(), flagName, regionName, regionLoc);
        }
        return true;
    }

    private static boolean logAddFlag(FlagEvent.Add add) {
        if (add.isCanceled()) {
            Constants.LOGGER.debug("onAddFlag was canceled.");
            return false;
        }
        ServerPlayer player = add.getPlayer();
        var region = add.getRegion();
        var regionName = region.getName();
        var regionLoc = region.getRegionType() == RegionType.LOCAL ? " (" + region.getDim().identifier() + ")" : "";
        var flagName = add.getFlag().getName();
        if (player == null) {
            Constants.LOGGER.debug("Flag '{}' added to region '{}'{}.", flagName, regionName, regionLoc);
        } else {
            Constants.LOGGER.debug("Player '{}' added flag '{}' to region '{}'{}.",
                    player.getScoreboardName(), flagName, regionName, regionLoc);
        }
        return true;
    }

    private static boolean logEnterRegion(RegionEvent.PlayerEnter onEnter) {
        if (onEnter.isCanceled()) {
            Constants.LOGGER.debug("onEnterRegion was canceled.");
            return false;
        }
        var player = onEnter.getPlayer();
        var region = onEnter.getRegion();
        var regionLoc = region.getRegionType() == RegionType.LOCAL ? " (" + region.getDim().identifier() + ")" : "";
        Constants.LOGGER.debug("Player {} entered region '{}'{} at {}",
                player.getScoreboardName(), region.getName(), regionLoc, ChatComponentBuilder.tinyBlockPos(player.blockPosition()));
        return true;
    }

    private static boolean logLeaveRegion(RegionEvent.PlayerLeave onLeave) {
        if (onLeave.isCanceled()) {
            Constants.LOGGER.debug("onLeaveRegion was canceled.");
            return false;
        }
        var player = onLeave.getPlayer();
        var region = onLeave.getRegion();
        var regionLoc = region.getRegionType() == RegionType.LOCAL ? " (" + region.getDim().identifier() + ")" : "";

        Constants.LOGGER.debug("Player {} left region '{}'{} at {}",
                player.getScoreboardName(), region.getName(), regionLoc, ChatComponentBuilder.tinyBlockPos(player.blockPosition()));
        return true;
    }

    private static boolean logCreateRegion(RegionEvent.Create create) {
        if (create.isCanceled()) {
            Constants.LOGGER.debug("Region creation was canceled.");
            return false;
        }
        ServerPlayer player = create.getPlayer();
        var region = create.getRegion();
        var regionName = region.getName();
        var regionLoc = region.getRegionType() == RegionType.LOCAL ? " (" + region.getDim().identifier() + ")" : "";
        if (player == null) {
            Constants.LOGGER.debug("Region '{}'{} was created.", regionName, regionLoc);
        } else {
            Constants.LOGGER.debug("Region '{}'{} was created by '{}'.", regionName, regionLoc, player.getScoreboardName());
        }
        return true;
    }

    private static boolean logRemoveRegion(RegionEvent.Remove remove) {
        if (remove.isCanceled()) {
            Constants.LOGGER.debug("onRemoveRegion was canceled.");
            return false;
        }
        ServerPlayer player = remove.getPlayer();
        var region = remove.getRegion();
        var regionName = region.getName();
        var regionLoc = region.getRegionType() == RegionType.LOCAL ? " (" + region.getDim().identifier() + ")" : "";
        if (player == null) {
            Constants.LOGGER.debug("Region '{}'{} was deleted.", regionName, regionLoc);
        } else {
            Constants.LOGGER.debug("Region '{}'{} was deleted by '{}'.", regionName, regionLoc, player.getScoreboardName());
        }
        return true;
    }

    private static boolean logUpdateRegion(RegionEvent.UpdateArea update) {
        if (update.isCanceled()) {
            Constants.LOGGER.debug("onUpdateRegion was canceled.");
            return false;
        }
        ServerPlayer player = update.getPlayer();
        var region = update.getRegion();
        var regionName = region.getName();
        var regionLoc = region.getRegionType() == RegionType.LOCAL ? " (" + region.getDim().identifier() + ")" : "";

        if (player == null) {
            Constants.LOGGER.debug("Region area of '{}'{} was updated.", regionName, regionLoc);
        } else {
            Constants.LOGGER.debug("Region area of '{}'{} was updated by '{}'.", regionName, regionLoc, player.getScoreboardName());
        }
        return true;
    }
}
