package de.z0rdak.yawp.handler;

import de.z0rdak.yawp.api.MessageSender;
import de.z0rdak.yawp.api.events.flag.FlagCheckRequest;
import de.z0rdak.yawp.api.events.flag.FlagEvent;
import de.z0rdak.yawp.api.events.region.RegionEvent;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.platform.Services;
import de.z0rdak.yawp.util.text.TitleBuilder;
import net.minecraft.ChatFormatting;
import net.minecraft.core.registries.Registries;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.ComponentUtils;
import net.minecraft.resources.ResourceKey;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
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

    private static MinecraftServer minecraftServer;

    public static void storeRef(MinecraftServer server) {
        minecraftServer = server;
    }

    public static void onAddFlag(FlagEvent.Add event) {
        if (event.getFlag().getName().contains("spawning") && Services.FLAG_CONFIG.removeEntitiesEnabled()) {
            removeInvolvedEntities(event.getRegion(), RegionFlag.fromId(event.getFlag().getName()));
        }
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
                return HandlerUtil::isMonster;
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

    public static void onPlayerEnterRegion(RegionEvent.PlayerEnter onEnter) {
        var titleText = ComponentUtils.wrapInSquareBrackets(
                Component.literal(onEnter.getRegion().getName()).withStyle(ChatFormatting.AQUA));
        var title = TitleBuilder.of(onEnter.getPlayer(), onEnter.getRegion())
                .title(titleText)
                .subtitleWelcome()
                .timings(10, 40, 15)
                .build();
        title.send();
    }

    public static void onPlayerLeaveRegion(RegionEvent.PlayerLeave onLeave) {
        var titleText = ComponentUtils.wrapInSquareBrackets(
                Component.literal(onLeave.getRegion().getName()).withStyle(ChatFormatting.AQUA));
        var title = TitleBuilder.of(onLeave.getPlayer(), onLeave.getRegion())
                .title(titleText)
                .subtitleBye()
                .timings(10, 40, 15)
                .build();
        title.send();
    }
}
