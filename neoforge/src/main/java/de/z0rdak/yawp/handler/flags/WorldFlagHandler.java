package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.api.FlagEvaluator;
import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.core.BlockPos;
import net.minecraft.resources.ResourceKey;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.entity.npc.AbstractVillager;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.entity.vehicle.AbstractMinecart;
import net.minecraft.world.level.Level;
import net.neoforged.bus.api.SubscribeEvent;
import net.neoforged.fml.common.EventBusSubscriber;
import net.neoforged.neoforge.event.entity.EntityStruckByLightningEvent;
import net.neoforged.neoforge.event.entity.EntityTravelToDimensionEvent;
import net.neoforged.neoforge.event.entity.item.ItemExpireEvent;
import net.neoforged.neoforge.event.level.BlockEvent;

import javax.annotation.Nullable;

import static de.z0rdak.yawp.core.flag.RegionFlag.*;
import static de.z0rdak.yawp.handler.HandlerUtil.*;
import static de.z0rdak.yawp.api.MessageSender.sendFlagMsg;

/**
 * Contains event handler for flags not directly related to player actions.
 * E.g.
 */
@EventBusSubscriber(modid = Constants.MOD_ID, bus = EventBusSubscriber.Bus.GAME)
public class WorldFlagHandler {

    private WorldFlagHandler() {
    }

    /**
     * Prevents all lightning strikes to hurt entities and removes the lightning entity itself
     *
     * @param event information about the lightning striking an entity
     */
    @SubscribeEvent
    public static void onLightningStrikeOccur(EntityStruckByLightningEvent event) {
        if (NeoForgeHandlerUtil.isServerSide(event)) {
            Entity poorEntity = event.getEntity();
            FlagCheckEvent checkEvent = new FlagCheckEvent(poorEntity.blockPosition(), LIGHTNING_PROT, event.getEntity().level().dimension());
            if (Services.EVENT.post(checkEvent)) {
                return;
            }
            FlagEvaluator.processCheck(checkEvent, denyResult -> {
                event.setCanceled(true);
                event.getLightning().remove(Entity.RemovalReason.DISCARDED);
            });
        }
    }

    @SubscribeEvent
    public static void onItemExpire(ItemExpireEvent event) {
        FlagCheckEvent checkEvent = new FlagCheckEvent(event.getEntity().blockPosition(), NO_ITEM_DESPAWN, event.getEntity().level().dimension());
        if (Services.EVENT.post(checkEvent)) {
            return;
        }

        FlagEvaluator.processCheck(checkEvent, deny -> {
            event.setExtraLife(6000); // Reset age to avoid flag checking every tick.
        });
    }

    /**
     * Prevents all nether portal spawning.
     * E.g. flint and steel, fire charge, ghast projectiles, dispenser + flint and steel, etc.
     * This has its uses for markable regions but has limited use for dimensional regions.
     *
     * @param event containing information of nether portal to be created
     */
    @SubscribeEvent
    public static void onNetherPortalSpawn(BlockEvent.PortalSpawnEvent event) {
        Level world = (Level) event.getLevel();
        if (isServerSide(world)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(event.getPos(), SPAWN_PORTAL, world.dimension());
            if (Services.EVENT.post(checkEvent)) {
                return;
            }
            FlagEvaluator.processCheck(checkEvent, deny -> event.setCanceled(true));
        }
    }

    /**
     * Handler prevents entities from using portals to travel between dimensions.
     * This has its uses for markable regions but limited use for dimensional regions.
     * Note: This event is only fired for PlayerEntity (1.16.5), See mixins for other entities.
     *
     * @param event holding info about the entity traveling from one to another dimension.
     */
    @SubscribeEvent
    public static void onUsePortal(EntityTravelToDimensionEvent event) {
        if (isServerSide(event.getEntity())) {
            Entity entity = event.getEntity();
            ResourceKey<Level> dimension = event.getEntity().level().dimension();
            BlockPos target = entity.blockPosition();
            Player player = entity instanceof Player ? (Player) entity : null;
            FlagCheckEvent checkEvent = new FlagCheckEvent(target, USE_PORTAL, dimension, player);
            if (Services.EVENT.post(checkEvent)) {
                return;
            }
            FlagEvaluator.processCheck(checkEvent, denyResult -> {
                event.setCanceled(true);
            });

            if (entity instanceof Player) {
                checkEvent = new FlagCheckEvent(target, USE_PORTAL_PLAYERS, dimension, player);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                FlagEvaluator.processCheck(checkEvent, denyResult -> {
                    event.setCanceled(true);
                });
            } else {
                checkEvent = getNonPlayerCheckEventFor(entity, target, dimension);
                if (checkEvent != null) {
                    if (Services.EVENT.post(checkEvent)) {
                        return;
                    }
                    FlagEvaluator.processCheck(checkEvent, denyResult -> {
                        event.setCanceled(true);
                    });
                }
            }
        }
    }

    @Nullable
    @Deprecated
    // this will be replaced with resource key matching in the next updates, so all these flags will disappear, too.
    private static FlagCheckEvent getNonPlayerCheckEventFor(Entity entity, BlockPos target, ResourceKey<Level> dimension) {
        FlagCheckEvent nonPlayerCheckEvent = null;
        if (entity instanceof ItemEntity) {
            nonPlayerCheckEvent = new FlagCheckEvent(target, USE_PORTAL_ITEMS, dimension);
        }
        if (isAnimal(entity)) {
            nonPlayerCheckEvent = new FlagCheckEvent(target, USE_PORTAL_ANIMALS, dimension);
        }
        if (isMonster(entity)) {
            nonPlayerCheckEvent = new FlagCheckEvent(target, USE_PORTAL_MONSTERS, dimension);
        }
        if (entity instanceof AbstractVillager) {
            nonPlayerCheckEvent = new FlagCheckEvent(target, USE_PORTAL_VILLAGERS, dimension);
        }
        if (entity instanceof AbstractMinecart) {
            nonPlayerCheckEvent = new FlagCheckEvent(target, USE_PORTAL_MINECARTS, dimension);
        }
        return nonPlayerCheckEvent;
    }

    @SubscribeEvent
    public static void onTravelToDim(EntityTravelToDimensionEvent event) {
        if (isServerSide(event.getEntity())) {
            if (event.getEntity() instanceof Player player) {
                ResourceKey<Level> dim = event.getDimension();
                ServerLevel targetServerLevel = player.getServer().getLevel(dim);
                if (targetServerLevel != null) {
                  /*
                    TODO: Get target position correctly - until then flag only works for dimension
                    WorldBorder worldborder = targetServerLevel.getWorldBorder();
                    double tpPosScale = DimensionType.getTeleportationScale(player.level.dimensionType(), targetServerLevel.dimensionType());
                    BlockPos targetPos = worldborder.clampToBounds(player.getX() * tpPosScale, player.getY(), player.getZ() * tpPosScale);
                     */
                    // FIXME: Workaround is to not let users add this flag to Local Regions for now until the block position is correctly determined
                    FlagCheckEvent checkGeneralEvent = new FlagCheckEvent(player.blockPosition(), ENTER_DIM, dim, player);
                    if (Services.EVENT.post(checkGeneralEvent)) {
                        return;
                    }
                    FlagEvaluator.processCheck(checkGeneralEvent, denyResult -> {
                        event.setCanceled(true);
                        sendFlagMsg(denyResult);
                    });
                }
            }
        }
    }
}