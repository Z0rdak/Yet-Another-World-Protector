package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import net.minecraft.entity.Entity;
import net.minecraft.entity.item.ItemEntity;
import net.minecraft.entity.item.minecart.AbstractMinecartEntity;
import net.minecraft.entity.merchant.villager.AbstractVillagerEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraft.world.server.ServerWorld;
import net.minecraftforge.event.entity.EntityStruckByLightningEvent;
import net.minecraftforge.event.entity.EntityTravelToDimensionEvent;
import net.minecraftforge.event.world.BlockEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;

import javax.annotation.Nullable;

import static de.z0rdak.yawp.api.events.region.RegionEvents.post;
import static de.z0rdak.yawp.core.flag.RegionFlag.*;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;
import static de.z0rdak.yawp.util.MessageSender.sendFlagMsg;
import static net.minecraftforge.fml.common.Mod.EventBusSubscriber.Bus.FORGE;

/**
 * Contains event handler for flags not directly related to player actions.
 * E.g.
 */
@Mod.EventBusSubscriber(modid = YetAnotherWorldProtector.MODID, bus = FORGE)
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
        if (isServerSide(event)) {
            Entity poorEntity = event.getEntity();
            FlagCheckEvent checkEvent = new FlagCheckEvent(poorEntity.blockPosition(), LIGHTNING_PROT, event.getEntity().level.dimension(), null);
            if (post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, denyResult -> {
                event.setCanceled(true);
                event.getLightning().remove();
            });
        }
    }

    /**
     * Prevents all nether portal spawning.
     * E.g. flint and steel, fire charge, ghast projectiles, dispenser + flint & steel, etc.
     * This has its uses for markable regions but has limited use for dimensional regions.
     *
     * @param event containing information of nether portal to be created
     */
    @SubscribeEvent
    public static void onNetherPortalSpawn(BlockEvent.PortalSpawnEvent event) {
        World world = (World) event.getWorld();
        if (isServerSide(event)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(event.getPos(), SPAWN_PORTAL, world.dimension());
            if (post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, deny -> event.setCanceled(true));
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
            RegistryKey<World> dimension = event.getEntity().level.dimension();
            BlockPos target = entity.blockPosition();
            PlayerEntity player = entity instanceof PlayerEntity ? (PlayerEntity) entity : null;
            FlagCheckEvent checkEvent = new FlagCheckEvent(target, USE_PORTAL, dimension, player);
            if (post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, denyResult -> {
                event.setCanceled(true);
            });

            if (entity instanceof PlayerEntity) {
                FlagCheckEvent checkPlayerEvent = new FlagCheckEvent(target, USE_PORTAL_PLAYERS, dimension, player);
                if (post(checkEvent)) {
                    return;
                }
                processCheck(checkPlayerEvent, denyResult -> {
                    event.setCanceled(true);
                });
            } else {
                checkEvent = getNonPlayerCheckEventFor(entity, target, dimension);
                if (checkEvent != null) {
                    if (post(checkEvent)) {
                        return;
                    }
                    processCheck(checkEvent, denyResult -> {
                        event.setCanceled(true);
                    });
                }
            }
        }
    }

    @Nullable
    @Deprecated
    // this will be replaced with resource key matching in the next updates, so all these flags will disappear, too.
    private static FlagCheckEvent getNonPlayerCheckEventFor(Entity entity, BlockPos target, RegistryKey<World> dimension) {
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
        if (entity instanceof AbstractVillagerEntity) {
            nonPlayerCheckEvent = new FlagCheckEvent(target, USE_PORTAL_VILLAGERS, dimension);
        }
        if (entity instanceof AbstractMinecartEntity) {
            nonPlayerCheckEvent = new FlagCheckEvent(target, USE_PORTAL_MINECARTS, dimension);
        }
        return nonPlayerCheckEvent;
    }

    @SubscribeEvent
    public static void onTravelToDim(EntityTravelToDimensionEvent event) {
        if (isServerSide(event.getEntity())) {
            if (event.getEntity() instanceof PlayerEntity) {
                PlayerEntity player = (PlayerEntity) event.getEntity();
                RegistryKey<World> dim = event.getDimension();
                ServerWorld targetServerLevel = player.getServer().getLevel(dim);
                if (targetServerLevel != null) {
                  /*
                    TODO: Get target position correctly - until then flag only works for dimension
                    WorldBorder worldborder = targetServerLevel.getWorldBorder();
                    double tpPosScale = DimensionType.getTeleportationScale(player.level.dimensionType(), targetServerLevel.dimensionType());
                    BlockPos targetPos = worldborder.clampToBounds(player.getX() * tpPosScale, player.getY(), player.getZ() * tpPosScale);
                     */
                    // FIXME: Workaround is to not let users add this flag to Local Regions for now until the block position is correctly determined
                    FlagCheckEvent checkEvent = new FlagCheckEvent(player.blockPosition(), ENTER_DIM, dim, player);
                    if (post(checkEvent)) {
                        return;
                    }
                    processCheck(checkEvent, denyResult -> {
                        event.setCanceled(true);
                        sendFlagMsg(denyResult);
                    });
                }
            }
        }
    }
}