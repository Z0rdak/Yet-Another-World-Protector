package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.MessageSender;
import net.minecraft.core.BlockPos;
import net.minecraft.resources.ResourceKey;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.animal.MushroomCow;
import net.minecraft.world.entity.animal.Pig;
import net.minecraft.world.entity.animal.horse.SkeletonHorse;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.entity.monster.Creeper;
import net.minecraft.world.entity.npc.AbstractVillager;
import net.minecraft.world.entity.npc.Villager;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.entity.vehicle.AbstractMinecart;
import net.minecraft.world.level.Level;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.entity.EntityStruckByLightningEvent;
import net.minecraftforge.event.entity.EntityTravelToDimensionEvent;
import net.minecraftforge.event.world.BlockEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;

import javax.annotation.Nullable;

import static de.z0rdak.yawp.core.flag.RegionFlag.*;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;
import static net.minecraftforge.fml.common.Mod.EventBusSubscriber.Bus.FORGE;

/**
 * Contains event handler for flags not directly related to player actions.
 * E.g.
 */
@Mod.EventBusSubscriber(modid = YetAnotherWorldProtector.MODID, value = Dist.DEDICATED_SERVER, bus = FORGE)
public class WorldFlagHandler {

    private WorldFlagHandler() {
    }

    /**
     * TODO: How to prevent lightning strikes which are not hitting entities?
     * Prevents all lightning strikes to hurt entities and removes the lightning entity itself
     *
     * @param event information about the lightning striking an entity
     */
    @SubscribeEvent
    public static void onLightningStrikeOccur(EntityStruckByLightningEvent event) {
        if (isServerSide(event)) {
            Entity poorEntity = event.getEntity();
            FlagCheckEvent checkEvent = new FlagCheckEvent(poorEntity.blockPosition(), LIGHTNING_PROT, event.getEntity().level.dimension(), null);
            if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                return;
            }
            HandlerUtil.processCheck(checkEvent, null, denyResult -> {
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
        Level world = (Level) event.getWorld();
        if (isServerSide(event)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(event.getPos(), SPAWN_PORTAL, world.dimension(), null);
            if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                return;
            }
            HandlerUtil.processCheck(checkEvent, null, deny -> event.setCanceled(true));
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
            ResourceKey<Level> dimension = event.getEntity().level.dimension();
            BlockPos target = entity.blockPosition();
            Player player = entity instanceof Player ? (Player) entity : null;
            FlagCheckEvent checkGeneralEvent = new FlagCheckEvent(target, USE_PORTAL, dimension, player);
            if (MinecraftForge.EVENT_BUS.post(checkGeneralEvent)) {
                return;
            }
            HandlerUtil.processCheck(checkGeneralEvent, null, denyResult -> {
                event.setCanceled(true);
            });

            if (entity instanceof Player) {
                FlagCheckEvent checkPlayerEvent = new FlagCheckEvent(target, USE_PORTAL_PLAYERS, dimension, player);
                if (MinecraftForge.EVENT_BUS.post(checkPlayerEvent)) {
                    return;
                }
                HandlerUtil.processCheck(checkPlayerEvent, null, denyResult -> {
                    event.setCanceled(true);
                });
            } else {
                FlagCheckEvent nonPlayerCheckEvent = getNonPlayerCheckEventFor(entity, target, dimension);
                if (nonPlayerCheckEvent != null) {
                    if (MinecraftForge.EVENT_BUS.post(nonPlayerCheckEvent)) {
                        return;
                    }
                    HandlerUtil.processCheck(nonPlayerCheckEvent, null, denyResult -> {
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
            nonPlayerCheckEvent = new FlagCheckEvent(target, USE_PORTAL_ITEMS, dimension, null);
        }
        if (isAnimal(entity)) {
            nonPlayerCheckEvent = new FlagCheckEvent(target, USE_PORTAL_ANIMALS, dimension, null);
        }
        if (isMonster(entity)) {
            nonPlayerCheckEvent = new FlagCheckEvent(target, USE_PORTAL_MONSTERS, dimension, null);
        }
        if (entity instanceof AbstractVillager) {
            nonPlayerCheckEvent = new FlagCheckEvent(target, USE_PORTAL_VILLAGERS, dimension, null);
        }
        if (entity instanceof AbstractMinecart) {
            nonPlayerCheckEvent = new FlagCheckEvent(target, USE_PORTAL_MINECARTS, dimension, null);
        }
        return nonPlayerCheckEvent;
    }

    @SubscribeEvent
    public static void onTravelToDim(EntityTravelToDimensionEvent event) {
        if (isServerSide(event.getEntity())) {
            if (event.getEntity() instanceof Player player) {
                ResourceKey<Level> dim = event.getDimension();
                DimensionalRegion dimRegion = RegionDataManager.get().cacheFor(dim).getDimensionalRegion();
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
                    if (MinecraftForge.EVENT_BUS.post(checkGeneralEvent)) {
                        return;
                    }
                    HandlerUtil.processCheck(checkGeneralEvent, null, denyResult -> {
                        event.setCanceled(true);
                        MessageSender.sendFlagMsg(denyResult);
                    });
                }
            }
        }
    }
}