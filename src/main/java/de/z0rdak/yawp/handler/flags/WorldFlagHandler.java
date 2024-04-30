package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
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
import net.minecraftforge.event.entity.EntityStruckByLightningEvent;
import net.minecraftforge.event.entity.EntityTravelToDimensionEvent;
import net.minecraftforge.event.world.BlockEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;

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
     * Prevents all lightning strikes to hurt entities
     * and removes the lightning entity itself (needs testing)
     *
     * @param event information about the lightning striking an entity
     */
    @SubscribeEvent
    public static void onLightningStrikeOccur(EntityStruckByLightningEvent event) {
        if (isServerSide(event)) {
            Entity poorEntity = event.getEntity();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(poorEntity));
            FlagCheckEvent flagCheckEvent = HandlerUtil.checkEvent(poorEntity.blockPosition(), LIGHTNING_PROT, dimCache.getDimensionalRegion());
            event.setCanceled(flagCheckEvent.isDenied());
            if (flagCheckEvent.isDenied()) {
                event.getLightning().remove(Entity.RemovalReason.DISCARDED);
                return;
            }

            // TODO: Implement, flags not yet defined
            if (poorEntity instanceof Player) {
            }
            if (poorEntity instanceof Pig) {
            }
            if (poorEntity instanceof Creeper) {
            }
            if (poorEntity instanceof MushroomCow) { // Also check for entity data Type == red
            }
            if (poorEntity instanceof Villager) {
            }
            if (poorEntity instanceof SkeletonHorse) {
            }
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
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(world.dimension());
            FlagCheckEvent flagCheckEvent = HandlerUtil.checkEvent(event.getPos(), SPAWN_PORTAL, dimCache.getDimensionalRegion());
            event.setCanceled(flagCheckEvent.isDenied());
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
            DimensionalRegion dimRegion = RegionDataManager.get().cacheFor(getEntityDim(entity)).getDimensionalRegion();
            FlagCheckEvent flagCheckEvent = HandlerUtil.checkEvent(entity.blockPosition(), USE_PORTAL, dimRegion);
            event.setCanceled(flagCheckEvent.isDenied());
            if (event.isCanceled()) {
                if (entity instanceof Player) {
                    sendFlagMsg(new PlayerFlagEvent(flagCheckEvent, (Player) entity));
                }
                return;
            }
            if (entity instanceof Player) {
                FlagCheckEvent playerFlagCheckEvent = checkEvent(entity.blockPosition(), USE_PORTAL_PLAYERS, dimRegion, (Player) entity);
                handleAndSendMsg(event, playerFlagCheckEvent);
            } else {

                if (entity instanceof ItemEntity) {
                    flagCheckEvent = HandlerUtil.checkEvent(entity.blockPosition(), USE_PORTAL_ITEMS, dimRegion);
                    event.setCanceled(flagCheckEvent.isDenied());
                    return;
                }
                if (isAnimal(entity)) {
                    flagCheckEvent = HandlerUtil.checkEvent(entity.blockPosition(), USE_PORTAL_ANIMALS, dimRegion);
                    event.setCanceled(flagCheckEvent.isDenied());
                    return;
                }
                if (isMonster(entity)) {
                    flagCheckEvent = HandlerUtil.checkEvent(entity.blockPosition(), USE_PORTAL_MONSTERS, dimRegion);
                    event.setCanceled(flagCheckEvent.isDenied());
                    return;
                }
                if (entity instanceof AbstractVillager) {
                    flagCheckEvent = HandlerUtil.checkEvent(entity.blockPosition(), USE_PORTAL_VILLAGERS, dimRegion);
                    event.setCanceled(flagCheckEvent.isDenied());
                    return;
                }
                if (entity instanceof AbstractMinecart) {
                    flagCheckEvent = HandlerUtil.checkEvent(entity.blockPosition(), USE_PORTAL_MINECARTS, dimRegion);
                    event.setCanceled(flagCheckEvent.isDenied());
                }
            }
        }
    }

    @SubscribeEvent
    public static void onTravelToDim(EntityTravelToDimensionEvent event) {
        if (isServerSide(event.getEntity())) {
            if (event.getEntity() instanceof Player player) {
                DimensionalRegion dimRegion = RegionDataManager.get().cacheFor(event.getDimension()).getDimensionalRegion();
                ServerLevel targetServerLevel = player.getServer().getLevel(event.getDimension());
                if (targetServerLevel != null) {
                    /*
                    FIXME: Get target position correctly - until then flag only works for dimension
                    WorldBorder worldborder = targetServerLevel.getWorldBorder();
                    double tpPosScale = DimensionType.getTeleportationScale(player.level.dimensionType(), targetServerLevel.dimensionType());
                    BlockPos targetPos = worldborder.clampToBounds(player.getX() * tpPosScale, player.getY(), player.getZ() * tpPosScale);
                     */
                    PlayerFlagEvent playerFlagCheckEvent = new PlayerFlagEvent(player, dimRegion, null, ENTER_DIM);
                    playerFlagCheckEvent.setDeniedLocal(false);
                    if (dimRegion.isActive()) {
                        if (dimRegion.containsFlag(ENTER_DIM) && !dimRegion.permits(player)) {
                            IFlag flag = dimRegion.getFlag(ENTER_DIM.name);
                            // TODO: Check state with allowed
                            playerFlagCheckEvent.setDeniedInDim(flag.isActive());
                        } else {
                            playerFlagCheckEvent.setDeniedInDim(false);
                        }
                    } else {
                        playerFlagCheckEvent.setDeniedInDim(false);
                    }
                    playerFlagCheckEvent.setDenied(playerFlagCheckEvent.isDeniedInDim());
                    handleAndSendMsg(event, playerFlagCheckEvent);
                }
            }
        }
    }
}