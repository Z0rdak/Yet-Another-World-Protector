package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.entity.Entity;
import net.minecraft.entity.item.ItemEntity;
import net.minecraft.entity.item.minecart.AbstractMinecartEntity;
import net.minecraft.entity.merchant.villager.AbstractVillagerEntity;
import net.minecraft.entity.merchant.villager.VillagerEntity;
import net.minecraft.entity.monster.CreeperEntity;
import net.minecraft.entity.passive.MooshroomEntity;
import net.minecraft.entity.passive.PigEntity;
import net.minecraft.entity.passive.horse.SkeletonHorseEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.world.World;
import net.minecraft.world.server.ServerWorld;
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
@Mod.EventBusSubscriber(modid = YetAnotherWorldProtector.MODID, bus = FORGE)
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
                event.getLightning().remove();
                return;
            }

            // TODO: Implement, flags not yet defined
            if (poorEntity instanceof PlayerEntity) {
            }
            if (poorEntity instanceof PigEntity) {
            }
            if (poorEntity instanceof CreeperEntity) {
            }
            if (poorEntity instanceof MooshroomEntity) { // Also check for entity data Type == red
            }
            if (poorEntity instanceof VillagerEntity) {
            }
            if (poorEntity instanceof SkeletonHorseEntity) {
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
        World world = (World) event.getWorld();
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
                if (entity instanceof PlayerEntity) {
                    sendFlagMsg(new PlayerFlagEvent(flagCheckEvent, (PlayerEntity) entity));
                }
                return;
            }
            if (entity instanceof PlayerEntity) {
                FlagCheckEvent playerFlagCheckEvent = checkEvent(entity.blockPosition(), USE_PORTAL_PLAYERS, dimRegion, (PlayerEntity) entity);
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
                if (entity instanceof AbstractVillagerEntity) {
                    flagCheckEvent = HandlerUtil.checkEvent(entity.blockPosition(), USE_PORTAL_VILLAGERS, dimRegion);
                    event.setCanceled(flagCheckEvent.isDenied());
                    return;
                }
                if (entity instanceof AbstractMinecartEntity) {
                    flagCheckEvent = HandlerUtil.checkEvent(entity.blockPosition(), USE_PORTAL_MINECARTS, dimRegion);
                    event.setCanceled(flagCheckEvent.isDenied());
                }
            }
        }
    }


    @SubscribeEvent
    public static void onTravelToDim(EntityTravelToDimensionEvent event) {
        if (isServerSide(event.getEntity())) {
            if (event.getEntity() instanceof PlayerEntity) {
                PlayerEntity player = (PlayerEntity) event.getEntity();
                DimensionalRegion dimRegion = RegionDataManager.get().cacheFor(event.getDimension()).getDimensionalRegion();
                ServerWorld targetServerLevel = player.getServer().getLevel(event.getDimension());
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