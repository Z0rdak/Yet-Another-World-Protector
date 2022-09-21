package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.resources.ResourceKey;
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
import net.minecraftforge.event.level.BlockEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;

import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;
import static net.minecraftforge.fml.common.Mod.EventBusSubscriber.Bus.FORGE;

/**
 * Contains event handler for flags not directly related to player actions.
 * E.g.
 */
@Mod.EventBusSubscriber(modid = YetAnotherWorldProtector.MODID, value = Dist.DEDICATED_SERVER, bus = FORGE)
public class WorldFlagHandler {

    private WorldFlagHandler(){}

    /**
     * Prevents all lightning strikes to hurt entities
     * and removes the lightning entity itself (needs testing)
     * @param event information about the lightning striking an entity
     */
    @SubscribeEvent
    public static void onLightningStrikeOccur(EntityStruckByLightningEvent event){
        if (isServerSide(event)) {
            Entity poorBastard = event.getEntity();
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(poorBastard));
                DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                // TODO: Implement
                if (poorBastard instanceof Player) {

                }
                if (poorBastard instanceof Pig) {

                }
                if (poorBastard instanceof Creeper) {

                }
                if (poorBastard instanceof MushroomCow) {
                    // Check for entity data Type == red
                }
                if (poorBastard instanceof Villager) {

                }
                if (poorBastard instanceof SkeletonHorse) {

                }
                if (dimRegion.containsFlag(RegionFlag.LIGHTNING_PROT)){
                    event.setCanceled(true);
                    event.getLightning().remove(Entity.RemovalReason.DISCARDED);
                }
        }
    }

    /**
     * Prevents all nether portal spawning.
     * E.g. flint and steel, fire charge, ghast projectiles, dispenser + flint & steel, etc.
     * This has its uses for markable regions but has limited use for dimensional regions.
     * @param event containing information of nether portal to be created
     */
    @SubscribeEvent
    public static void onNetherPortalSpawn(BlockEvent.PortalSpawnEvent event) {
        Level world = (Level) event.getLevel();
        if (!world.isClientSide()) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(((Level) event.getLevel()).dimension());
            DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
            if (dimRegion.containsFlag(RegionFlag.SPAWN_PORTAL)){
                event.setCanceled(true);
            }
        }

    }

    /**
     * Handler prevents entities from using portals to travel between dimensions.
     * This has its uses for markable regions but limited use for dimensional regions.
     * Note: This event is only fired for Player (1.16.5), See mixins for other entities.
     * @param event holding info about the entity traveling from one to another dimension.
     */
    // TODO: FROM and TO dim
    @SubscribeEvent
    public static void onUsePortal(EntityTravelToDimensionEvent event) {
        if (isServerSide(event.getEntity())) {
            Entity entity = event.getEntity();
            ResourceKey<Level> dim = getEntityDim(entity);
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(dim);
            DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
            boolean prohibitsPortalUsage = dimRegion.containsFlag(RegionFlag.USE_PORTAL);
            if (prohibitsPortalUsage) {
                event.setCanceled(true);
            }
            if (entity instanceof Player) {
                if (containsFlagAndHasNoAffiliationFor(dimCache, RegionFlag.USE_PORTAL_PLAYERS, (Player) entity)) {
                    event.setCanceled(true);
                    MessageUtil.sendMessage((Player) entity, "flag.msg.event.player.change_dim");
                    return;
                } else {
                    event.setCanceled(false);
                }
            }
            if (dimRegion.containsFlag(RegionFlag.USE_PORTAL_ITEMS) && entity instanceof ItemEntity
                    || dimRegion.containsFlag(RegionFlag.USE_PORTAL_ANIMALS) && isAnimal(entity)
                    || dimRegion.containsFlag(RegionFlag.USE_PORTAL_MONSTERS) && isMonster(entity)
                    || dimRegion.containsFlag(RegionFlag.USE_PORTAL_VILLAGERS) && entity instanceof AbstractVillager
                    || dimRegion.containsFlag(RegionFlag.USE_PORTAL_MINECARTS) && entity instanceof AbstractMinecart) {
                event.setCanceled(true);
                return;
            }
        }
    }
}
