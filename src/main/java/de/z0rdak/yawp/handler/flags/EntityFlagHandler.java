package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.ExperienceOrb;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.animal.IronGolem;
import net.minecraft.world.entity.monster.EnderMan;
import net.minecraft.world.entity.monster.Shulker;
import net.minecraft.world.entity.npc.AbstractVillager;
import net.minecraft.world.entity.player.Player;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.event.entity.EntityJoinLevelEvent;
import net.minecraftforge.event.entity.EntityTeleportEvent;
import net.minecraftforge.event.entity.living.LivingFallEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;

import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;
import static net.minecraftforge.fml.common.Mod.EventBusSubscriber.Bus.FORGE;

@Mod.EventBusSubscriber(modid = YetAnotherWorldProtector.MODID, value = Dist.DEDICATED_SERVER, bus = FORGE)
public class EntityFlagHandler {

    private EntityFlagHandler() {
    }

    /**
     * @param event
     */
    @SubscribeEvent
    public static void onEnderTeleportTo(EntityTeleportEvent event) {
        if (isServerSide(event)) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getEntity()));
            if (dimCache != null && dimCache.getDimensionalRegion().isActive()) {
                DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                // handle player teleportation using ender pearls
                if (event instanceof EntityTeleportEvent.EnderEntity) {
                    EntityTeleportEvent.EnderEntity enderEntityEvent = (EntityTeleportEvent.EnderEntity) event;
                    // handle enderman teleportation
                    if (enderEntityEvent.getEntityLiving() instanceof EnderMan) {
                        if (dimRegion.containsFlag(RegionFlag.ENDERMAN_TELEPORT_FROM_REGION)
                                || dimRegion.containsFlag(RegionFlag.ENDERMAN_TELEPORT_TO_REGION)) {
                            event.setCanceled(true);
                            return;
                        }
                    }
                    // handle shulker teleportation
                    if (enderEntityEvent.getEntityLiving() instanceof Shulker) {
                        if (dimRegion.containsFlag(RegionFlag.SHULKER_TELEPORT_TO_REGION)
                                || dimRegion.containsFlag(RegionFlag.SHULKER_TELEPORT_FROM_REGION)) {
                            event.setCanceled(true);
                            return;
                        }
                    }
                }
            }
        }
    }

    /**
     * @param event
     */
    @SubscribeEvent
    public static void onFall(LivingFallEvent event) {
        if (isServerSide(event)) {
            LivingEntity entity = event.getEntity();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getEntity()));
            if (dimCache != null && dimCache.getDimensionalRegion().isActive()) {
                DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                // prevent fall damage for all entities
                if (dimRegion.containsFlag(RegionFlag.FALL_DAMAGE)) {
                    event.setCanceled(true); // same result as event.setDamageMultiplier(0.0f);
                    return;
                }
                // prevents fall damage only for affiliated players
                if (entity instanceof Player && dimRegion.containsFlag(RegionFlag.FALL_DAMAGE_PLAYERS)
                        && dimRegion.permits((Player) entity)) {
                    event.setDamageMultiplier(0.0f);
                    return;
                }
                if (entity instanceof AbstractVillager && dimRegion.containsFlag(RegionFlag.FALL_DAMAGE_VILLAGERS)) {
                    event.setDamageMultiplier(0.0f);
                    return;
                }
                if (isAnimal(entity) && dimRegion.containsFlag(RegionFlag.FALL_DAMAGE_ANIMALS)) {
                    event.setDamageMultiplier(0.0f);
                    return;
                }
                if (isMonster(entity) && dimRegion.containsFlag(RegionFlag.FALL_DAMAGE_MONSTERS)) {
                    event.setDamageMultiplier(0.0f);
                    return;
                }
            }
        }
    }

    /**
     * @param event
     */
    @SubscribeEvent
    public static void onEntityJoinWorld(EntityJoinLevelEvent event) {
        if (isServerSide(event)) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getEntity()));
            if (dimCache != null && dimCache.getDimensionalRegion().isActive()) {
                DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                Entity eventEntity = event.getEntity();
                if (dimRegion.containsFlag(RegionFlag.SPAWNING_ALL) && eventEntity instanceof Mob) {
                    event.setCanceled(true);
                    return;
                }
                if (dimRegion.containsFlag(RegionFlag.SPAWNING_ANIMAL) && isAnimal(eventEntity)) {
                    event.setCanceled(true);
                    return;
                }
                if (dimRegion.containsFlag(RegionFlag.SPAWNING_GOLEM) && eventEntity instanceof IronGolem) {
                    event.setCanceled(true);
                    return;
                }
                if (dimRegion.containsFlag(RegionFlag.SPAWNING_MONSTERS) && isMonster(eventEntity)) {
                    event.setCanceled(true);
                    return;
                }
                if (dimRegion.containsFlag(RegionFlag.SPAWNING_XP) && eventEntity instanceof ExperienceOrb) {
                    event.setCanceled(true);
                    return;
                }
            }
        }
    }
}
