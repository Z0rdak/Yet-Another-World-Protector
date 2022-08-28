package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.entity.Entity;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.MobEntity;
import net.minecraft.entity.item.ExperienceOrbEntity;
import net.minecraft.entity.merchant.villager.AbstractVillagerEntity;
import net.minecraft.entity.monster.EndermanEntity;
import net.minecraft.entity.monster.ShulkerEntity;
import net.minecraft.entity.passive.IronGolemEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.event.entity.EntityJoinWorldEvent;
import net.minecraftforge.event.entity.living.EntityTeleportEvent;
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
                    if (enderEntityEvent.getEntityLiving() instanceof EndermanEntity) {
                        if (dimRegion.containsFlag(RegionFlag.ENDERMAN_TELEPORT_FROM_REGION)
                                || dimRegion.containsFlag(RegionFlag.ENDERMAN_TELEPORT_TO_REGION)) {
                            event.setCanceled(true);
                            return;
                        }
                    }
                    // handle shulker teleportation
                    if (enderEntityEvent.getEntityLiving() instanceof ShulkerEntity) {
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
            LivingEntity entity = event.getEntityLiving();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getEntity()));
            if (dimCache != null && dimCache.getDimensionalRegion().isActive()) {
                DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                // prevent fall damage for all entities
                if (dimRegion.containsFlag(RegionFlag.FALL_DAMAGE)) {
                    event.setCanceled(true); // same result as event.setDamageMultiplier(0.0f);
                    return;
                }
                // prevents fall damage only for affiliated players
                if (entity instanceof PlayerEntity && dimRegion.containsFlag(RegionFlag.FALL_DAMAGE_PLAYERS)
                        && dimRegion.permits((PlayerEntity) entity)) {
                    event.setDamageMultiplier(0.0f);
                    return;
                }
                if (entity instanceof AbstractVillagerEntity && dimRegion.containsFlag(RegionFlag.FALL_DAMAGE_VILLAGERS)) {
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
    public static void onEntityJoinWorld(EntityJoinWorldEvent event) {
        if (isServerSide(event)) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getEntity()));
            if (dimCache != null && dimCache.getDimensionalRegion().isActive()) {
                DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                Entity eventEntity = event.getEntity();
                if (dimRegion.containsFlag(RegionFlag.SPAWNING_ALL) && eventEntity instanceof MobEntity) {
                    event.setCanceled(true);
                    return;
                }
                if (dimRegion.containsFlag(RegionFlag.SPAWNING_ANIMAL) && isAnimal(eventEntity)) {
                    event.setCanceled(true);
                    return;
                }
                if (dimRegion.containsFlag(RegionFlag.SPAWNING_GOLEM) && eventEntity instanceof IronGolemEntity) {
                    event.setCanceled(true);
                    return;
                }
                if (dimRegion.containsFlag(RegionFlag.SPAWNING_MONSTERS) && isMonster(eventEntity)) {
                    event.setCanceled(true);
                    return;
                }
                if (dimRegion.containsFlag(RegionFlag.SPAWNING_XP) && eventEntity instanceof ExperienceOrbEntity) {
                    event.setCanceled(true);
                    return;
                }
            }
        }
    }
}
