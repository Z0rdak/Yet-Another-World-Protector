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
import net.minecraft.world.entity.animal.SnowGolem;
import net.minecraft.world.entity.monster.EnderMan;
import net.minecraft.world.entity.monster.Shulker;
import net.minecraft.world.entity.monster.Slime;
import net.minecraft.world.entity.npc.Villager;
import net.minecraft.world.entity.npc.WanderingTrader;
import net.neoforged.api.distmarker.Dist;
import net.neoforged.bus.api.SubscribeEvent;
import net.neoforged.fml.common.EventBusSubscriber;
import net.neoforged.neoforge.event.entity.EntityJoinLevelEvent;
import net.neoforged.neoforge.event.entity.EntityTeleportEvent;
import net.neoforged.neoforge.event.entity.living.*;

import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;

@EventBusSubscriber(modid = YetAnotherWorldProtector.MODID, value = Dist.DEDICATED_SERVER, bus = EventBusSubscriber.Bus.GAME)
public class EntityFlagHandler {

    private EntityFlagHandler() {
    }

    @SubscribeEvent
    public static void onEnderTeleportTo(EntityTeleportEvent.EnderEntity event) {
        if (isServerSide(event)) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getEntity()));
            if (dimCache != null) {
                DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                // handle enderman teleportation
                if (event.getEntityLiving() instanceof EnderMan) {
                    FlagCheckEvent flagCheckEvent = checkTargetEvent(event.getEntity().blockPosition(), RegionFlag.ENDERMAN_TELEPORT_FROM_REGION, dimRegion);
                    event.setCanceled(flagCheckEvent.isDenied());
                    if (event.isCanceled()) {
                        return;
                    }
                    /*
                    flagCheckEvent = checkTargetEvent(new BlockPos(event.getTarget()), RegionFlag.ENDERMAN_TELEPORT_TO_REGION, dimRegion);
                    event.setCanceled(flagCheckEvent.isDenied());
                    if (event.isCanceled()) {
                        return;
                    }
                     */
                }
                // handle shulker teleportation
                if (event.getEntityLiving() instanceof Shulker) {
                    FlagCheckEvent flagCheckEvent = checkTargetEvent(event.getEntity().blockPosition(), RegionFlag.SHULKER_TELEPORT_FROM_REGION, dimRegion);
                    event.setCanceled(flagCheckEvent.isDenied());
                    if (event.isCanceled()) {
                    }
                    /*
                    flagCheckEvent = checkTargetEvent(new BlockPos(event.getTarget()), RegionFlag.SHULKER_TELEPORT_TO_REGION, dimRegion);
                    event.setCanceled(flagCheckEvent.isDenied());
                    if (event.isCanceled()) {
                        return;
                    }
                     */
                }
            }
        }
    }

    @SubscribeEvent
    public static void onFall(LivingFallEvent event) {
        if (isServerSide(event)) {
            LivingEntity entity = event.getEntity();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getEntity()));
            if (dimCache != null) {
                DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                FlagCheckEvent flagCheckEvent = checkTargetEvent(entity.blockPosition(), RegionFlag.FALL_DAMAGE, dimRegion);
                event.setCanceled(flagCheckEvent.isDenied());
                if (event.isCanceled()) {
                    event.setDistance(0.0f);
                    event.setDamageMultiplier(0.0f);
                    return;
                }
                if (isPlayer(entity)) {
                    flagCheckEvent = checkTargetEvent(entity.blockPosition(), RegionFlag.FALL_DAMAGE_PLAYERS, dimRegion);
                    event.setCanceled(flagCheckEvent.isDenied());
                    if (event.isCanceled()) {
                        event.setDistance(0.0f);
                        event.setDamageMultiplier(0.0f);
                        return;
                    }
                }
                if (isVillager(entity)) {
                    flagCheckEvent = checkTargetEvent(entity.blockPosition(), RegionFlag.FALL_DAMAGE_VILLAGERS, dimRegion);
                    event.setCanceled(flagCheckEvent.isDenied());
                    if (event.isCanceled()) {
                        event.setDistance(0.0f);
                        event.setDamageMultiplier(0.0f);
                        return;
                    }
                }
                if (isAnimal(entity)) {
                    flagCheckEvent = checkTargetEvent(entity.blockPosition(), RegionFlag.FALL_DAMAGE_ANIMALS, dimRegion);
                    event.setCanceled(flagCheckEvent.isDenied());
                    if (event.isCanceled()) {
                        event.setDistance(0.0f);
                        event.setDamageMultiplier(0.0f);
                        return;
                    }
                }
                if (isMonster(entity)) {
                    flagCheckEvent = checkTargetEvent(entity.blockPosition(), RegionFlag.FALL_DAMAGE_MONSTERS, dimRegion);
                    event.setCanceled(flagCheckEvent.isDenied());
                    if (event.isCanceled()) {
                        event.setDistance(0.0f);
                        event.setDamageMultiplier(0.0f);
                    }
                }
            }
        }
    }

    @SubscribeEvent
    public static void onEntityJoinWorld(EntityJoinLevelEvent event) {
        if (isServerSide(event)) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getEntity()));
            if (dimCache != null) {
                DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                Entity entity = event.getEntity();
                if (entity instanceof Mob) {
                    FlagCheckEvent flagCheckEvent = checkTargetEvent(entity.blockPosition(), RegionFlag.SPAWNING_ALL, dimRegion);
                    event.setCanceled(flagCheckEvent.isDenied());
                    if (event.isCanceled()) {
                        return;
                    }
                }
                if (isAnimal(entity)) {
                    FlagCheckEvent flagCheckEvent = checkTargetEvent(entity.blockPosition(), RegionFlag.SPAWNING_ANIMAL, dimRegion);
                    event.setCanceled(flagCheckEvent.isDenied());
                    if (event.isCanceled()) {
                        return;
                    }
                }
                if (isMonster(entity)) {
                    FlagCheckEvent flagCheckEvent = checkTargetEvent(entity.blockPosition(), RegionFlag.SPAWNING_MONSTER, dimRegion);
                    event.setCanceled(flagCheckEvent.isDenied());
                    if (event.isCanceled()) {
                        return;
                    }
                }
                if (entity instanceof SnowGolem || entity instanceof IronGolem) {
                    FlagCheckEvent flagCheckEvent = checkTargetEvent(entity.blockPosition(), RegionFlag.SPAWNING_GOLEM, dimRegion);
                    event.setCanceled(flagCheckEvent.isDenied());
                    if (event.isCanceled()) {
                        return;
                    }
                }
                if (entity instanceof Villager) {
                    FlagCheckEvent flagCheckEvent = checkTargetEvent(entity.blockPosition(), RegionFlag.SPAWNING_VILLAGER, dimRegion);
                    event.setCanceled(flagCheckEvent.isDenied());
                    if (event.isCanceled()) {
                        return;
                    }
                }
                if (entity instanceof WanderingTrader) {
                    FlagCheckEvent flagCheckEvent = checkTargetEvent(entity.blockPosition(), RegionFlag.SPAWNING_TRADER, dimRegion);
                    event.setCanceled(flagCheckEvent.isDenied());
                    if (event.isCanceled()) {
                        return;
                    }
                }
                if (entity instanceof Slime) {
                    FlagCheckEvent flagCheckEvent = checkTargetEvent(entity.blockPosition(), RegionFlag.SPAWNING_SLIME, dimRegion);
                    event.setCanceled(flagCheckEvent.isDenied());
                    if (event.isCanceled()) {
                        return;
                    }
                }
                if (entity instanceof ExperienceOrb) {
                    FlagCheckEvent flagCheckEvent = checkTargetEvent(entity.blockPosition(), RegionFlag.SPAWNING_XP, dimRegion);
                    event.setCanceled(flagCheckEvent.isDenied());
                }
            }
        }
    }
}
