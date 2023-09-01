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
import net.minecraft.entity.merchant.villager.VillagerEntity;
import net.minecraft.entity.merchant.villager.WanderingTraderEntity;
import net.minecraft.entity.monster.EndermanEntity;
import net.minecraft.entity.monster.ShulkerEntity;
import net.minecraft.entity.monster.SlimeEntity;
import net.minecraft.entity.passive.IronGolemEntity;
import net.minecraft.entity.passive.SnowGolemEntity;
import net.minecraft.util.math.BlockPos;
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

    @SubscribeEvent
    public static void onEnderTeleportTo(EntityTeleportEvent.EnderEntity event) {
        if (isServerSide(event)) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getEntity()));
            if (dimCache != null) {
                DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                // handle enderman teleportation
                if (event.getEntityLiving() instanceof EndermanEntity) {
                    FlagCheckEvent flagCheckEvent = HandlerUtil.checkEvent(new BlockPos(event.getPrev()), RegionFlag.ENDERMAN_TELEPORT_FROM_REGION, dimRegion);
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
                if (event.getEntityLiving() instanceof ShulkerEntity) {
                    FlagCheckEvent flagCheckEvent = HandlerUtil.checkEvent(new BlockPos(event.getPrev()), RegionFlag.SHULKER_TELEPORT_FROM_REGION, dimRegion);
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
            LivingEntity entity = event.getEntityLiving();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getEntity()));
            if (dimCache != null) {
                DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                FlagCheckEvent flagCheckEvent = HandlerUtil.checkEvent(entity.blockPosition(), RegionFlag.FALL_DAMAGE, dimRegion);
                event.setCanceled(flagCheckEvent.isDenied());
                if (event.isCanceled()) {
                    event.setDistance(0.0f);
                    event.setDamageMultiplier(0.0f);
                    return;
                }
                if (isPlayer(entity)) {
                    flagCheckEvent = HandlerUtil.checkEvent(entity.blockPosition(), RegionFlag.FALL_DAMAGE_PLAYERS, dimRegion);
                    event.setCanceled(flagCheckEvent.isDenied());
                    if (event.isCanceled()) {
                        event.setDistance(0.0f);
                        event.setDamageMultiplier(0.0f);
                        return;
                    }
                }
                if (isVillager(entity)) {
                    flagCheckEvent = HandlerUtil.checkEvent(entity.blockPosition(), RegionFlag.FALL_DAMAGE_VILLAGERS, dimRegion);
                    event.setCanceled(flagCheckEvent.isDenied());
                    if (event.isCanceled()) {
                        event.setDistance(0.0f);
                        event.setDamageMultiplier(0.0f);
                        return;
                    }
                }
                if (isAnimal(entity)) {
                    flagCheckEvent = HandlerUtil.checkEvent(entity.blockPosition(), RegionFlag.FALL_DAMAGE_ANIMALS, dimRegion);
                    event.setCanceled(flagCheckEvent.isDenied());
                    if (event.isCanceled()) {
                        event.setDistance(0.0f);
                        event.setDamageMultiplier(0.0f);
                        return;
                    }
                }
                if (isMonster(entity)) {
                    flagCheckEvent = HandlerUtil.checkEvent(entity.blockPosition(), RegionFlag.FALL_DAMAGE_MONSTERS, dimRegion);
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
    public static void onEntityJoinWorld(EntityJoinWorldEvent event) {
        if (isServerSide(event)) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getEntity()));
            if (dimCache != null) {
                DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                Entity entity = event.getEntity();
                if (entity instanceof MobEntity) {
                    FlagCheckEvent flagCheckEvent = HandlerUtil.checkEvent(entity.blockPosition(), RegionFlag.SPAWNING_ALL, dimRegion);
                    event.setCanceled(flagCheckEvent.isDenied());
                    if (event.isCanceled()) {
                        return;
                    }
                }
                if (isAnimal(entity)) {
                    FlagCheckEvent flagCheckEvent = HandlerUtil.checkEvent(entity.blockPosition(), RegionFlag.SPAWNING_ANIMAL, dimRegion);
                    event.setCanceled(flagCheckEvent.isDenied());
                    if (event.isCanceled()) {
                        return;
                    }
                }
                if (isMonster(entity)) {
                    FlagCheckEvent flagCheckEvent = HandlerUtil.checkEvent(entity.blockPosition(), RegionFlag.SPAWNING_MONSTER, dimRegion);
                    event.setCanceled(flagCheckEvent.isDenied());
                    if (event.isCanceled()) {
                        return;
                    }
                }
                if (entity instanceof SnowGolemEntity || entity instanceof IronGolemEntity) {
                    FlagCheckEvent flagCheckEvent = HandlerUtil.checkEvent(entity.blockPosition(), RegionFlag.SPAWNING_GOLEM, dimRegion);
                    event.setCanceled(flagCheckEvent.isDenied());
                    if (event.isCanceled()) {
                        return;
                    }
                }
                if (entity instanceof VillagerEntity) {
                    FlagCheckEvent flagCheckEvent = HandlerUtil.checkEvent(entity.blockPosition(), RegionFlag.SPAWNING_VILLAGER, dimRegion);
                    event.setCanceled(flagCheckEvent.isDenied());
                    if (event.isCanceled()) {
                        return;
                    }
                }
                if (entity instanceof WanderingTraderEntity) {
                    FlagCheckEvent flagCheckEvent = HandlerUtil.checkEvent(entity.blockPosition(), RegionFlag.SPAWNING_TRADER, dimRegion);
                    event.setCanceled(flagCheckEvent.isDenied());
                    if (event.isCanceled()) {
                        return;
                    }
                }
                if (entity instanceof SlimeEntity) {
                    FlagCheckEvent flagCheckEvent = HandlerUtil.checkEvent(entity.blockPosition(), RegionFlag.SPAWNING_SLIME, dimRegion);
                    event.setCanceled(flagCheckEvent.isDenied());
                    if (event.isCanceled()) {
                        return;
                    }
                }
                if (entity instanceof ExperienceOrbEntity) {
                    FlagCheckEvent flagCheckEvent = HandlerUtil.checkEvent(entity.blockPosition(), RegionFlag.SPAWNING_XP, dimRegion);
                    event.setCanceled(flagCheckEvent.isDenied());
                }
            }
        }
    }
}
