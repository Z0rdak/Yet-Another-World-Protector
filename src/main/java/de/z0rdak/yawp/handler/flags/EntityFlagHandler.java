package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.api.events.region.FlagCheckResult;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.core.flag.RegionFlag;
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
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.event.entity.EntityJoinWorldEvent;
import net.minecraftforge.event.entity.living.EntityTeleportEvent;
import net.minecraftforge.event.entity.living.LivingFallEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;

import java.util.function.Consumer;

import static de.z0rdak.yawp.api.events.region.RegionEvents.post;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;
import static net.minecraftforge.fml.common.Mod.EventBusSubscriber.Bus.FORGE;

@Mod.EventBusSubscriber(modid = YetAnotherWorldProtector.MODID, bus = FORGE)
public class EntityFlagHandler {

    private EntityFlagHandler() {
    }

    @SubscribeEvent
    public static void onEnderTeleportTo(EntityTeleportEvent.EnderEntity event) {
        if (isServerSide(event)) {
            RegistryKey<World> dim = getDimKey(event.getEntity());
            // handle enderman teleportation
            BlockPos target = new BlockPos(event.getPrev());
            if (event.getEntityLiving() instanceof EndermanEntity) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(target, RegionFlag.ENDERMAN_TELEPORT_FROM_REGION, dim);
                if (post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, denyResult -> event.setCanceled(true));
            }
            // handle shulker teleportation
            if (event.getEntityLiving() instanceof ShulkerEntity) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(target, RegionFlag.SHULKER_TELEPORT_FROM_REGION, dim);
                if (post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, denyResult -> event.setCanceled(true));
            }
        }
    }

    @SubscribeEvent
    public static void onFall(LivingFallEvent event) {
        if (isServerSide(event)) {
            LivingEntity entity = event.getEntityLiving();
            RegistryKey<World> dim = getDimKey(event.getEntity());
            BlockPos target = entity.blockPosition();
            PlayerEntity player = entity instanceof PlayerEntity ? (PlayerEntity) entity : null;
            Consumer<FlagCheckResult> onDenyHandler = denyResult -> {
                event.setCanceled(true);
                event.setDistance(0.0f);
                event.setDamageMultiplier(0.0f);
            };

            FlagCheckEvent checkEvent = new FlagCheckEvent(target, RegionFlag.FALL_DAMAGE, dim, player);
            if (post(checkEvent)) {
                return;
            }
            FlagState flagState = processCheck(checkEvent, onDenyHandler);
            if (flagState == FlagState.DENIED)
                return;

            if (isPlayer(entity)) {
                checkEvent = new FlagCheckEvent(target, RegionFlag.FALL_DAMAGE_PLAYERS, dim, player);
                if (post(checkEvent)) {
                    return;
                }
            }
            if (isVillager(entity)) {
                checkEvent = new FlagCheckEvent(target, RegionFlag.FALL_DAMAGE_VILLAGERS, dim);
                if (post(checkEvent)) {
                    return;
                }
            }
            if (isAnimal(entity)) {
                checkEvent = new FlagCheckEvent(target, RegionFlag.FALL_DAMAGE_ANIMALS, dim);
                if (post(checkEvent)) {
                    return;
                }
            }
            if (isMonster(entity)) {
                checkEvent = new FlagCheckEvent(target, RegionFlag.FALL_DAMAGE_MONSTERS, dim);
                if (post(checkEvent)) {
                    return;
                }
            }
            if (checkEvent != null) {
                processCheck(checkEvent, onDenyHandler);
            }
        }
    }

    @SubscribeEvent
    public static void onEntityJoinWorld(EntityJoinWorldEvent event) {
        if (isServerSide(event)) {
            Entity entity = event.getEntity();
            RegistryKey<World> dim = getDimKey(event.getEntity());
            BlockPos target = entity.blockPosition();
            Consumer<FlagCheckResult> onDenyHandler = denyResult -> event.setCanceled(true);

            FlagCheckEvent checkEvent = null;
            if (entity instanceof MobEntity) {
                checkEvent = new FlagCheckEvent(target, RegionFlag.SPAWNING_ALL, dim);
                if (post(checkEvent)) {
                    return;
                }
                FlagState flagState = processCheck(checkEvent, onDenyHandler);
                if (flagState == FlagState.DENIED)
                    return;
            }
            if (isAnimal(entity)) {
                checkEvent = new FlagCheckEvent(target, RegionFlag.SPAWNING_ANIMAL, dim);
                if (post(checkEvent)) {
                    return;
                }
            }
            if (isMonster(entity)) {
                checkEvent = new FlagCheckEvent(target, RegionFlag.SPAWNING_MONSTER, dim);
                if (post(checkEvent)) {
                    return;
                }
            }
            if (entity instanceof SnowGolemEntity || entity instanceof IronGolemEntity) {
                checkEvent = new FlagCheckEvent(target, RegionFlag.SPAWNING_GOLEM, dim);
                if (post(checkEvent)) {
                    return;
                }
            }
            if (entity instanceof VillagerEntity) {
                checkEvent = new FlagCheckEvent(target, RegionFlag.SPAWNING_VILLAGER, dim);
                if (post(checkEvent)) {
                    return;
                }
            }
            if (entity instanceof WanderingTraderEntity) {
                checkEvent = new FlagCheckEvent(target, RegionFlag.SPAWNING_TRADER, dim);
                if (post(checkEvent)) {
                    return;
                }
            }
            if (entity instanceof SlimeEntity) {
                checkEvent = new FlagCheckEvent(target, RegionFlag.SPAWNING_SLIME, dim);
                if (post(checkEvent)) {
                    return;
                }
            }
            if (entity instanceof ExperienceOrbEntity) {
                checkEvent = new FlagCheckEvent(target, RegionFlag.SPAWNING_XP, dim);
                if (post(checkEvent)) {
                    return;
                }
            }
            if (checkEvent != null) {
                processCheck(checkEvent, onDenyHandler);
            }
        }
    }
}
