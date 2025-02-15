package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.api.FlagEvaluator;
import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.api.events.region.FlagCheckResult;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.handler.HandlerUtil;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.core.BlockPos;
import net.minecraft.resources.ResourceKey;
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
import net.minecraft.world.level.Level;
import net.minecraftforge.event.entity.EntityJoinLevelEvent;
import net.minecraftforge.event.entity.EntityTeleportEvent;
import net.minecraftforge.event.entity.living.LivingFallEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;

import java.util.function.Consumer;

import static de.z0rdak.yawp.handler.HandlerUtil.*;
import static net.minecraftforge.fml.common.Mod.EventBusSubscriber.Bus.FORGE;

@Mod.EventBusSubscriber(modid = Constants.MOD_ID, bus = FORGE)
public class EntityFlagHandler {

    private EntityFlagHandler() {
    }

    @SubscribeEvent
    public static void onEnderTeleportTo(EntityTeleportEvent.EnderEntity event) {
        if (isServerSide(event.getEntity())) {
            ResourceKey<Level> dim = HandlerUtil.getDimKey(event.getEntity());
            // handle enderman teleportation
            BlockPos target = new BlockPos((int) event.getPrev().x, (int) event.getPrev().y, (int) event.getPrev().z);
            if (event.getEntityLiving() instanceof EnderMan) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(target, RegionFlag.ENDERMAN_TELEPORT_FROM_REGION, dim);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                FlagEvaluator.processCheck(checkEvent, denyResult -> event.setCanceled(true));
            }
            // handle shulker teleportation
            if (event.getEntityLiving() instanceof Shulker) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(target, RegionFlag.SHULKER_TELEPORT_FROM_REGION, dim);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                FlagEvaluator.processCheck(checkEvent, denyResult -> event.setCanceled(true));
            }
        }
    }

    @SubscribeEvent
    public static void onFall(LivingFallEvent event) {
        if (isServerSide(event.getEntity())) {
            LivingEntity entity = event.getEntity();
            ResourceKey<Level> dim = HandlerUtil.getDimKey(event.getEntity());
            BlockPos target = entity.blockPosition();
            Consumer<FlagCheckResult> preventFallDmg = denyResult -> {
                event.setCanceled(true);
                event.setDistance(0.0f);
                event.setDamageMultiplier(0.0f);
            };

            FlagCheckEvent checkEvent = new FlagCheckEvent(target, RegionFlag.FALL_DAMAGE, dim);
            if (Services.EVENT.post(checkEvent)) {
                return;
            }
            FlagState flagState = FlagEvaluator.processCheck(checkEvent, preventFallDmg);
            if (flagState == FlagState.DENIED)
                return;

            if (isPlayer(entity)) {
                checkEvent = new FlagCheckEvent(target, RegionFlag.FALL_DAMAGE_PLAYERS, dim);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
            }
            if (isVillager(entity)) {
                checkEvent = new FlagCheckEvent(target, RegionFlag.FALL_DAMAGE_VILLAGERS, dim);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
            }
            if (isAnimal(entity)) {
                checkEvent = new FlagCheckEvent(target, RegionFlag.FALL_DAMAGE_ANIMALS, dim);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
            }
            if (isMonster(entity)) {
                checkEvent = new FlagCheckEvent(target, RegionFlag.FALL_DAMAGE_MONSTERS, dim);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
            }
            if (checkEvent != null) {
                FlagEvaluator.processCheck(checkEvent, preventFallDmg);
            }
        }
    }

    @SubscribeEvent
    public static void onEntityJoinWorld(EntityJoinLevelEvent event) {
        if (isServerSide(event.getLevel())) {
            Entity entity = event.getEntity();
            ResourceKey<Level> dim = HandlerUtil.getDimKey(event.getEntity());
            BlockPos target = entity.blockPosition();
            Consumer<FlagCheckResult> onDenyHandler = denyResult -> event.setCanceled(true);

            FlagCheckEvent checkEvent = null;
            if (entity instanceof Mob) {
                checkEvent = new FlagCheckEvent(target, RegionFlag.SPAWNING_ALL, dim);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                FlagState flagState = FlagEvaluator.processCheck(checkEvent, onDenyHandler);
                if (flagState == FlagState.DENIED)
                    return;
            }
            if (isAnimal(entity)) {
                checkEvent = new FlagCheckEvent(target, RegionFlag.SPAWNING_ANIMAL, dim);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
            }
            if (isMonster(entity)) {
                checkEvent = new FlagCheckEvent(target, RegionFlag.SPAWNING_MONSTER, dim);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
            }
            if (entity instanceof SnowGolem || entity instanceof IronGolem) {
                checkEvent = new FlagCheckEvent(target, RegionFlag.SPAWNING_GOLEM, dim);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
            }
            if (entity instanceof Villager) {
                checkEvent = new FlagCheckEvent(target, RegionFlag.SPAWNING_VILLAGER, dim);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
            }
            if (entity instanceof WanderingTrader) {
                checkEvent = new FlagCheckEvent(target, RegionFlag.SPAWNING_TRADER, dim);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
            }
            if (entity instanceof Slime) {
                checkEvent = new FlagCheckEvent(target, RegionFlag.SPAWNING_SLIME, dim);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
            }
            if (entity instanceof ExperienceOrb) {
                checkEvent = new FlagCheckEvent(target, RegionFlag.SPAWNING_XP, dim);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
            }
            if (checkEvent != null) {
                FlagEvaluator.processCheck(checkEvent, onDenyHandler);
            }
        }
    }
}
