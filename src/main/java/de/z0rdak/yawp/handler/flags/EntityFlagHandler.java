package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.api.events.region.FlagCheckResult;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.core.flag.RegionFlag;
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
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.neoforged.bus.api.SubscribeEvent;
import net.neoforged.fml.common.Mod;
import net.neoforged.neoforge.common.NeoForge;
import net.neoforged.neoforge.event.entity.EntityJoinLevelEvent;
import net.neoforged.neoforge.event.entity.EntityTeleportEvent;
import net.neoforged.neoforge.event.entity.living.LivingFallEvent;

import java.util.function.Consumer;

import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;
import static net.neoforged.fml.common.Mod.EventBusSubscriber.Bus.FORGE;

@Mod.EventBusSubscriber(modid = YetAnotherWorldProtector.MODID, bus = FORGE)
public class EntityFlagHandler {

    private EntityFlagHandler() {
    }

    @SubscribeEvent
    public static void onEnderTeleportTo(EntityTeleportEvent.EnderEntity event) {
        if (isServerSide(event)) {
            ResourceKey<Level> dim = getEntityDim(event.getEntity());
            // handle enderman teleportation
            BlockPos target = new BlockPos((int) event.getPrev().x, (int) event.getPrev().y, (int) event.getPrev().z);
            if (event.getEntityLiving() instanceof EnderMan) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(target, RegionFlag.ENDERMAN_TELEPORT_FROM_REGION, dim, null);
                if (NeoForge.EVENT_BUS.post(checkEvent).isCanceled()) {
                    return;
                }
                processCheck(checkEvent, null, denyResult -> event.setCanceled(true));
            }
            // handle shulker teleportation
            if (event.getEntityLiving() instanceof Shulker) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(target, RegionFlag.SHULKER_TELEPORT_FROM_REGION, dim, null);
                if (NeoForge.EVENT_BUS.post(checkEvent).isCanceled()) {
                    return;
                }
                processCheck(checkEvent, null, denyResult -> event.setCanceled(true));
            }
        }
    }

    @SubscribeEvent
    public static void onFall(LivingFallEvent event) {
        if (isServerSide(event)) {
            LivingEntity entity = event.getEntity();
            ResourceKey<Level> dim = getEntityDim(event.getEntity());
            BlockPos target = entity.blockPosition();
            Player player = entity instanceof Player ? (Player) entity : null;
            Consumer<FlagCheckResult> onDenyHandler = denyResult -> {
                event.setCanceled(true);
                event.setDistance(0.0f);
                event.setDamageMultiplier(0.0f);
            };

            FlagCheckEvent checkEvent = new FlagCheckEvent(target, RegionFlag.FALL_DAMAGE, dim, player);
            if (NeoForge.EVENT_BUS.post(checkEvent).isCanceled()) {
                return;
            }
            FlagState flagState = processCheck(checkEvent, null, onDenyHandler);
            if (flagState == FlagState.DENIED)
                return;

            if (isPlayer(entity)) {
                checkEvent = new FlagCheckEvent(target, RegionFlag.FALL_DAMAGE_PLAYERS, dim, player);
                if (NeoForge.EVENT_BUS.post(checkEvent).isCanceled()) {
                    return;
                }
            }
            if (isVillager(entity)) {
                checkEvent = new FlagCheckEvent(target, RegionFlag.FALL_DAMAGE_VILLAGERS, dim, null);
                if (NeoForge.EVENT_BUS.post(checkEvent).isCanceled()) {
                    return;
                }
            }
            if (isAnimal(entity)) {
                checkEvent = new FlagCheckEvent(target, RegionFlag.FALL_DAMAGE_ANIMALS, dim, null);
                if (NeoForge.EVENT_BUS.post(checkEvent).isCanceled()) {
                    return;
                }
            }
            if (isMonster(entity)) {
                checkEvent = new FlagCheckEvent(target, RegionFlag.FALL_DAMAGE_MONSTERS, dim, null);
                if (NeoForge.EVENT_BUS.post(checkEvent).isCanceled()) {
                    return;
                }
            }
            if (checkEvent != null) {
                processCheck(checkEvent, null, onDenyHandler);
            }
        }
    }

    @SubscribeEvent
    public static void onEntityJoinWorld(EntityJoinLevelEvent event) {
        if (isServerSide(event)) {
            Entity entity = event.getEntity();
            ResourceKey<Level> dim = getEntityDim(event.getEntity());
            BlockPos target = entity.blockPosition();
            Consumer<FlagCheckResult> onDenyHandler = denyResult -> event.setCanceled(true);

            FlagCheckEvent checkEvent = null;
            if (entity instanceof Mob) {
                checkEvent = new FlagCheckEvent(target, RegionFlag.SPAWNING_ALL, dim, null);
                if (NeoForge.EVENT_BUS.post(checkEvent).isCanceled()) {
                    return;
                }
                FlagState flagState = processCheck(checkEvent, null, onDenyHandler);
                if (flagState == FlagState.DENIED)
                    return;
            }
            if (isAnimal(entity)) {
                checkEvent = new FlagCheckEvent(target, RegionFlag.SPAWNING_ANIMAL, dim, null);
                if (NeoForge.EVENT_BUS.post(checkEvent).isCanceled()) {
                    return;
                }
            }
            if (isMonster(entity)) {
                checkEvent = new FlagCheckEvent(target, RegionFlag.SPAWNING_MONSTER, dim, null);
                if (NeoForge.EVENT_BUS.post(checkEvent).isCanceled()) {
                    return;
                }
            }
            if (entity instanceof SnowGolem || entity instanceof IronGolem) {
                checkEvent = new FlagCheckEvent(target, RegionFlag.SPAWNING_GOLEM, dim, null);
                if (NeoForge.EVENT_BUS.post(checkEvent).isCanceled()) {
                    return;
                }
            }
            if (entity instanceof Villager) {
                checkEvent = new FlagCheckEvent(target, RegionFlag.SPAWNING_VILLAGER, dim, null);
                if (NeoForge.EVENT_BUS.post(checkEvent).isCanceled()) {
                    return;
                }
            }
            if (entity instanceof WanderingTrader) {
                checkEvent = new FlagCheckEvent(target, RegionFlag.SPAWNING_TRADER, dim, null);
                if (NeoForge.EVENT_BUS.post(checkEvent).isCanceled()) {
                    return;
                }
            }
            if (entity instanceof Slime) {
                checkEvent = new FlagCheckEvent(target, RegionFlag.SPAWNING_SLIME, dim, null);
                if (NeoForge.EVENT_BUS.post(checkEvent).isCanceled()) {
                    return;
                }
            }
            if (entity instanceof ExperienceOrb) {
                checkEvent = new FlagCheckEvent(target, RegionFlag.SPAWNING_XP, dim, null);
                if (NeoForge.EVENT_BUS.post(checkEvent).isCanceled()) {
                    return;
                }
            }
            if (checkEvent != null) {
                processCheck(checkEvent, null, onDenyHandler);
            }
        }
    }
}
