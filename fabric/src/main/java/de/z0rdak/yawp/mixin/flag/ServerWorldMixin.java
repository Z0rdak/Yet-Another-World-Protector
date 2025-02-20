package de.z0rdak.yawp.mixin.flag;

import de.z0rdak.yawp.api.FlagEvaluator;
import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.ExperienceOrb;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.animal.IronGolem;
import net.minecraft.world.entity.animal.SnowGolem;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.entity.monster.Slime;
import net.minecraft.world.entity.npc.WanderingTrader;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.core.flag.RegionFlag.*;
import static de.z0rdak.yawp.handler.HandlerUtil.*;

@Mixin(ServerLevel.class)
public class ServerWorldMixin {

    @Inject(method = "addEntity", at = @At("HEAD"), cancellable = true, allow = 1)
    public void onSpawnEntity(Entity entity, CallbackInfoReturnable<Boolean> cir) {
        if (isServerSide(entity.level())) {
            FlagCheckEvent checkEvent;
            if (entity instanceof Mob) { // should not cover paintings, armor stands, item entities, players
                checkEvent = new FlagCheckEvent(entity.blockPosition(), SPAWNING_ALL, getDimKey(entity));
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                FlagEvaluator.processCheck(checkEvent, deny -> cir.setReturnValue(false));
            }
            if (isMonster(entity)) {
                checkEvent = new FlagCheckEvent(entity.blockPosition(), SPAWNING_MONSTER, getDimKey(entity));
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                FlagEvaluator.processCheck(checkEvent, deny -> cir.setReturnValue(false));
            }
            if (isAnimal(entity)) {
                checkEvent = new FlagCheckEvent(entity.blockPosition(), SPAWNING_ANIMAL, getDimKey(entity));
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                FlagEvaluator.processCheck(checkEvent, deny -> cir.setReturnValue(false));
            }
            if (isVillager(entity)) {
                checkEvent = new FlagCheckEvent(entity.blockPosition(), SPAWNING_VILLAGER, getDimKey(entity));
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                FlagEvaluator.processCheck(checkEvent, deny -> cir.setReturnValue(false));
            }
            if (entity instanceof WanderingTrader) {
                checkEvent = new FlagCheckEvent(entity.blockPosition(), SPAWNING_TRADER, getDimKey(entity));
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                FlagEvaluator.processCheck(checkEvent, deny -> cir.setReturnValue(false));
            }
            if (entity instanceof SnowGolem || entity instanceof IronGolem) {
                checkEvent = new FlagCheckEvent(entity.blockPosition(), SPAWNING_GOLEM, getDimKey(entity));
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                FlagEvaluator.processCheck(checkEvent, deny -> cir.setReturnValue(false));
            }
            if (entity instanceof Slime) {
                checkEvent = new FlagCheckEvent(entity.blockPosition(), SPAWNING_SLIME, getDimKey(entity));
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                FlagEvaluator.processCheck(checkEvent, deny -> cir.setReturnValue(false));
            }
            if (entity instanceof ExperienceOrb) {
                checkEvent = new FlagCheckEvent(entity.blockPosition(), SPAWNING_XP, getDimKey(entity));
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                FlagEvaluator.processCheck(checkEvent, deny -> cir.setReturnValue(false));
            }
        }
    }
}
