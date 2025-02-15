package de.z0rdak.yawp.mixin.flag;

import de.z0rdak.yawp.api.FlagEvaluator;
import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.data.region.RegionDataManager;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LightningBolt;
import net.minecraft.world.entity.RelativeMovement;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.entity.vehicle.AbstractMinecart;
import net.minecraft.world.item.trading.Merchant;
import net.minecraft.world.level.portal.DimensionTransition;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import java.util.Set;

import static de.z0rdak.yawp.core.flag.RegionFlag.*;
import static de.z0rdak.yawp.handler.HandlerUtil.*;
import static de.z0rdak.yawp.api.MessageSender.sendFlagMsg;

@Mixin(Entity.class)
public abstract class EntityMixin {

    @Inject(method = "startRiding(Lnet/minecraft/world/entity/Entity;Z)Z", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void spawnChildFromBreeding(Entity vehicle, boolean force, CallbackInfoReturnable<Boolean> cir) {
        Entity rider = (Entity) (Object) this;
        if (isServerSide(rider)) {
            if (rider instanceof Player player) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(vehicle.blockPosition(), ANIMAL_MOUNTING, getDimKey(vehicle), player);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                FlagEvaluator.processCheck(checkEvent, deny -> {
                    sendFlagMsg(deny);
                    cir.setReturnValue(false);
                });
            }
        }
    }

    @Inject(method = "thunderHit", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onHitByLightning(ServerLevel world, LightningBolt lightning, CallbackInfo ci) {
        Entity poorSoul = (Entity) (Object) this;
        if (isServerSide(poorSoul)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(poorSoul.blockPosition(), LIGHTNING_PROT, getDimKey(poorSoul), null);
            if (Services.EVENT.post(checkEvent)) {
                return;
            }
            FlagEvaluator.processCheck(checkEvent, deny -> {
                ci.cancel();
            });
        }
    }

    @Inject(method = "stopRiding", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void spawnChildFromBreeding(CallbackInfo ci) {
        Entity rider = (Entity) (Object) this;
        if (isServerSide(rider)) {
            if (rider instanceof Player player) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(player.blockPosition(), ANIMAL_UNMOUNTING, getDimKey(player), player);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                FlagEvaluator.processCheck(checkEvent, deny -> {
                    sendFlagMsg(deny);
                    ci.cancel();
                });
            }
        }
    }

    /**
     * Covers USE_PORTAL* flags
     * Note: does not seem to trigger for players, which is fine
     */
    @Inject(method = "changeDimension", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onChangeDimension(DimensionTransition transition, CallbackInfoReturnable<Boolean> cir) {
        Entity self = (Entity) (Object) this;
        if (isServerSide(self.level())) {
            RegionDataManager.addDimKeyOnDimensionChange(null, self.level(), transition.newLevel());
            FlagCheckEvent checkEvent = new FlagCheckEvent(self.blockPosition(), USE_PORTAL, getDimKey(self));
            if (Services.EVENT.post(checkEvent)) {
                return;
            }
            FlagEvaluator.processCheck(checkEvent, deny -> {
                cir.setReturnValue(null);
            });
            if (self instanceof Player player) {
                checkEvent = new FlagCheckEvent(self.blockPosition(), USE_PORTAL_PLAYERS, getDimKey(self), player);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                FlagEvaluator.processCheck(checkEvent, deny -> {
                    sendFlagMsg(deny);
                    cir.setReturnValue(null);
                });
            }
            if (self instanceof ItemEntity) {
                checkEvent = new FlagCheckEvent(self.blockPosition(), USE_PORTAL_ITEMS, getDimKey(self));
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                FlagEvaluator.processCheck(checkEvent, deny -> {
                    cir.setReturnValue(null);
                });
            }
            if (isAnimal(self)) {
                checkEvent = new FlagCheckEvent(self.blockPosition(), USE_PORTAL_ANIMALS, getDimKey(self));
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                FlagEvaluator.processCheck(checkEvent, deny -> {
                    cir.setReturnValue(null);
                });
            }
            if (isMonster(self)) {
                checkEvent = new FlagCheckEvent(self.blockPosition(), USE_PORTAL_MONSTERS, getDimKey(self));
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                FlagEvaluator.processCheck(checkEvent, deny -> {
                    cir.setReturnValue(null);
                });
            }
            if (self instanceof Merchant) {
                checkEvent = new FlagCheckEvent(self.blockPosition(), USE_PORTAL_VILLAGERS, getDimKey(self));
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                FlagEvaluator.processCheck(checkEvent, deny -> {
                    cir.setReturnValue(null);
                });
            }
            if (self instanceof AbstractMinecart) {
                checkEvent = new FlagCheckEvent(self.blockPosition(), USE_PORTAL_MINECARTS, getDimKey(self));
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                FlagEvaluator.processCheck(checkEvent, deny -> {
                    cir.setReturnValue(null);
                });
            }
        }
    }
}
