package de.z0rdak.yawp.mixin.flag.player;

import de.z0rdak.yawp.api.FlagEvaluator;
import de.z0rdak.yawp.api.events.flag.FlagCheckRequest;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.player.Player;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.handler.HandlerUtil.getDimKey;
import static de.z0rdak.yawp.handler.HandlerUtil.isServerSide;

@Mixin({Player.class})
public abstract class PlayerEntityMixin {
    
    @Inject(method = "tryToStartFallFlying()Z", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/entity/player/Player;startFallFlying()V"), allow = 1, cancellable = true)
    void injectElytraCheck(CallbackInfoReturnable<Boolean> cir) {
        Player self = (Player) (Object) this;
        if (isServerSide(self.level())) {
            FlagCheckRequest checkEvent = new FlagCheckRequest(self.blockPosition(), RegionFlag.USE_ELYTRA, getDimKey(self), self);
            if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent)) {
                return;
            }
            FlagEvaluator.processCheck(checkEvent, denyResult -> cir.setReturnValue(false));
        }
    }

    @Inject(method = "dropEquipment", at = @At(value = "HEAD"), allow = 1, cancellable = true)
    void onDropEquipment(ServerLevel level, CallbackInfo ci) {
        Player self = (Player) (Object) this;
        if (isServerSide(self.level())) {
            FlagCheckRequest checkEvent = new FlagCheckRequest(self.blockPosition(), RegionFlag.KEEP_INV, getDimKey(self));
            if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent)) {
                return;
            }
            FlagEvaluator.process(checkEvent)
                    .onAllow( res -> ci.cancel());
        }
    }

    @Inject(method = "causeFoodExhaustion", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/food/FoodData;addExhaustion(F)V"), cancellable = true, allow = 1)
    public void onGainHunger(float exhaustion, CallbackInfo ci) {
        Player self = (Player) (Object) this;
        if (isServerSide(self)) {
            FlagCheckRequest checkEvent = new FlagCheckRequest(self.blockPosition(), RegionFlag.NO_HUNGER, getDimKey(self), self);
            if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent))
                return;
            FlagEvaluator.process(checkEvent)
                    .onAllow( res -> ci.cancel());
        }
    }
}