package de.z0rdak.yawp.mixin.flag.player;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.player.Player;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.handler.HandlerUtil.isServerSide;
import static de.z0rdak.yawp.handler.HandlerUtil.processCheck;

@Mixin({Player.class})
public abstract class PlayerEntityMixin {
    
    @Inject(method = "tryToStartFallFlying()Z", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/entity/player/Player;startFallFlying()V"), allow = 1, cancellable = true)
    void injectElytraCheck(CallbackInfoReturnable<Boolean> cir) {
        Player player = (Player) (Object) this;
        if (isServerSide(player.level())) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(player.blockPosition(), RegionFlag.USE_ELYTRA, player.level().dimension());
            if (Services.EVENT.post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, denyResult -> cir.setReturnValue(false));
        }
    }

    @Inject(method = "dropEquipment", at = @At(value = "HEAD"), allow = 1, cancellable = true)
    void onDropEquipment(ServerLevel level, CallbackInfo ci) {
        Player self = (Player) (Object) this;
        if (isServerSide(self.level())) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(self.blockPosition(), RegionFlag.KEEP_INV, self.level().dimension());
            if (Services.EVENT.post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, denyResult -> ci.cancel());
        }
    }


}
