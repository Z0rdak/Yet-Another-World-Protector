package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.world.entity.player.Player;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.core.flag.RegionFlag.USE_ELYTRA;
import static de.z0rdak.yawp.handler.HandlerUtil.isServerSide;
import static de.z0rdak.yawp.handler.HandlerUtil.processCheck;

@Mixin({Player.class})
public abstract class PlayerEntityMixin {


    @Inject(at = @At(value = "FIELD", target = "Lnet/minecraft/world/entity/EquipmentSlot;CHEST:Lnet/minecraft/world/entity/EquipmentSlot;"), method = "tryToStartFallFlying()Z", allow = 1, cancellable = true)
    void injectElytraCheck(CallbackInfoReturnable<Boolean> cir) {
        Player player = (Player) (Object) this;
        if (isServerSide(player.level())) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(player.blockPosition(), USE_ELYTRA, player.level().dimension());
            if (Services.EVENT.post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, denyResult -> cir.setReturnValue(false));
        }
    }
}
