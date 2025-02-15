package de.z0rdak.yawp.mixin.flag;

import de.z0rdak.yawp.api.FlagEvaluator;
import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.world.entity.monster.EnderMan;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.core.flag.RegionFlag.ENDERMAN_TELEPORT_FROM_REGION;
import static de.z0rdak.yawp.handler.HandlerUtil.*;
import static de.z0rdak.yawp.api.MessageSender.sendFlagMsg;

@Mixin(EnderMan.class)
public abstract class EnderManEntityMixin {
    @Inject(method = "teleport(DDD)Z", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onEndermanTeleport(double x, double y, double z, CallbackInfoReturnable<Boolean> cir) {
        EnderMan self = (EnderMan) (Object) this;
        if (isServerSide(self.level())) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(self.blockPosition(), ENDERMAN_TELEPORT_FROM_REGION, getDimKey(self));
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
