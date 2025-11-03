package de.z0rdak.yawp.mixin.flag;

import de.z0rdak.yawp.api.FlagEvaluator;
import de.z0rdak.yawp.api.events.flag.FlagCheckRequest;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.world.entity.monster.Shulker;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.core.flag.RegionFlag.SHULKER_TELEPORT_FROM_REGION;
import static de.z0rdak.yawp.handler.HandlerUtil.*;

@Mixin(Shulker.class)
public abstract class ShulkerEntityMixin {
    @Inject(method = "teleportSomewhere", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onShulkerTeleport(CallbackInfoReturnable<Boolean> cir) {
        Shulker self = (Shulker) (Object) this;
        if (isServerSide(self.level())) {
            FlagCheckRequest checkEvent = new FlagCheckRequest(self.blockPosition(), SHULKER_TELEPORT_FROM_REGION, getDimKey(self));
            if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent))
                return;
            FlagEvaluator.processCheck(checkEvent, deny -> cir.setReturnValue(false));
        }
    }
}
