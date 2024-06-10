package de.z0rdak.yawp.mixin.flag;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import net.minecraft.entity.mob.ShulkerEntity;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.api.events.region.RegionEvents.post;
import static de.z0rdak.yawp.core.flag.RegionFlag.SHULKER_TELEPORT_FROM_REGION;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;

@Mixin(ShulkerEntity.class)
public abstract class ShulkerEntityMixin {
    @Inject(method = "tryTeleport", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onShulkerTeleport(CallbackInfoReturnable<Boolean> cir) {
        ShulkerEntity self = (ShulkerEntity) (Object) this;
        if (isServerSide(self.getWorld())) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(self.getBlockPos(), SHULKER_TELEPORT_FROM_REGION, getDimKey(self), null);
            if (post(checkEvent)) 
                return;
            processCheck(checkEvent, null, deny -> cir.setReturnValue(false));
        }
    }
}
