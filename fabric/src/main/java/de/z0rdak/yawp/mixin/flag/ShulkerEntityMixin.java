package de.z0rdak.yawp.mixin.flag;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import net.minecraft.world.entity.monster.Shulker;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.api.events.region.FabricRegionEvents.post;
import static de.z0rdak.yawp.core.flag.RegionFlag.SHULKER_TELEPORT_FROM_REGION;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;

@Mixin(Shulker.class)
public abstract class ShulkerEntityMixin {
    @Inject(method = "teleportSomewhere", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onShulkerTeleport(CallbackInfoReturnable<Boolean> cir) {
        Shulker self = (Shulker) (Object) this;
        if (isServerSide(self.level())) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(self.blockPosition(), SHULKER_TELEPORT_FROM_REGION, getDimKey(self), null);
            if (post(checkEvent))
                return;
            processCheck(checkEvent, null, deny -> cir.setReturnValue(false));
        }
    }
}
