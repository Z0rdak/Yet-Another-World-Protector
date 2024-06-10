package de.z0rdak.yawp.mixin.flag.player;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import net.minecraft.entity.passive.TameableEntity;
import net.minecraft.entity.player.PlayerEntity;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import static de.z0rdak.yawp.api.events.region.RegionEvents.post;
import static de.z0rdak.yawp.core.flag.RegionFlag.ANIMAL_TAMING;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;
import static de.z0rdak.yawp.util.MessageSender.*;

@Mixin(TameableEntity.class)
public abstract class TameableEntityMixin {
    @Inject(method = "setOwner", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onAnimalTame(PlayerEntity player, CallbackInfo ci) {
        TameableEntity self = (TameableEntity) (Object) this;
        if (isServerSide(self.getWorld())) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(self.getBlockPos(), ANIMAL_TAMING, getDimKey(self), player);
            if (post(checkEvent)) 
                return;
            processCheck(checkEvent, null, deny -> {
                sendFlagMsg(deny);
                ci.cancel();
            });
        }
    }
}
