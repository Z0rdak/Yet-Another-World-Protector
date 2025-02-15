package de.z0rdak.yawp.mixin.flag.player;

import de.z0rdak.yawp.api.FlagEvaluator;
import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.world.entity.TamableAnimal;
import net.minecraft.world.entity.player.Player;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import static de.z0rdak.yawp.core.flag.RegionFlag.ANIMAL_TAMING;
import static de.z0rdak.yawp.handler.HandlerUtil.*;
import static de.z0rdak.yawp.api.MessageSender.sendFlagMsg;

@Mixin(TamableAnimal.class)
public abstract class TameableEntityMixin {
    @Inject(method = "tame", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onAnimalTame(Player player, CallbackInfo ci) {
        TamableAnimal self = (TamableAnimal) (Object) this;
        if (isServerSide(self.level())) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(self.blockPosition(), ANIMAL_TAMING, getDimKey(self), player);
            if (Services.EVENT.post(checkEvent))
                return;
            FlagEvaluator.processCheck(checkEvent, deny -> {
                sendFlagMsg(deny);
                ci.cancel();
            });
        }
    }
}
