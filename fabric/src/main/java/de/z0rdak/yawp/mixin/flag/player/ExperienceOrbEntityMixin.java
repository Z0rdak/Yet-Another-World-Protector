package de.z0rdak.yawp.mixin.flag.player;

import de.z0rdak.yawp.api.FlagEvaluator;
import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.world.entity.ExperienceOrb;
import net.minecraft.world.entity.player.Player;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import static de.z0rdak.yawp.core.flag.RegionFlag.XP_PICKUP;
import static de.z0rdak.yawp.handler.HandlerUtil.*;
import static de.z0rdak.yawp.api.MessageSender.sendFlagMsg;

@Mixin(ExperienceOrb.class)
public abstract class ExperienceOrbEntityMixin {

    @Inject(method = "playerTouch", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/entity/player/Player;take(Lnet/minecraft/world/entity/Entity;I)V"),
            cancellable = true, allow = 1)
    public void onPickUpExperience(Player player, CallbackInfo ci) {
        ExperienceOrb xpOrb = (ExperienceOrb) (Object) this;
        if (isServerSide(xpOrb.level())) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(xpOrb.blockPosition(), XP_PICKUP, getDimKey(player), player);
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
