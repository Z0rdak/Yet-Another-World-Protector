package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.api.events.region.RegionEvents;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.handler.flags.HandlerUtil;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.MessageSender;
import net.minecraft.entity.ExperienceOrbEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.util.ActionResult;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import static de.z0rdak.yawp.core.flag.RegionFlag.USE_BONEMEAL;
import static de.z0rdak.yawp.core.flag.RegionFlag.XP_PICKUP;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;

@Mixin(ExperienceOrbEntity.class)
public abstract class ExperienceOrbEntityMixin {

    @Inject(method = "onPlayerCollision", at = @At(value = "INVOKE", target = "Lnet/minecraft/entity/player/PlayerEntity;sendPickup(Lnet/minecraft/entity/Entity;I)V"), cancellable = true, allow = 1)
    public void onPickUpExperience(PlayerEntity player, CallbackInfo ci) {
        ExperienceOrbEntity xpOrb = (ExperienceOrbEntity) (Object) this;
        if (isServerSide(xpOrb.getWorld())) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(xpOrb.getBlockPos(), XP_PICKUP, getEntityDim(player), player);
            if (RegionEvents.CHECK_FLAG.invoker().checkFlag(checkEvent)) {
                return;
            }
            HandlerUtil.processCheck(checkEvent, null, deny -> {
                MessageSender.sendFlagMsg(deny);
                ci.cancel();
            });
        }
    }
}
