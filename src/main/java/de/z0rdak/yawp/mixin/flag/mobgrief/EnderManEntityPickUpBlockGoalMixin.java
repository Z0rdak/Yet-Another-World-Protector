package de.z0rdak.yawp.mixin.flag.mobgrief;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import net.minecraft.entity.mob.EndermanEntity;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;


import static de.z0rdak.yawp.api.events.region.RegionEvents.post;
import static de.z0rdak.yawp.core.flag.RegionFlag.*;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.getDimKey;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.processCheck;

@Mixin(targets = "net.minecraft.entity.mob.EndermanEntity$PickUpBlockGoal")
public abstract class EnderManEntityPickUpBlockGoalMixin {
    @Final
    @Shadow
    private EndermanEntity enderman;

    @Inject(method = "canStart()Z", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onCanStart(CallbackInfoReturnable<Boolean> cir) {
        EndermanEntity self = enderman;
        FlagCheckEvent checkEvent = new FlagCheckEvent(self.getBlockPos(), ENDERMAN_GRIEFING, getDimKey(self), null);
        if (post(checkEvent)) {
            return;
        }
        processCheck(checkEvent, null, deny -> {
            cir.setReturnValue(false);
        });

        checkEvent = new FlagCheckEvent(self.getBlockPos(), MOB_GRIEFING, getDimKey(self), null);
        if (post(checkEvent)) {
            return;
        }
        processCheck(checkEvent, null, deny -> {
            cir.setReturnValue(false);
        });
    }
}
