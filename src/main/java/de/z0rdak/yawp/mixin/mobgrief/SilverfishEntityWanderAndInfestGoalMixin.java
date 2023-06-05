package de.z0rdak.yawp.mixin.mobgrief;

import net.minecraft.entity.mob.SilverfishEntity;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import de.z0rdak.yawp.util.MobGriefingHelper;


@Mixin(targets = "net.minecraft.entity.mob.SilverfishEntity$WanderAndInfestGoal")
public abstract class SilverfishEntityWanderAndInfestGoalMixin{

    private SilverfishEntity s;

    // Remember the outer class instance
    @Inject(method = "<init>", at = @At("TAIL"))
    private void onConstructor(SilverfishEntity silverfish, CallbackInfo ci) {
        this.s = silverfish;
    }


    @Inject(method = "canStart()Z", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onCanStart(CallbackInfoReturnable<Boolean> cir) {
        if (MobGriefingHelper.preventGrief(s)) {
            cir.setReturnValue(false);
        }
    }
}
