package de.z0rdak.yawp.mixin.flag.mobgrief;

import de.z0rdak.yawp.handler.flags.HandlerUtil;
import net.minecraft.entity.mob.SilverfishEntity;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;


@Mixin(targets = "net.minecraft.entity.mob.SilverfishEntity$WanderAndInfestGoal")
public abstract class SilverfishEntityWanderAndInfestGoalMixin{

    @Unique
    private SilverfishEntity silverfish;

    // Remember the outer class instance
    @Inject(method = "<init>", at = @At("TAIL"))
    private void onConstructor(SilverfishEntity silverfish, CallbackInfo ci) {
        this.silverfish = silverfish;
    }

    @Inject(method = "canStart()Z", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onCanStart(CallbackInfoReturnable<Boolean> cir) {
        HandlerUtil.checkMobGrief(silverfish, cir);
    }
}
