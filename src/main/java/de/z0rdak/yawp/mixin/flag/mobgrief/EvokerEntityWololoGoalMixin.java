package de.z0rdak.yawp.mixin.flag.mobgrief;

import de.z0rdak.yawp.util.MobGriefingHelper;
import net.minecraft.entity.mob.EvokerEntity;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;


@Mixin(net.minecraft.entity.mob.EvokerEntity.WololoGoal.class)
public abstract class EvokerEntityWololoGoalMixin {
    private EvokerEntity evoker;

    // Remember the outer class instance
    @Inject(method = "<init>(Lnet/minecraft/entity/mob/EvokerEntity;)V", at = @At("TAIL"))
    private void onConstructor(EvokerEntity evoker, CallbackInfo ci) {
        this.evoker = evoker;
    }

    @Inject(method = "canStart()Z", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onCanStart(CallbackInfoReturnable<Boolean> cir) {
        if (MobGriefingHelper.preventGrief(evoker)) {
            cir.setReturnValue(false);
        }
    }
}
