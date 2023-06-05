package de.z0rdak.yawp.mixin.mobgrief;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import de.z0rdak.yawp.util.MobGriefingHelper;
import net.minecraft.entity.passive.FoxEntity;

@Mixin(net.minecraft.entity.passive.FoxEntity.EatBerriesGoal.class)
public class FoxEntityEatBerriesGoalMixin {
    private FoxEntity fox;

    // Remember the outer class instance
    @Inject(method = "<init>(Lnet/minecraft/entity/passive/FoxEntity;DII)V", at = @At("TAIL"))
    private void onConstructor(FoxEntity fox, double speed, int range, int maxYDifference, CallbackInfo ci) {
        this.fox = fox;
    }

    @Inject(method = "eatBerries()V", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onEatBerries(CallbackInfo ci) {
        if (MobGriefingHelper.preventGrief(fox)) {
            ci.cancel();
        }
    }
}
