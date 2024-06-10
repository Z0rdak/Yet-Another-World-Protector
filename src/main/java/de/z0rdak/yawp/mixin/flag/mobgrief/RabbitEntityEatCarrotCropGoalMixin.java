package de.z0rdak.yawp.mixin.flag.mobgrief;

import net.minecraft.entity.passive.RabbitEntity;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import de.z0rdak.yawp.util.MobGriefingHelper;


@Mixin(targets = "net.minecraft.entity.passive.RabbitEntity$EatCarrotCropGoal")
public class RabbitEntityEatCarrotCropGoalMixin {
    @Shadow 
    private RabbitEntity rabbit;

    @Inject(method = "canStart()Z", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onCanStart(CallbackInfoReturnable<Boolean> cir) {
        if (MobGriefingHelper.preventGrief(rabbit)) {
            cir.setReturnValue(false);
        }
    }
}
