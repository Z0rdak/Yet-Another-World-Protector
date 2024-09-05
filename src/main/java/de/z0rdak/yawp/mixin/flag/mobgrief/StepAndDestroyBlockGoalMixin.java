package de.z0rdak.yawp.mixin.flag.mobgrief;

import de.z0rdak.yawp.handler.flags.HandlerUtil;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import net.minecraft.entity.ai.goal.StepAndDestroyBlockGoal;
import net.minecraft.entity.mob.MobEntity;

@Mixin(StepAndDestroyBlockGoal.class)
public abstract class StepAndDestroyBlockGoalMixin {
    @Final
    @Shadow private MobEntity stepAndDestroyMob;

    @Inject(method = "canStart", at = @At(value = "HEAD"), cancellable = true)
    public void onCanStart(CallbackInfoReturnable<Boolean> cir) {
        HandlerUtil.checkMobGrief(stepAndDestroyMob, cir);
    }
}
