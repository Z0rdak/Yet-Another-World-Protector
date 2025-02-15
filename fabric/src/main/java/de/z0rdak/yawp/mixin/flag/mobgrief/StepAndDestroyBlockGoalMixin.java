package de.z0rdak.yawp.mixin.flag.mobgrief;

import de.z0rdak.yawp.api.FlagEvaluator;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.goal.RemoveBlockGoal;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(RemoveBlockGoal.class)
public abstract class StepAndDestroyBlockGoalMixin {
    @Unique
    @Final
    private Mob stepAndDestroyMob;

    @Inject(method = "canUse", at = @At(value = "HEAD"), cancellable = true)
    public void onCanStart(CallbackInfoReturnable<Boolean> cir) {
        if (stepAndDestroyMob != null) {
            FlagEvaluator.checkMobGrief(stepAndDestroyMob, cir);
        }
    }
}
