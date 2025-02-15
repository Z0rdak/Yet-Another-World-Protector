package de.z0rdak.yawp.mixin.flag.mobgrief;

import de.z0rdak.yawp.api.FlagEvaluator;
import net.minecraft.world.entity.monster.Silverfish;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(targets = "net.minecraft.world.entity.monster.Silverfish$SilverfishMergeWithStoneGoal")
public abstract class SilverfishMergeWithStoneGoalMixin {

    @Unique
    private Silverfish silverfish;

    // Remember the outer class instance
    @Inject(method = "<init>", at = @At("TAIL"))
    private void onConstructor(Silverfish silverfish, CallbackInfo ci) {
        this.silverfish = silverfish;
    }

    @Inject(method = "canUse()Z", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onCanStart(CallbackInfoReturnable<Boolean> cir) {
        FlagEvaluator.checkMobGrief(silverfish, cir);
    }


}
