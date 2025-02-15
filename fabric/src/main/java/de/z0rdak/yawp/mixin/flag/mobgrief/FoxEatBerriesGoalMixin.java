package de.z0rdak.yawp.mixin.flag.mobgrief;

import de.z0rdak.yawp.api.FlagEvaluator;
import net.minecraft.world.entity.animal.Fox;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(Fox.FoxEatBerriesGoal.class)
public class FoxEatBerriesGoalMixin {
    @Unique
    private Fox fox;

    // Remember the outer class instance
    @Inject(method = "<init>", at = @At("TAIL"))
    private void onConstructor(Fox fox, double speed, int range, int maxYDifference, CallbackInfo ci) {
        this.fox = fox;
    }

    @Inject(method = "onReachedTarget", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onEatBerries(CallbackInfo ci) {
        FlagEvaluator.checkMobGrief(fox, ci);
    }
}
