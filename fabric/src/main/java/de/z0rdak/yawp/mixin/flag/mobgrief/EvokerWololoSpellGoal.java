package de.z0rdak.yawp.mixin.flag.mobgrief;

import de.z0rdak.yawp.api.FlagEvaluator;
import net.minecraft.world.entity.monster.Evoker;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(Evoker.EvokerWololoSpellGoal.class)
public abstract class EvokerWololoSpellGoal {
    @Unique
    private Evoker evoker;

    // Remember the outer class instance
    @Inject(method = "<init>", at = @At("TAIL"))
    private void onConstructor(Evoker evoker, CallbackInfo ci) {
        this.evoker = evoker;
    }

    @Inject(method = "canUse", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onCanStart(CallbackInfoReturnable<Boolean> cir) {
        FlagEvaluator.checkMobGrief(evoker, cir);
    }
}
