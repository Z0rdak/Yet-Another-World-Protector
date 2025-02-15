package de.z0rdak.yawp.mixin.flag.mobgrief;

import de.z0rdak.yawp.api.FlagEvaluator;
import net.minecraft.world.entity.animal.SnowGolem;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(SnowGolem.class)
public abstract class SnowGolemEntityMixin {

    @Inject(method = "aiStep", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/level/block/Block;defaultBlockState()Lnet/minecraft/world/level/block/state/BlockState;"), cancellable = true, allow = 1)
    void onCreateSnowLayer(CallbackInfo ci) {
        SnowGolem entity = (SnowGolem) (Object) this;
        FlagEvaluator.checkMobGrief(entity, ci);
    }

}
