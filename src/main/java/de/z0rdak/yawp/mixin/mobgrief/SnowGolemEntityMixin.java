package de.z0rdak.yawp.mixin.mobgrief;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import de.z0rdak.yawp.util.MobGriefingHelper;
import net.minecraft.entity.passive.SnowGolemEntity;

@Mixin(SnowGolemEntity.class)
public abstract class SnowGolemEntityMixin {

    @Inject(method = "tickMovement()V", at = @At(value = "INVOKE", target = "Lnet/minecraft/block/Block;getDefaultState()Lnet/minecraft/block/BlockState;"), cancellable = true, allow = 1)
    void onTickMovement(CallbackInfo ci) {
        SnowGolemEntity entity = (SnowGolemEntity) (Object) this;
        if (MobGriefingHelper.preventGrief(entity)) {
            ci.cancel();
        }
    }

}
