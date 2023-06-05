package de.z0rdak.yawp.mixin.mobgrief;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import de.z0rdak.yawp.util.MobGriefingHelper;
import net.minecraft.entity.projectile.SmallFireballEntity;
import net.minecraft.util.hit.BlockHitResult;

@Mixin(SmallFireballEntity.class)
public class SmallFireballEntityMixin {

    @Inject(method = "onBlockHit(Lnet/minecraft/util/hit/BlockHitResult;)V", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onBlockHit(BlockHitResult b, CallbackInfo ci) {
        SmallFireballEntity self = (SmallFireballEntity) (Object) this;
        if (!self.world.isClient) {
            if (MobGriefingHelper.preventGrief(self)) {
                ci.cancel();
            }
        }
    }

}
