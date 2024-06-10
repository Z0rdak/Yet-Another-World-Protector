package de.z0rdak.yawp.mixin.flag.mobgrief;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import de.z0rdak.yawp.util.MobGriefingHelper;
import net.minecraft.entity.projectile.FireballEntity;

@Mixin(FireballEntity.class)
public class FireballEntityMixin {

    @Inject(method = "onCollision(Lnet/minecraft/util/hit/HitResult;)V", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/World;createExplosion(Lnet/minecraft/entity/Entity;DDDFZLnet/minecraft/world/World$ExplosionSourceType;)Lnet/minecraft/world/explosion/Explosion;"), cancellable = true, allow = 1)
    public void onBlockHit(CallbackInfo ci) {
        FireballEntity self = (FireballEntity) (Object) this;
        if (MobGriefingHelper.preventGrief(self)) {
            ci.cancel();
        }
    }

}
