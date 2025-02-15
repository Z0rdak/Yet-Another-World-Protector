package de.z0rdak.yawp.mixin.flag.mobgrief;

import de.z0rdak.yawp.api.FlagEvaluator;
import net.minecraft.world.entity.projectile.SmallFireball;
import net.minecraft.world.phys.BlockHitResult;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(SmallFireball.class)
public class SmallFireballEntityMixin {

    @Inject(method = "onHitBlock", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onBlockHit(BlockHitResult b, CallbackInfo ci) {
        SmallFireball self = (SmallFireball) (Object) this;
        if (self.getOwner() != null) {
            FlagEvaluator.checkMobGrief(self, ci);
        }
    }

}
