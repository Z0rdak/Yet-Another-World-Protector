package de.z0rdak.yawp.mixin.mobgrief;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import de.z0rdak.yawp.util.MobGriefingHelper;
import net.minecraft.entity.mob.MobEntity;
import net.minecraft.world.World;

@Mixin(MobEntity.class)
public abstract class MobEntityMixin {

    @Inject(method = "tickMovement()V", at = @At(value = "INVOKE", target = "Lnet/minecraft/entity/LivingEntity;tickMovement()V", shift = At.Shift.AFTER), cancellable = true, allow = 1)
    void onTickMovement(CallbackInfo ci) {
        MobEntity self = (MobEntity) (Object) this;
        World world = self.getWorld();
        if (!world.isClient && self.canPickUpLoot() && self.isAlive()) {
            if (MobGriefingHelper.preventGrief(world, self)) {
                ci.cancel();
            }
        }
    }

}
