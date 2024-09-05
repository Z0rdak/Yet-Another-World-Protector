package de.z0rdak.yawp.mixin.flag.mobgrief;

import de.z0rdak.yawp.handler.flags.HandlerUtil;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import net.minecraft.entity.mob.MobEntity;
import net.minecraft.world.World;

import static de.z0rdak.yawp.handler.flags.HandlerUtil.isServerSide;

@Mixin(MobEntity.class)
public abstract class MobEntityMixin {

    @Inject(method = "tickMovement()V", at = @At(value = "INVOKE", target = "Lnet/minecraft/entity/LivingEntity;tickMovement()V", shift = At.Shift.AFTER), cancellable = true, allow = 1)
    void chekcMobLooting(CallbackInfo ci) {
        MobEntity self = (MobEntity) (Object) this;
        World world = self.getWorld();
        if (isServerSide(world) && self.canPickUpLoot() && self.isAlive()) {
            HandlerUtil.checkMobGrief(self, ci);
        }
    }

}
