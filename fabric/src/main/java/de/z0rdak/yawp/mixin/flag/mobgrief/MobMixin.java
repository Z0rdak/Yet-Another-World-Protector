package de.z0rdak.yawp.mixin.flag.mobgrief;

import de.z0rdak.yawp.api.FlagEvaluator;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.level.Level;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import static de.z0rdak.yawp.handler.HandlerUtil.isServerSide;

@Mixin(Mob.class)
public abstract class MobMixin {

    @Inject(method = "tick", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/entity/LivingEntity;tick()V", shift = At.Shift.AFTER), cancellable = true, allow = 1)
    void checkMobLooting(CallbackInfo ci) {
        Mob self = (Mob) (Object) this;
        Level world = self.level();
        if (isServerSide(world) && self.canPickUpLoot() && self.isAlive()) {
            FlagEvaluator.checkMobGrief(self, ci);
        }
    }

}
