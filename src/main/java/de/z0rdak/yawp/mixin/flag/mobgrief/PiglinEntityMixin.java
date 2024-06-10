package de.z0rdak.yawp.mixin.flag.mobgrief;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import de.z0rdak.yawp.util.MobGriefingHelper;
import net.minecraft.entity.mob.PiglinEntity;
import net.minecraft.entity.Entity;
import net.minecraft.item.ItemStack;

@Mixin(PiglinEntity.class)
public abstract class PiglinEntityMixin {
    Entity self = (PiglinEntity) (Object) this;
    @Inject(method = "canGather", at = @At(value = "HEAD"), cancellable = true)
    public void onCanGather(ItemStack stack, CallbackInfoReturnable<Boolean> cir) {
        if (MobGriefingHelper.preventGrief(self)) {
            cir.setReturnValue(false);
        }
    }
}
