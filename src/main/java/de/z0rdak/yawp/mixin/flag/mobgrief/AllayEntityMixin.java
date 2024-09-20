package de.z0rdak.yawp.mixin.flag.mobgrief;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import de.z0rdak.yawp.util.MobGriefingHelper;
import net.minecraft.entity.passive.AllayEntity;
import net.minecraft.entity.Entity;
import net.minecraft.item.ItemStack;

@Mixin(AllayEntity.class)
public abstract class AllayEntityMixin {
    Entity self = (AllayEntity) (Object) this;
    @Inject(method = "canGather", at = @At(value = "HEAD"), cancellable = true)
    public void onCanGather(ItemStack stack, CallbackInfoReturnable<Boolean> cir) {     
        // TODO: self pos is not correct - it should be the pos of the stack entity in the world
        if (MobGriefingHelper.preventGrief(self)) {
            cir.setReturnValue(false);
        }
    }
}
