package de.z0rdak.yawp.mixin.flag.mobgrief;

import de.z0rdak.yawp.handler.flags.HandlerUtil;
import net.minecraft.entity.ItemEntity;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import net.minecraft.entity.mob.PiglinEntity;
import net.minecraft.entity.Entity;
import net.minecraft.item.ItemStack;

@Mixin(PiglinEntity.class)
public abstract class PiglinEntityMixin {
    @Unique
    Entity self = (PiglinEntity) (Object) this;
    @Inject(method = "canGather", at = @At(value = "HEAD"), cancellable = true)
    public void onCanGather(ItemStack stack, CallbackInfoReturnable<Boolean> cir) {        
        HandlerUtil.checkMobGrief(self.getWorld(), self.getBlockPos(), cir);
    }

}
