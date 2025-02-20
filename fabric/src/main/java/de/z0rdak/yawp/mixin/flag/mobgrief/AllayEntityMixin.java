package de.z0rdak.yawp.mixin.flag.mobgrief;

import de.z0rdak.yawp.api.FlagEvaluator;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.animal.allay.Allay;
import net.minecraft.world.item.ItemStack;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(Allay.class)
public abstract class AllayEntityMixin {
    @Unique
    Entity self = (Allay) (Object) this;
    @Inject(method = "wantsToPickUp", at = @At(value = "HEAD"), cancellable = true)
    public void onCanGather(ItemStack stack, CallbackInfoReturnable<Boolean> cir) {
        FlagEvaluator.checkMobGrief(self.level(), self.blockPosition(), cir);
    }
}
