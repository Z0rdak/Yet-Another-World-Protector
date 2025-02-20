package de.z0rdak.yawp.mixin.flag.mobgrief;

import de.z0rdak.yawp.api.FlagEvaluator;
import de.z0rdak.yawp.handler.HandlerUtil;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ambient.Bat;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.TurtleEggBlock;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(TurtleEggBlock.class)
public abstract class TurtleEggBlockMixin {
    @Inject(method = "canDestroyEgg", at = @At(value = "HEAD"), cancellable = true)
    public void onBreaksEgg(Level world, Entity entity, CallbackInfoReturnable<Boolean> cir) {
        if (entity instanceof Player || entity instanceof Bat || !(entity instanceof LivingEntity)) {
            return;
        }
        FlagEvaluator.checkMobGrief(entity, cir);
    }
}