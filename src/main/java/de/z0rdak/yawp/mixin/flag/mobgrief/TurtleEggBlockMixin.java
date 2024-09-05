package de.z0rdak.yawp.mixin.flag.mobgrief;

import de.z0rdak.yawp.handler.flags.HandlerUtil;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import net.minecraft.block.TurtleEggBlock;
import net.minecraft.entity.Entity;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.passive.BatEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.world.World;

@Mixin(TurtleEggBlock.class)
public abstract class TurtleEggBlockMixin {
    @Inject(method = "breaksEgg", at = @At(value = "HEAD"), cancellable = true)
    public void onBreaksEgg(World world, Entity entity, CallbackInfoReturnable<Boolean> cir) {
        if (entity instanceof PlayerEntity || entity instanceof BatEntity || ! (entity instanceof LivingEntity)) {
            return;
        }
        HandlerUtil.checkMobGrief(entity, cir);
    }
}
