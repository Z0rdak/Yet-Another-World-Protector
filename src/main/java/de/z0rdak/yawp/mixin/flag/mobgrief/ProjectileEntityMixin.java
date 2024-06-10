package de.z0rdak.yawp.mixin.flag.mobgrief;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import de.z0rdak.yawp.util.MobGriefingHelper;
import net.minecraft.entity.projectile.ProjectileEntity;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

@Mixin(ProjectileEntity.class)
public abstract class ProjectileEntityMixin {
    @Inject(method = "canModifyAt", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/World;getGameRules()Lnet/minecraft/world/GameRules;"), cancellable = true)
    public void onCanModifyAt(World world, BlockPos pos, CallbackInfoReturnable<Boolean> cir) {
        if (MobGriefingHelper.preventGrief(world, pos)) {
            cir.setReturnValue(false);
        }
    }
}
