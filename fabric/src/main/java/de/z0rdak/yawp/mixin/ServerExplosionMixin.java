package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.handler.flags.ExplosionDamageCalculatorInterceptor;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.damagesource.DamageSource;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.level.Explosion;
import net.minecraft.world.level.ExplosionDamageCalculator;
import net.minecraft.world.level.ServerExplosion;
import net.minecraft.world.phys.Vec3;
import org.jetbrains.annotations.Nullable;
import org.spongepowered.asm.mixin.*;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(ServerExplosion.class)
public abstract class ServerExplosionMixin {


    @Unique
    @Final
    private ServerLevel world;

    @Unique
    @Mutable
    private ExplosionDamageCalculator behavior;

    // Note: part of the explosion flag handling system is in ServerWorldMixin
    @Inject(method = "<init>", at = @At("TAIL"))
    private void interposeExplosionBehavior(
            ServerLevel level, 
            @Nullable Entity source, 
            @Nullable DamageSource damageSource, 
            @Nullable ExplosionDamageCalculator damageCalculator, 
            Vec3 center, float radius, 
            boolean fire, 
            Explosion.BlockInteraction blockInteraction,
            CallbackInfo ci
    ) {
        this.behavior = new ExplosionDamageCalculatorInterceptor(this.behavior);
    }
}
