package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.handler.flags.ExplosionDamageCalculatorInterceptor;
import net.minecraft.core.Holder;
import net.minecraft.core.particles.ParticleOptions;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.sounds.SoundEvent;
import net.minecraft.world.damagesource.DamageSource;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.level.Explosion;
import net.minecraft.world.level.ExplosionDamageCalculator;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.Vec3;
import org.jetbrains.annotations.Nullable;
import org.spongepowered.asm.mixin.*;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(Explosion.class)
public abstract class ExplosionMixin {

    @Final
    @Shadow
    @Mutable
    private ExplosionDamageCalculator damageCalculator;

    // Note: part of the explosion flag handling system is in ServerWorldMixin
    @Inject(method = "<init>(Lnet/minecraft/world/level/Level;Lnet/minecraft/world/entity/Entity;Lnet/minecraft/world/damagesource/DamageSource;Lnet/minecraft/world/level/ExplosionDamageCalculator;DDDFZLnet/minecraft/world/level/Explosion$BlockInteraction;Lnet/minecraft/core/particles/ParticleOptions;Lnet/minecraft/core/particles/ParticleOptions;Lnet/minecraft/core/Holder;)V", at = @At("TAIL"))
    public void interposeExplosionBehavior(
            Level level, 
            @Nullable Entity source, 
            @Nullable DamageSource damageSource, 
            @Nullable ExplosionDamageCalculator damageCalculator, 
            double x, double y, double z, 
            float radius, boolean fire, 
            Explosion.BlockInteraction blockInteraction, 
            ParticleOptions smallExplosionParticles, ParticleOptions largeExplosionParticles, 
            Holder<SoundEvent> explosionSound, 
            CallbackInfo ci
    ) {
        this.damageCalculator = new ExplosionDamageCalculatorInterceptor(this.damageCalculator, level);
    }
}
