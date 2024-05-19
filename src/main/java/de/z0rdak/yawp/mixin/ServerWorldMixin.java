package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.handler.flags.HandlerUtil;
import net.minecraft.core.BlockPos;
import net.minecraft.core.particles.ParticleOptions;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.sounds.SoundEvent;
import net.minecraft.world.damagesource.DamageSource;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.level.Explosion;
import net.minecraft.world.level.ExplosionDamageCalculator;
import net.minecraft.world.level.Level;
import net.neoforged.neoforge.common.NeoForge;
import org.jetbrains.annotations.Nullable;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.core.flag.RegionFlag.IGNITE_EXPLOSIVES;
import static de.z0rdak.yawp.core.flag.RegionFlag.MOB_GRIEFING;

@Mixin(ServerLevel.class)
public class ServerWorldMixin {

    /**
     * Returning a null explosion will cause this event to be canceled.
     * An arrow on fire or fire charge shot by an e.g. dispenser will cause the type of the explosion to be ExplosionSourceType.TNT
     */
    @Inject(method = "explode", at = @At("HEAD"), cancellable = true, allow = 1)
    public void onIgniteExplosive(@Nullable Entity entity, @org.jetbrains.annotations.Nullable DamageSource damageSource, @Nullable ExplosionDamageCalculator behavior, double x, double y, double z, float power, boolean createFire, Level.ExplosionInteraction explosionSourceType, ParticleOptions particle, ParticleOptions emitterParticle, SoundEvent soundEvent, CallbackInfoReturnable<Explosion> cir) {
        ServerLevel world = (ServerLevel) (Object) this;
        if (!world.isClientSide) {
            if (explosionSourceType == Level.ExplosionInteraction.TNT ||
                    explosionSourceType == Level.ExplosionInteraction.BLOCK) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(new BlockPos((int) x, (int) y, (int) z), IGNITE_EXPLOSIVES, world.dimension(), null);
                if (NeoForge.EVENT_BUS.post(checkEvent).isCanceled()) {
                    return;
                }
                HandlerUtil.processCheck(checkEvent, null, denyResult -> {
                    cir.setReturnValue(null);
                });
            }
            if (explosionSourceType == Level.ExplosionInteraction.MOB) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(new BlockPos((int) x, (int) y, (int) z), MOB_GRIEFING, world.dimension(), null);
                if (NeoForge.EVENT_BUS.post(checkEvent).isCanceled()) {
                    return;
                }
                HandlerUtil.processCheck(checkEvent, null, denyResult -> {
                    cir.setReturnValue(null);
                });
            }
        }
    }
}
