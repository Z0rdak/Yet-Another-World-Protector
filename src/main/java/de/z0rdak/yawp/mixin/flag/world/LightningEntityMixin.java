package de.z0rdak.yawp.mixin.flag.world;

import net.minecraft.entity.LightningEntity;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(LightningEntity.class)
public abstract class LightningEntityMixin {

    /**
     * lightning-oxidation-removal
     * Prevent oxidation removal from blocks when lightning hits
     *
     * @param ci
     */
    @Inject(method = "tick", at = @At(value = "INVOKE", target = "Lnet/minecraft/entity/LightningEntity;cleanOxidation(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)V"), cancellable = true, allow = 1)
    public void cleanOxidationOnHitBlock(CallbackInfo ci) {
        LightningEntity pearl = (LightningEntity) (Object) this;
    }

    /**
     * lighting-fire-spread
     * Prevent fire spreading when lightning hits blocks
     *
     * @param spreadAttempts
     * @param ci
     */
    @Inject(method = "spawnFire", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onSpawnFireFromLightning(int spreadAttempts, CallbackInfo ci) {
        LightningEntity pearl = (LightningEntity) (Object) this;
    }

    /**
     * lightning-power-rods
     * Still hit lightning rod but prevent powering it
     *
     * @param ci
     */
    @Inject(method = "powerLightningRod", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onPowerLightningRodFromLightning(CallbackInfo ci) {
        LightningEntity pearl = (LightningEntity) (Object) this;
    }
}
