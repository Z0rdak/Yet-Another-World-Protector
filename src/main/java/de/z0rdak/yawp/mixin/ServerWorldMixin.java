package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import net.minecraft.entity.Entity;
import net.minecraft.entity.effect.LightningBoltEntity;
import net.minecraft.profiler.IProfiler;
import net.minecraft.util.DamageSource;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.ChunkPos;
import net.minecraft.world.DifficultyInstance;
import net.minecraft.world.Explosion;
import net.minecraft.world.ExplosionContext;
import net.minecraft.world.chunk.Chunk;
import net.minecraft.world.server.ServerWorld;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;
import org.spongepowered.asm.mixin.injection.callback.LocalCapture;

import javax.annotation.Nullable;

import static de.z0rdak.yawp.api.events.region.RegionEvents.post;
import static de.z0rdak.yawp.core.flag.RegionFlag.IGNITE_EXPLOSIVES;
import static de.z0rdak.yawp.core.flag.RegionFlag.LIGHTNING_PROT;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.isServerSide;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.processCheck;

@Mixin(ServerWorld.class)
public class ServerWorldMixin {

    /**
     * Returning a null explosion will cause this event to be canceled.
     * An arrow on fire or fire charge shot by an e.g. dispenser will cause the type of the explosion to be ExplosionSourceType.TNT
     */
    @Inject(method = "explode", at = @At("HEAD"), cancellable = true, allow = 1)
    public void onIgniteExplosive(@Nullable Entity entity, @Nullable DamageSource damageSource, @Nullable ExplosionContext behavior, double x, double y, double z, float power, boolean createFire, Explosion.Mode explosionMode, CallbackInfoReturnable<Explosion> cir) {
        ServerWorld world = (ServerWorld) (Object) this;
        if (isServerSide(world)) {
            if (explosionMode == Explosion.Mode.BREAK || explosionMode == Explosion.Mode.DESTROY) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(new BlockPos((int) x, (int) y, (int) z), IGNITE_EXPLOSIVES, world.dimension(), null);
                if (post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, denyResult -> {
                    cir.setReturnValue(null);
                });
            }
        }
    }

    /**
     * Injection for lightning protection flag. It prevents lightning strikes which are not hitting entities and would potentially cause fire.
     */
    @Inject(method = "tickChunk", locals = LocalCapture.CAPTURE_FAILSOFT, at = @At(value = "INVOKE", target = "Lnet/minecraft/entity/effect/LightningBoltEntity;setVisualOnly(Z)V"), cancellable = false, allow = 1)
    public void onSpawnLightning(Chunk chunk, int randomTickSpeed, CallbackInfo ci, ChunkPos chunkPos, boolean bl, int i, int j, IProfiler profiler, BlockPos blockPos, DifficultyInstance localDifficulty, boolean b, LightningBoltEntity lightningEntity) {
        if (isServerSide(chunk.getLevel())) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(blockPos, LIGHTNING_PROT, chunk.getLevel().dimension());
            if (post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, deny -> {
                lightningEntity.remove(false);
                YetAnotherWorldProtector.LOGGER.info("Discarded 'minecraft:lightning_bolt' due to flag in region {}. You can ignore the warning printed by the vanilla code.", deny.getResponsible().getName());
            });
        }
    }
}
