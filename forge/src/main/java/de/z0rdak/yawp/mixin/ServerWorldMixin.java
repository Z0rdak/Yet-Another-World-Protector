package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.util.profiling.ProfilerFiller;
import net.minecraft.world.DifficultyInstance;
import net.minecraft.world.damagesource.DamageSource;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LightningBolt;
import net.minecraft.world.level.ChunkPos;
import net.minecraft.world.level.Explosion;
import net.minecraft.world.level.ExplosionDamageCalculator;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.chunk.LevelChunk;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;
import org.spongepowered.asm.mixin.injection.callback.LocalCapture;

import javax.annotation.Nullable;

import static de.z0rdak.yawp.core.flag.RegionFlag.*;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;

@Mixin(ServerLevel.class)
public class ServerWorldMixin {

    /**
     * Returning a null explosion will cause this event to be canceled.
     * An arrow on fire or fire charge shot by an e.g. dispenser will cause the type of the explosion to be ExplosionSourceType.TNT
     */
    @Inject(method = "explode", at = @At("HEAD"), cancellable = true, allow = 1)
    public void onIgniteExplosive(@Nullable Entity entity, @Nullable DamageSource damageSource, @Nullable ExplosionDamageCalculator behavior, double x, double y, double z, float power, boolean createFire, Level.ExplosionInteraction explosionMode, CallbackInfoReturnable<Explosion> cir) {
        ServerLevel world = (ServerLevel) (Object) this;
        if (isServerSide(world)) {
            if (explosionMode == Level.ExplosionInteraction.TNT ||
                    explosionMode == Level.ExplosionInteraction.BLOCK) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(new BlockPos((int) x, (int) y, (int) z), IGNITE_EXPLOSIVES, world.dimension(), null);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, denyResult -> {
                    cir.setReturnValue(null);
                });
            }
            if (explosionMode == Level.ExplosionInteraction.MOB) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(new BlockPos((int) x, (int) y, (int) z), MOB_GRIEFING, world.dimension(), null);
                if (Services.EVENT.post(checkEvent)) {
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
    @Inject(method = "tickChunk", locals = LocalCapture.CAPTURE_FAILSOFT, at = @At(value = "INVOKE", target = "Lnet/minecraft/world/entity/LightningBolt;setVisualOnly(Z)V"), cancellable = false, allow = 1)
    public void onSpawnLightning(LevelChunk chunk, int randomTickSpeed, CallbackInfo ci, ChunkPos chunkPos, boolean bl, int i, int j, ProfilerFiller profiler, BlockPos blockPos, DifficultyInstance localDifficulty, boolean b, LightningBolt lightningEntity) {
        if (isServerSide(chunk.getLevel())) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(blockPos, LIGHTNING_PROT, getDimKey(chunk.getLevel()), null);
            if (Services.EVENT.post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, null, deny -> {
                lightningEntity.remove(Entity.RemovalReason.DISCARDED);
                YetAnotherWorldProtector.LOGGER.info("Discarded 'minecraft:lightning_bolt' due to flag in region {}. You can ignore the warning printed by the vanilla code.", deny.getResponsible().getName());
            });
        }
    }
}
