package de.z0rdak.yawp.mixin.flag;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import net.minecraft.entity.Entity;
import net.minecraft.entity.LightningEntity;
import net.minecraft.entity.damage.DamageSource;
import net.minecraft.server.world.ServerWorld;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.ChunkPos;
import net.minecraft.util.profiler.Profiler;
import net.minecraft.world.LocalDifficulty;
import net.minecraft.world.World.ExplosionSourceType;
import net.minecraft.world.chunk.WorldChunk;
import net.minecraft.world.explosion.Explosion;
import net.minecraft.world.explosion.ExplosionBehavior;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;
import org.spongepowered.asm.mixin.injection.callback.LocalCapture;

import static de.z0rdak.yawp.api.events.region.RegionEvents.post;
import static de.z0rdak.yawp.core.flag.RegionFlag.*;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;

@Mixin(ServerWorld.class)
public class ServerWorldMixin {

    @Inject(method = "addEntity", at = @At("HEAD"), cancellable = true, allow = 1)
    public void onSpawnEntity(Entity entity, CallbackInfoReturnable<Boolean> cir) {
        if (isServerSide(entity.getEntityWorld())) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(entity.getBlockPos(), SPAWNING_ALL, getDimKey(entity), null);
            if (post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, null, deny -> {
                cir.setReturnValue(false);
            });

            checkEvent = new FlagCheckEvent(entity.getBlockPos(), SPAWNING_MONSTER, getDimKey(entity), null);
            if (post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, null, deny -> {
                cir.setReturnValue(false);
            });
            
            checkEvent = new FlagCheckEvent(entity.getBlockPos(), SPAWNING_ANIMAL, getDimKey(entity), null);
            if (post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, null, deny -> {
                cir.setReturnValue(false);
            });
            
            checkEvent = new FlagCheckEvent(entity.getBlockPos(), SPAWNING_VILLAGER, getDimKey(entity), null);
            if (post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, null, deny -> {
                cir.setReturnValue(false);
            });
            
            checkEvent = new FlagCheckEvent(entity.getBlockPos(), SPAWNING_TRADER, getDimKey(entity), null);
            if (post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, null, deny -> {
                cir.setReturnValue(false);
            });
            
            checkEvent = new FlagCheckEvent(entity.getBlockPos(), SPAWNING_GOLEM, getDimKey(entity), null);
            if (post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, null, deny -> {
                cir.setReturnValue(false);
            });
            
            checkEvent = new FlagCheckEvent(entity.getBlockPos(), SPAWNING_SLIME, getDimKey(entity), null);
            if (post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, null, deny -> {
                cir.setReturnValue(false);
            });
            
            checkEvent = new FlagCheckEvent(entity.getBlockPos(), SPAWNING_XP, getDimKey(entity), null);
            if (post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, null, deny -> {
                cir.setReturnValue(false);
            });
        }
    }

    /**
     * Returning a null explosion will cause this event to be canceled.
     * An arrow on fire or fire charge shot by an e.g. dispenser will cause the type of the explosion to be ExplosionSourceType.TNT
     */
    @Inject(method = "createExplosion", at = @At("HEAD"), cancellable = true, allow = 1)
    public void onIgniteExplosive(Entity entity, DamageSource damageSource, ExplosionBehavior behavior, double x, double y, double z, float power, boolean createFire, ExplosionSourceType explosionSourceType, CallbackInfoReturnable<Explosion> cir) {
        ServerWorld world = (ServerWorld) (Object) this;
        if (isServerSide(world)) {         
            if (explosionSourceType == ExplosionSourceType.TNT || explosionSourceType == ExplosionSourceType.BLOCK) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(new BlockPos((int) x, (int) y, (int) z), IGNITE_EXPLOSIVES, getDimKey(entity), null);
                if (post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, null, deny -> {
                    cir.setReturnValue(null);
                });
            }
            if (explosionSourceType == ExplosionSourceType.MOB) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(new BlockPos((int) x, (int) y, (int) z), MOB_GRIEFING, getDimKey(entity), null);
                if (post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, null, deny -> {
                    cir.setReturnValue(null);
                });
            }
        }
    }

    /**
     * Injection for lightning protection flag. It prevents lightning strikes which are not hitting entities and would potentially cause fire.
     */
    @Inject(method = "tickChunk", locals = LocalCapture.CAPTURE_FAILSOFT, at = @At(value = "INVOKE", target = "Lnet/minecraft/entity/LightningEntity;setCosmetic(Z)V"), cancellable = true, allow = 1)
    public void onSpawnLightning(WorldChunk chunk, int randomTickSpeed, CallbackInfo ci, ChunkPos chunkPos, boolean bl, int i, int j, Profiler profiler, BlockPos blockPos, LocalDifficulty localDifficulty, boolean b, LightningEntity lightningEntity) {
        if (isServerSide(chunk.getWorld())) {            
            FlagCheckEvent checkEvent = new FlagCheckEvent(blockPos, LIGHTNING_PROT, getDimKey(chunk.getWorld()), null);
            if (post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, null, deny -> {
                lightningEntity.remove(Entity.RemovalReason.DISCARDED);
                YetAnotherWorldProtector.LOGGER.info("Discarded 'minecraft:lightning_bolt' due to flag in region {}. You can ignore the warning printed by the vanilla code.", deny.getResponsible().getName());
            });
        }
    }
}
