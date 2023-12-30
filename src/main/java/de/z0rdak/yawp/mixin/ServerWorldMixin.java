package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.handler.flags.FlagCheckEvent;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.entity.Entity;
import net.minecraft.entity.ExperienceOrbEntity;
import net.minecraft.entity.TntEntity;
import net.minecraft.entity.damage.DamageSource;
import net.minecraft.entity.mob.MobEntity;
import net.minecraft.entity.mob.SlimeEntity;
import net.minecraft.entity.passive.IronGolemEntity;
import net.minecraft.entity.passive.SnowGolemEntity;
import net.minecraft.entity.passive.VillagerEntity;
import net.minecraft.entity.passive.WanderingTraderEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.particle.ParticleEffect;
import net.minecraft.server.world.ServerWorld;
import net.minecraft.sound.SoundEvent;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraft.world.World.ExplosionSourceType;
import net.minecraft.world.explosion.Explosion;
import net.minecraft.world.explosion.ExplosionBehavior;
import org.jetbrains.annotations.Nullable;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.core.flag.RegionFlag.*;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;

@Mixin(ServerWorld.class)
public class ServerWorldMixin {

    @Inject(method = "addEntity", at = @At("HEAD"), cancellable = true, allow = 1)
    public void onSpawnEntity(Entity entity, CallbackInfoReturnable<Boolean> cir) {
        if (!entity.getWorld().isClient) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(entity));
            FlagCheckEvent flagCheck = checkTargetEvent(entity.getBlockPos(), SPAWNING_ALL, dimCache.getDimensionalRegion());
            if (entity instanceof MobEntity) {
                if (flagCheck.isDenied()) {
                    cir.setReturnValue(false);
                    return;
                }
            }
            if (isMonster(entity)) {
                flagCheck = checkTargetEvent(entity.getBlockPos(), SPAWNING_MONSTER, dimCache.getDimensionalRegion());
                if (flagCheck.isDenied()) {
                    cir.setReturnValue(false);
                }
            }
            if (isAnimal(entity)) {
                flagCheck = checkTargetEvent(entity.getBlockPos(), SPAWNING_ANIMAL, dimCache.getDimensionalRegion());
                if (flagCheck.isDenied()) {
                    cir.setReturnValue(false);
                }
            }
            if (entity instanceof VillagerEntity) {
                flagCheck = checkTargetEvent(entity.getBlockPos(), SPAWNING_VILLAGER, dimCache.getDimensionalRegion());
                if (flagCheck.isDenied()) {
                    cir.setReturnValue(false);
                }
            }
            if (entity instanceof WanderingTraderEntity) {
                flagCheck = checkTargetEvent(entity.getBlockPos(), SPAWNING_TRADER, dimCache.getDimensionalRegion());
                if (flagCheck.isDenied()) {
                    cir.setReturnValue(false);
                }
            }
            if (entity instanceof SnowGolemEntity || entity instanceof IronGolemEntity) {
                flagCheck = checkTargetEvent(entity.getBlockPos(), SPAWNING_GOLEM, dimCache.getDimensionalRegion());
                if (flagCheck.isDenied()) {
                    cir.setReturnValue(false);
                }
            }
            if (entity instanceof SlimeEntity) {
                flagCheck = checkTargetEvent(entity.getBlockPos(), SPAWNING_SLIME, dimCache.getDimensionalRegion());
                if (flagCheck.isDenied()) {
                    cir.setReturnValue(false);
                }
            }
            if (entity instanceof ExperienceOrbEntity) {
                flagCheck = checkTargetEvent(entity.getBlockPos(), SPAWNING_XP, dimCache.getDimensionalRegion());
                if (flagCheck.isDenied()) {
                    cir.setReturnValue(false);
                }
            }

        }

    }

    @Inject(method = "createExplosion", at = @At("HEAD"), cancellable = true, allow = 1)
    public void onIgniteExplosive(@Nullable Entity entity, @Nullable DamageSource damageSource, @Nullable ExplosionBehavior behavior, double x, double y, double z, float power, boolean createFire, World.ExplosionSourceType explosionSourceType, ParticleEffect particle, ParticleEffect emitterParticle, SoundEvent soundEvent, CallbackInfoReturnable<Explosion> cir) {
        ServerWorld world = (ServerWorld) (Object) this;
        DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(world.getRegistryKey());
        // Extra check for player initiated explosion here. Should normally be prevented by not allowing 
        // the ignition to begin with.
        if (entity instanceof TntEntity tntEntity && tntEntity.getOwner() instanceof PlayerEntity player) {
            FlagCheckEvent flagCheck = checkPlayerEvent(player, new BlockPos((int) x, (int) y, (int) z), IGNITE_EXPLOSIVES, dimCache.getDimensionalRegion());
            if (flagCheck.isDenied()) {
                sendFlagDeniedMsg(flagCheck, player);
                Explosion explosion = world.createExplosion(entity, damageSource, behavior, x, y, z, power, createFire, ExplosionSourceType.NONE, particle, emitterParticle, soundEvent);
                cir.setReturnValue(explosion);
            }
        }
        if (explosionSourceType == ExplosionSourceType.MOB) {
            FlagCheckEvent flagCheck = checkTargetEvent(new BlockPos((int) x, (int) y, (int) z), MOB_GRIEFING, dimCache.getDimensionalRegion());
            if (flagCheck.isDenied()) {
                Explosion explosion = world.createExplosion(entity, damageSource, behavior, x, y, z, power, createFire, ExplosionSourceType.NONE, particle, emitterParticle, soundEvent);
                cir.setReturnValue(explosion);
            }
        }
    }
}
