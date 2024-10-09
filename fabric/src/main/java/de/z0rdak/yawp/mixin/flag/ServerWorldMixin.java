package de.z0rdak.yawp.mixin.flag;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.constants.Constants;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.util.profiling.ProfilerFiller;
import net.minecraft.world.DifficultyInstance;
import net.minecraft.world.damagesource.DamageSource;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.ExperienceOrb;
import net.minecraft.world.entity.LightningBolt;
import net.minecraft.world.entity.animal.IronGolem;
import net.minecraft.world.entity.animal.SnowGolem;
import net.minecraft.world.entity.monster.Slime;
import net.minecraft.world.entity.npc.WanderingTrader;
import net.minecraft.world.level.ChunkPos;
import net.minecraft.world.level.Explosion;
import net.minecraft.world.level.ExplosionDamageCalculator;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.chunk.LevelChunk;
import org.jetbrains.annotations.Nullable;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;
import org.spongepowered.asm.mixin.injection.callback.LocalCapture;

import static de.z0rdak.yawp.api.events.region.FabricRegionEvents.post;
import static de.z0rdak.yawp.core.flag.RegionFlag.*;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;

@Mixin(ServerLevel.class)
public class ServerWorldMixin {

    @Inject(method = "addEntity", at = @At("HEAD"), cancellable = true, allow = 1)
    public void onSpawnEntity(Entity entity, CallbackInfoReturnable<Boolean> cir) {
        if (isServerSide(entity.level())) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(entity.blockPosition(), SPAWNING_ALL, getDimKey(entity), null);
            if (post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, null, deny -> {
                cir.setReturnValue(false);
            });

            if (isMonster(entity)) {
                checkEvent = new FlagCheckEvent(entity.blockPosition(), SPAWNING_MONSTER, getDimKey(entity), null);
                if (post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, null, deny -> {
                    cir.setReturnValue(false);
                });
            }
            if (isAnimal(entity)) {
                checkEvent = new FlagCheckEvent(entity.blockPosition(), SPAWNING_ANIMAL, getDimKey(entity), null);
                if (post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, null, deny -> {
                    cir.setReturnValue(false);
                });
            }
            if (isVillager(entity)) {
                checkEvent = new FlagCheckEvent(entity.blockPosition(), SPAWNING_VILLAGER, getDimKey(entity), null);
                if (post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, null, deny -> {
                    cir.setReturnValue(false);
                });
            }
            if (entity instanceof WanderingTrader) {
                checkEvent = new FlagCheckEvent(entity.blockPosition(), SPAWNING_TRADER, getDimKey(entity), null);
                if (post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, null, deny -> {
                    cir.setReturnValue(false);
                });
            }
            if (entity instanceof SnowGolem || entity instanceof IronGolem) {
                checkEvent = new FlagCheckEvent(entity.blockPosition(), SPAWNING_GOLEM, getDimKey(entity), null);
                if (post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, null, deny -> {
                    cir.setReturnValue(false);
                });
            }
            if (entity instanceof Slime) {
                checkEvent = new FlagCheckEvent(entity.blockPosition(), SPAWNING_SLIME, getDimKey(entity), null);
                if (post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, null, deny -> {
                    cir.setReturnValue(false);
                });
            }
            if (entity instanceof ExperienceOrb) {
                checkEvent = new FlagCheckEvent(entity.blockPosition(), SPAWNING_XP, getDimKey(entity), null);
                if (post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, null, deny -> {
                    cir.setReturnValue(false);
                });
            }
        }
    }

    /**
     * Returning a null explosion will cause this event to be canceled.
     * An arrow on fire or fire charge shot by an e.g. dispenser will cause the type of the explosion to be ExplosionSourceType.TNT
     */
    @Inject(method = "explode", at = @At("HEAD"), cancellable = true, allow = 1)
    public void onIgniteExplosive(@Nullable Entity entity, @Nullable DamageSource damageSource, @Nullable ExplosionDamageCalculator explosionDamageCalculator, double x, double y, double z, float g, boolean bl, Level.ExplosionInteraction explosionInteraction, CallbackInfoReturnable<Explosion> cir) {
        ServerLevel world = (ServerLevel) (Object) this;
        if (isServerSide(world)) {
            if (explosionInteraction == Level.ExplosionInteraction.TNT || explosionInteraction == Level.ExplosionInteraction.BLOCK) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(new BlockPos((int) x, (int) y, (int) z), IGNITE_EXPLOSIVES, getDimKey(world));
                if (post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, deny -> cir.setReturnValue(null));
            }
            if (explosionInteraction == Level.ExplosionInteraction.MOB) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(new BlockPos((int) x, (int) y, (int) z), MOB_GRIEFING, getDimKey(world));
                if (post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, deny -> cir.setReturnValue(null));
            }
        }
    }

    /**
     * Injection for lightning protection flag. It prevents lightning strikes which are not hitting entities and would potentially cause fire.
     */
    @Inject(method = "tickChunk", locals = LocalCapture.CAPTURE_FAILSOFT, at = @At(value = "INVOKE", target = "Lnet/minecraft/world/entity/LightningBolt;setVisualOnly(Z)V"), cancellable = false, allow = 1)
    public void onSpawnLightning(LevelChunk chunk, int randomTickSpeed, CallbackInfo ci, ChunkPos chunkPos, boolean bl, int i, int j, ProfilerFiller profiler, BlockPos blockPos, DifficultyInstance localDifficulty, boolean b, LightningBolt lightningEntity) {
        if (isServerSide(chunk.getLevel())) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(blockPos, LIGHTNING_PROT, getDimKey(chunk.getLevel()));
            if (post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, deny -> {
                lightningEntity.remove(Entity.RemovalReason.DISCARDED);
                Constants.LOGGER.info("Discarded 'minecraft:lightning_bolt' due to flag in region {}. You can ignore the warning printed by the vanilla code.", deny.getResponsible().getName());
            });
        }
    }
}
