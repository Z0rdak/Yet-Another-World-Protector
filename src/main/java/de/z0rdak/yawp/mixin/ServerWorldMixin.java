package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.handler.flags.FlagCheckEvent;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.entity.Entity;
import net.minecraft.entity.ExperienceOrbEntity;
import net.minecraft.entity.damage.DamageSource;
import net.minecraft.entity.mob.SlimeEntity;
import net.minecraft.entity.passive.IronGolemEntity;
import net.minecraft.entity.passive.SnowGolemEntity;
import net.minecraft.entity.passive.VillagerEntity;
import net.minecraft.entity.passive.WanderingTraderEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.server.world.ServerWorld;
import net.minecraft.util.math.BlockPos;
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
        if (!entity.world.isClient) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(entity));
            FlagCheckEvent flagCheck = checkTargetEvent(entity.getBlockPos(), SPAWNING_ALL, dimCache.getDimensionalRegion());
            if (flagCheck.isDenied()) {
                cir.setReturnValue(false);
                return;
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
    public void onIgniteExplosive(@Nullable Entity entity, @Nullable DamageSource damageSource, @Nullable ExplosionBehavior behavior, double x, double y, double z, float power, boolean createFire, Explosion.DestructionType destructionType, CallbackInfoReturnable<Boolean> cir) {
        ServerWorld world = (ServerWorld) (Object) this;
        Explosion explosion = new Explosion(world, entity, damageSource, behavior, x, y, z, power, createFire, destructionType);
        if (!world.isClient) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(world.getRegistryKey());
            FlagCheckEvent flagCheck = checkTargetEvent(new BlockPos(x, y, z), IGNITE_EXPLOSIVES, dimCache.getDimensionalRegion());
            if (flagCheck.isDenied()) {
                if (explosion.getDamageSource().getSource() instanceof PlayerEntity player) {
                    sendFlagDeniedMsg(flagCheck, player);
                }
                cir.setReturnValue(false);
            }
        }
    }

}
