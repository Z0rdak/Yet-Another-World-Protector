package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.handler.flags.FlagCheckEvent;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.entity.ExperienceOrbEntity;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.damage.DamageSource;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.server.world.ServerWorld;
import org.jetbrains.annotations.Nullable;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.core.flag.RegionFlag.*;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;

@Mixin(LivingEntity.class)
public abstract class LivingEntityMixin {

    @Shadow
    @Nullable
    protected PlayerEntity attackingPlayer;

    @Inject(method = "takeKnockback", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onKnockback(double strength, double x, double z, CallbackInfo ci) {
        LivingEntity target = (LivingEntity) (Object) this;
        if (isServerSide(target)) {
            if (target instanceof PlayerEntity) {
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(target));
                FlagCheckEvent flagCheckEvent = checkTargetEvent(target.getBlockPos(), KNOCKBACK_PLAYERS, dimCache.getDimensionalRegion());
                if (flagCheckEvent.isDenied()) {
                    ci.cancel();
                }
                flagCheckEvent = checkTargetEvent(target.getBlockPos(), INVINCIBLE, dimCache.getDimensionalRegion());
                if (flagCheckEvent.isDenied()) {
                    ci.cancel();
                }

            }

        }
    }

    // FIXME: Separate flags for dropLoot -> mobs, etc AND dropInventory ->
    @Inject(method = "drop", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onDrop(DamageSource source, CallbackInfo ci) {
        LivingEntity target = (LivingEntity) (Object) this;
        if (isServerSide(target)) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(target));
            FlagCheckEvent flagCheckEvent = checkTargetEvent(target.getBlockPos(), DROP_LOOT_ALL, dimCache.getDimensionalRegion());
            if (flagCheckEvent.isDenied()) {
                ci.cancel();
            }

            if (source.getSource() instanceof PlayerEntity player) {
                FlagCheckEvent.PlayerFlagEvent playerFlagCheckEvent = checkPlayerEvent(player, target.getBlockPos(), DROP_LOOT_PLAYER, dimCache.getDimensionalRegion());
                if (playerFlagCheckEvent.isDenied()) {
                    ci.cancel();
                }
            }
        }
    }

    @Inject(method = "handleFallDamage", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onFallDamage(float fallDistance, float damageMultiplier, DamageSource damageSource, CallbackInfoReturnable<Boolean> cir) {
        LivingEntity self = (LivingEntity) (Object) this;
        if (!self.world.isClient) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(self));
            FlagCheckEvent flagCheck = checkTargetEvent(self.getBlockPos(), FALL_DAMAGE, dimCache.getDimensionalRegion());
            if (flagCheck.isDenied()) {
                cir.setReturnValue(false);
                return;
            }
            if (isMonster(self)) {
                flagCheck = checkTargetEvent(self.getBlockPos(), FALL_DAMAGE_MONSTERS, dimCache.getDimensionalRegion());
                if (flagCheck.isDenied()) {
                    cir.setReturnValue(false);
                }

            }
            if (isAnimal(self)) {
                flagCheck = checkTargetEvent(self.getBlockPos(), FALL_DAMAGE_ANIMALS, dimCache.getDimensionalRegion());
                if (flagCheck.isDenied()) {
                    cir.setReturnValue(false);
                }
            }
            if (isVillager(self)) {
                flagCheck = checkTargetEvent(self.getBlockPos(), FALL_DAMAGE_VILLAGERS, dimCache.getDimensionalRegion());
                if (flagCheck.isDenied()) {
                    cir.setReturnValue(false);
                }
            }
            if (isPlayer(self)) {
                FlagCheckEvent.PlayerFlagEvent playerFlagCheck = checkPlayerEvent((PlayerEntity) self, self.getBlockPos(), FALL_DAMAGE_PLAYERS, dimCache.getDimensionalRegion());
                if (playerFlagCheck.isDenied()) {
                    cir.setReturnValue(false);
                }
            }
        }
    }

    @Inject(method = "dropXp", at = @At(value = "INVOKE", target = "Lnet/minecraft/entity/ExperienceOrbEntity;spawn(Lnet/minecraft/server/world/ServerWorld;Lnet/minecraft/util/math/Vec3d;I)V"), cancellable = true, allow = 1)
    public void onXpDrop(CallbackInfo ci) {
        LivingEntity self = (LivingEntity) (Object) this;
        DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(self));
        DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
        FlagCheckEvent flagCheckEvent = checkTargetEvent(self.getBlockPos(), RegionFlag.XP_DROP_ALL, dimRegion);
        if (flagCheckEvent.isDenied()) {
            ExperienceOrbEntity.spawn((ServerWorld) self.world, self.getPos(), 0);
            ci.cancel();
            return;
        }
        if (this.attackingPlayer != null) {
            FlagCheckEvent.PlayerFlagEvent playerFlagCheckEvent = checkPlayerEvent(this.attackingPlayer, self.getBlockPos(), RegionFlag.XP_DROP_PLAYER, dimCache.getDimensionalRegion());
            if (playerFlagCheckEvent.isDenied()) {
                sendFlagDeniedMsg(playerFlagCheckEvent);
                ExperienceOrbEntity.spawn((ServerWorld) self.world, self.getPos(), 0);
                ci.cancel();

            }
        }
        if (isMonster(self)) {
            flagCheckEvent = checkTargetEvent(self.getBlockPos(), RegionFlag.XP_DROP_MONSTER, dimRegion);
        } else {
            flagCheckEvent = checkTargetEvent(self.getBlockPos(), RegionFlag.XP_DROP_OTHER, dimRegion);
        }
        if (flagCheckEvent.isDenied()) {
            ExperienceOrbEntity.spawn((ServerWorld) self.world, self.getPos(), 0);
            ci.cancel();
        }


    }
}
