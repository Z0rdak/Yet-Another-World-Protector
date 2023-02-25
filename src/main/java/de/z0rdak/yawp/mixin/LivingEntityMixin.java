package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.handler.flags.FlagCheckEvent;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.damage.DamageSource;
import net.minecraft.entity.player.PlayerEntity;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.core.flag.RegionFlag.*;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;

@Mixin(LivingEntity.class)
public abstract class LivingEntityMixin {

    @Inject(method = "handleFallDamage", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void spawnChildFromBreeding(float fallDistance, float damageMultiplier, DamageSource damageSource, CallbackInfoReturnable<Boolean> cir) {
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
}
