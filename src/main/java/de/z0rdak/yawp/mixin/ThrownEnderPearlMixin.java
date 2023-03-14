package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.handler.flags.FlagCheckEvent;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.entity.Entity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.projectile.thrown.EnderPearlEntity;
import net.minecraft.util.hit.HitResult;
import net.minecraft.util.math.BlockPos;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;

@Mixin(EnderPearlEntity.class)
public abstract class ThrownEnderPearlMixin {

    @Inject(method = "onCollision", at = @At(value = "INVOKE", target = "Lnet/minecraft/util/math/random/Random;nextFloat()F"), cancellable = true, allow = 1)
    public void onThrowPearlIntoRegion(HitResult hitResult, CallbackInfo ci) {
        EnderPearlEntity pearl = (EnderPearlEntity) (Object) this;
        if (!pearl.world.isClient) {
            Entity owner = pearl.getOwner();
            if (owner instanceof PlayerEntity player) {
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(owner));
                BlockPos targetBlockPos = new BlockPos(pearl.getBlockX(), pearl.getBlockY(), pearl.getBlockZ());

                FlagCheckEvent.PlayerFlagEvent enderPearlToCheck = checkPlayerEvent(player, targetBlockPos, RegionFlag.USE_ENDERPEARL_TO_REGION, dimCache.getDimensionalRegion());
                if (enderPearlToCheck.isDenied()) {
                    sendFlagDeniedMsg(enderPearlToCheck);
                    ci.cancel();
                    pearl.remove(Entity.RemovalReason.KILLED);
                }
            }
        }
    }

    @Inject(method = "onCollision", at = @At(value = "INVOKE", target = "Lnet/minecraft/util/math/random/Random;nextFloat()F"), cancellable = true, allow = 1)
    public void onThrowPearlOutOfRegion(HitResult hitResult, CallbackInfo ci) {
        EnderPearlEntity pearl = (EnderPearlEntity) (Object) this;
        if (!pearl.world.isClient) {
            Entity owner = pearl.getOwner();
            if (owner instanceof PlayerEntity player) {
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(owner));
                FlagCheckEvent.PlayerFlagEvent enderPearlFromCheck = checkPlayerEvent(player, player.getBlockPos(), RegionFlag.USE_ENDERPEARL_FROM_REGION, dimCache.getDimensionalRegion());
                if (enderPearlFromCheck.isDenied()) {
                    sendFlagDeniedMsg(enderPearlFromCheck);
                    ci.cancel();
                    pearl.remove(Entity.RemovalReason.KILLED);
                }
            }
        }
    }
}
