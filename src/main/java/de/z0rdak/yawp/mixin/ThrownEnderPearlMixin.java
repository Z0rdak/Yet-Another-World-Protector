package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.handler.flags.HandlerUtil;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.MessageSender;
import net.minecraft.entity.Entity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.projectile.thrown.EnderPearlEntity;
import net.minecraft.util.hit.HitResult;
import net.minecraft.util.math.BlockPos;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import static de.z0rdak.yawp.api.events.region.RegionEvents.post;
import static de.z0rdak.yawp.core.flag.RegionFlag.*;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;

@Mixin(EnderPearlEntity.class)
public abstract class ThrownEnderPearlMixin {

    @Inject(method = "onCollision", at = @At(value = "INVOKE", target = "Lnet/minecraft/util/math/random/Random;nextFloat()F"), cancellable = true, allow = 1)
    public void onThrowPearlIntoRegion(HitResult hitResult, CallbackInfo ci) {
        EnderPearlEntity pearl = (EnderPearlEntity) (Object) this;
        if (isServerSide(pearl.getWorld())) {
            Entity owner = pearl.getOwner();
            if (owner instanceof PlayerEntity player) {
                BlockPos targetBlockPos = new BlockPos(pearl.getBlockX(), pearl.getBlockY(), pearl.getBlockZ());
                FlagCheckEvent checkEvent = new FlagCheckEvent(targetBlockPos, USE_ENDERPEARL_TO_REGION, getEntityDim(player), player);
                if (post(checkEvent)) {
                    return;
                }
                HandlerUtil.processCheck(checkEvent, null, deny -> {
                    MessageSender.sendFlagMsg(deny);
                    ci.cancel();
                    pearl.remove(Entity.RemovalReason.KILLED);
                });
            }
        }
    }

    @Inject(method = "onCollision", at = @At(value = "INVOKE", target = "Lnet/minecraft/util/math/random/Random;nextFloat()F"), cancellable = true, allow = 1)
    public void onThrowPearlOutOfRegion(HitResult hitResult, CallbackInfo ci) {
        EnderPearlEntity pearl = (EnderPearlEntity) (Object) this;
        if (!pearl.getWorld().isClient) {
            Entity owner = pearl.getOwner();
            if (owner instanceof PlayerEntity player) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(player.getBlockPos(), USE_ENDERPEARL_FROM_REGION, getEntityDim(player), player);
                if (post(checkEvent)) {
                    return;
                }
                HandlerUtil.processCheck(checkEvent, null, deny -> {
                    MessageSender.sendFlagMsg(deny);
                    ci.cancel();
                    pearl.remove(Entity.RemovalReason.KILLED);
                });
            }
        }
    }
}
