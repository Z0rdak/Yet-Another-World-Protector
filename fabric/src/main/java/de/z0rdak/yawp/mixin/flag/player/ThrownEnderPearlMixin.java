package de.z0rdak.yawp.mixin.flag.player;

import de.z0rdak.yawp.api.FlagEvaluator;
import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.entity.projectile.ThrownEnderpearl;
import net.minecraft.world.phys.HitResult;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import static de.z0rdak.yawp.core.flag.RegionFlag.USE_ENDERPEARL_FROM_REGION;
import static de.z0rdak.yawp.core.flag.RegionFlag.USE_ENDERPEARL_TO_REGION;
import static de.z0rdak.yawp.handler.HandlerUtil.*;
import static de.z0rdak.yawp.api.MessageSender.sendFlagMsg;

@Mixin(ThrownEnderpearl.class)
public abstract class ThrownEnderPearlMixin {

    @Inject(method = "onHit", at = @At(value = "INVOKE", target = "Lnet/minecraft/util/RandomSource;nextFloat()F"), cancellable = true, allow = 1)
    public void onThrowPearlIntoRegion(HitResult hitResult, CallbackInfo ci) {
        ThrownEnderpearl pearl = (ThrownEnderpearl) (Object) this;
        if (isServerSide(pearl.level())) {
            Entity owner = pearl.getOwner();
            if (owner instanceof Player player) {
                BlockPos targetBlockPos = new BlockPos(pearl.getBlockX(), pearl.getBlockY(), pearl.getBlockZ());
                FlagCheckEvent checkEvent = new FlagCheckEvent(targetBlockPos, USE_ENDERPEARL_TO_REGION, getDimKey(player), player);
                if (Services.EVENT.post(checkEvent))
                    return;
                FlagEvaluator.processCheck(checkEvent, deny -> {
                    sendFlagMsg(deny);
                    ci.cancel();
                    pearl.remove(Entity.RemovalReason.DISCARDED);
                });
            }
        }
    }

    @Inject(method = "onHit", at = @At(value = "INVOKE", target = "Lnet/minecraft/util/RandomSource;nextFloat()F"), cancellable = true, allow = 1)
    public void onThrowPearlOutOfRegion(HitResult hitResult, CallbackInfo ci) {
        ThrownEnderpearl pearl = (ThrownEnderpearl) (Object) this;
        if (isServerSide(pearl.level())) {
            Entity owner = pearl.getOwner();
            if (owner instanceof Player player) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(player.blockPosition(), USE_ENDERPEARL_FROM_REGION, getDimKey(player), player);
                if (Services.EVENT.post(checkEvent))
                    return;
                FlagEvaluator.processCheck(checkEvent, deny -> {
                    sendFlagMsg(deny);
                    ci.cancel();
                    pearl.remove(Entity.RemovalReason.DISCARDED);
                });
            }
        }
    }
}
