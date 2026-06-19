package de.z0rdak.yawp.mixin.flag.player;

import de.z0rdak.yawp.api.FlagEvaluator;
import de.z0rdak.yawp.api.FlagRegister;
import de.z0rdak.yawp.api.events.flag.FlagCheckRequest;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.entity.projectile.throwableitemprojectile.ThrownEnderpearl;
import net.minecraft.world.phys.HitResult;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import static de.z0rdak.yawp.api.MessageSender.sendFlagMsg;
import static de.z0rdak.yawp.handler.HandlerUtil.getDimKey;
import static de.z0rdak.yawp.handler.HandlerUtil.isServerSide;

@Mixin(ThrownEnderpearl.class)
public abstract class ThrownEnderPearlMixin {

    @Inject(method = "onHit", at = @At(value = "INVOKE", target = "Lnet/minecraft/util/RandomSource;nextFloat()F"), cancellable = true, allow = 1)
    public void onThrowPearlIntoRegion(HitResult hitResult, CallbackInfo ci) {
        ThrownEnderpearl pearl = (ThrownEnderpearl) (Object) this;
        if (isServerSide(pearl.level())) {
            Entity owner = pearl.getOwner();
            if (owner instanceof Player player) {
                BlockPos targetBlockPos = new BlockPos(pearl.getBlockX(), pearl.getBlockY(), pearl.getBlockZ());
                FlagCheckRequest checkEvent = new FlagCheckRequest(targetBlockPos, FlagRegister.PLAYER_USE_ENDERPEARL, getDimKey(player), player);
                if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent))
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
                FlagCheckRequest checkEvent = new FlagCheckRequest(player.blockPosition(), FlagRegister.PLAYER_ENDERPEARL_AWAY, getDimKey(player), player);
                if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent))
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
