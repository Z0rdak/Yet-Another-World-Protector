package de.z0rdak.yawp.mixin.flag;

import de.z0rdak.yawp.api.FlagEvaluator;
import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.animal.golem.IronGolem;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.entity.projectile.Projectile;
import net.minecraft.world.entity.projectile.ThrowableProjectile;
import net.minecraft.world.entity.projectile.throwableitemprojectile.Snowball;
import net.minecraft.world.entity.projectile.throwableitemprojectile.ThrownEgg;
import net.minecraft.world.entity.projectile.throwableitemprojectile.ThrownEnderpearl;
import net.minecraft.world.phys.EntityHitResult;
import net.minecraft.world.phys.HitResult;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.LocalCapture;

import static de.z0rdak.yawp.api.MessageSender.sendFlagMsg;
import static de.z0rdak.yawp.core.flag.RegionFlag.NO_PVP;
import static de.z0rdak.yawp.handler.HandlerUtil.getDimKey;
import static de.z0rdak.yawp.handler.HandlerUtil.isServerSide;

@Mixin(ThrowableProjectile.class)
public class ThrowableProjectileMixin {

    @Inject(method = "tick", locals = LocalCapture.CAPTURE_FAILSOFT, at = @At(value = "INVOKE", target = "Lnet/minecraft/world/phys/HitResult;getType()Lnet/minecraft/world/phys/HitResult$Type;"), cancellable = true)
    private void onTick(CallbackInfo ci, HitResult hitResult) {
        if (hitResult.getType() == HitResult.Type.ENTITY) {
            EntityHitResult entityHitResult = (EntityHitResult)hitResult;
            if (isServerSide(entityHitResult.getEntity().level())) {
                Projectile projectile = (Projectile) (Object) this;
                boolean isTypeOf = projectile instanceof Snowball
                        || projectile instanceof ThrownEgg
                        || projectile instanceof ThrownEnderpearl;
                if (!isTypeOf)
                    return;
                if (projectile.getOwner() instanceof Player shooter && entityHitResult.getEntity() instanceof IronGolem target) {
                    FlagCheckEvent checkEvent = new FlagCheckEvent(target.blockPosition(), NO_PVP, getDimKey(target.level()), shooter);
                    if (Services.EVENT.post(checkEvent)) {
                        return;
                    }
                    FlagEvaluator.processCheck(checkEvent, deny -> {
                        projectile.remove(Entity.RemovalReason.DISCARDED);
                        ci.cancel();
                        sendFlagMsg(deny);
                    });
                }
            }
        }

    }
}
