package de.z0rdak.yawp.mixin.flag;

import de.z0rdak.yawp.api.FlagEvaluator;
import de.z0rdak.yawp.api.events.flag.FlagCheckRequest;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.entity.projectile.FireworkRocketEntity;
import net.minecraft.world.entity.projectile.Projectile;
import net.minecraft.world.phys.EntityHitResult;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import static de.z0rdak.yawp.api.MessageSender.sendFlagMsg;
import static de.z0rdak.yawp.core.flag.RegionFlag.NO_PVP;
import static de.z0rdak.yawp.handler.HandlerUtil.getDimKey;
import static de.z0rdak.yawp.handler.HandlerUtil.isServerSide;

@Mixin(FireworkRocketEntity.class)
public class FireworkRocketEntityMixin {


    @Inject(method = "onHitEntity", at = @At(value = "HEAD"), cancellable = true)
    private void onHitPlayer(EntityHitResult result, CallbackInfo info) {
        if (isServerSide(result.getEntity().level())) {
            Projectile fwr = (Projectile) (Object) this;
            if (fwr.getOwner() instanceof Player shooter && result.getEntity() instanceof Player target) {
                FlagCheckRequest checkEvent = new FlagCheckRequest(target.blockPosition(), NO_PVP, getDimKey(result.getEntity().level()), shooter);
                if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent)) {
                    return;
                }
                FlagEvaluator.processCheck(checkEvent, deny -> {
                    info.cancel();
                    sendFlagMsg(deny);
                });
            }
        }
    }
}
