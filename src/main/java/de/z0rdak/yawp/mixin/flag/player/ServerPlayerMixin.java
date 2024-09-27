package de.z0rdak.yawp.mixin.flag.player;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.world.entity.Entity;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.player.Player;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.api.events.region.RegionEvents.post;
import static de.z0rdak.yawp.core.flag.RegionFlag.*;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;
import static de.z0rdak.yawp.util.text.MessageSender.sendFlagMsg;

@Mixin(ServerPlayer.class)
public abstract class ServerPlayerMixin {

    // This is preferred to forge ItemTossEvent, because the forge event does delete the stack
    @Inject(method = "drop(Z)Z", at = @At(value = "TAIL"), allow = 1, cancellable = true)
    private void onDropItem(boolean entireStack, CallbackInfoReturnable<Boolean> cir) {
        ServerPlayer player = (ServerPlayer) (Object) this;
        if (isServerSide(player)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(player.blockPosition(), ITEM_DROP, getDimKey(player), player);
            if (post(checkEvent))
                return;
            processCheck(checkEvent, null, deny -> {
                sendFlagMsg(deny);
                cir.setReturnValue(false);
            });
        }
    }

    /**
     * TODO: Fix ENTER_DIM for local regions
     */
    @Inject(method = "changeDimension", at = @At(value = "HEAD"), allow = 1, cancellable = true)
    private void onChangeDimension(ServerLevel destination, CallbackInfoReturnable<Entity> cir) {
        Player player = (Player) (Object) this;
        if (isServerSide(player)) {
            RegionDataManager.onPlayerChangeWorldAddDimKey(player, (ServerLevel) player.level(), destination);

            FlagCheckEvent checkEvent = new FlagCheckEvent(player.blockPosition(), USE_PORTAL_PLAYERS, getDimKey(player), player);
            if (post(checkEvent))
                return;
            processCheck(checkEvent, null, deny -> {
                sendFlagMsg(deny);
                cir.setReturnValue(null);
            });

            checkEvent = new FlagCheckEvent(player.blockPosition(), ENTER_DIM, getDimKey(destination), player);
            if (post(checkEvent))
                return;
            processCheck(checkEvent, null, deny -> {
                sendFlagMsg(deny);
                cir.setReturnValue(null);
            });
        }
    }

    @Inject(method = "teleportTo(Lnet/minecraft/server/level/ServerLevel;DDDFF)V", at = @At(value = "INVOKE", 
            target = "Lnet/minecraft/server/level/ServerPlayer;level()Lnet/minecraft/world/level/Level;"), allow = 1, cancellable = true)
    private void onTeleportToDimension(ServerLevel destination, double x, double y, double z, float yaw, float pitch, CallbackInfo ci) {
        Player player = (Player) (Object) this;
        if (isServerSide(player)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(player.blockPosition(), USE_PORTAL_PLAYERS, player.level().dimension(), player);
            if (post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, null, deny -> {
                sendFlagMsg(deny);
                ci.cancel();
            });
        }
    }
}
