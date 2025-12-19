package de.z0rdak.yawp.mixin.flag.player;

import de.z0rdak.yawp.api.FlagEvaluator;
import de.z0rdak.yawp.api.events.flag.FlagCheckRequest;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.platform.Services;
import de.z0rdak.yawp.util.text.TitleBuilder;
import net.minecraft.ChatFormatting;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerLevel;
import de.z0rdak.yawp.util.text.TitleBuilder;
import net.minecraft.ChatFormatting;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.Relative;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.portal.TeleportTransition;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import java.util.Set;

import static de.z0rdak.yawp.core.flag.RegionFlag.*;
import static de.z0rdak.yawp.handler.HandlerUtil.*;
import static de.z0rdak.yawp.api.MessageSender.sendFlagMsg;

@Mixin(ServerPlayer.class)
public abstract class ServerPlayerMixin {

    @Shadow public abstract ServerLevel serverLevel();

    // This is preferred to forge ItemTossEvent, because the forge event does delete the stack
    @Inject(method = "drop(Lnet/minecraft/world/item/ItemStack;ZZ)Lnet/minecraft/world/entity/item/ItemEntity;", at = @At(value = "HEAD"), allow = 1, cancellable = true)
    private void onDropItem(ItemStack stack, boolean b1, boolean b2, CallbackInfoReturnable<ItemEntity> cir) {
        ServerPlayer player = (ServerPlayer) (Object) this;
        if (isServerSide(player)) {
            FlagCheckRequest checkEvent = new FlagCheckRequest(player.blockPosition(), ITEM_DROP, getDimKey(player), player);
            if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent))
                return;
            FlagEvaluator.processCheck(checkEvent, deny -> {
                sendFlagMsg(deny);
                player.addItem(stack);
                player.getInventory().setChanged();
                cir.setReturnValue(null);
            });
        }
    }

    @Inject(method = "teleport(Lnet/minecraft/world/level/portal/TeleportTransition;)Lnet/minecraft/server/level/ServerPlayer;", at = @At(value = "HEAD"), allow = 1, cancellable = true)
    private void onChangeDimension(TeleportTransition teleportTransition, CallbackInfoReturnable<Entity> cir) {
        Player player = (Player) (Object) this;
        if (isServerSide(player)) {
            RegionDataManager.initLevelDataOnChangeWorld(player, player.level(), transition.newLevel());

            FlagCheckRequest checkEvent = new FlagCheckRequest(player.blockPosition(), USE_PORTAL_PLAYERS, getDimKey(player), player);
            if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent))
                return;
            FlagEvaluator.processCheck(checkEvent, deny -> {
                sendFlagMsg(deny);
                cir.setReturnValue(null);
            });

            checkEvent = new FlagCheckRequest(player.blockPosition(), ENTER_DIM, getDimKey(teleportTransition.newLevel()), player);
            if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent))
                return;
            FlagEvaluator.process(checkEvent)
                    .onDenyWithMsg(result -> cir.setReturnValue(null));
        }
    }

    @Inject(method = "teleportTo(Lnet/minecraft/server/level/ServerLevel;DDDLjava/util/Set;FFZ)Z", at = @At(value = "HEAD"), allow = 1, cancellable = true)
    private void onTeleportToDimension(ServerLevel level, double x, double y, double z, Set<Relative> relativeMovements, float yaw, float pitch, boolean setCamera, CallbackInfoReturnable<Boolean> cir) {
        Player player = (Player) (Object) this;
        if (isServerSide(player)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(player.blockPosition(), USE_PORTAL_PLAYERS, player.level().dimension(), player);
            if (Services.EVENT.post(checkEvent)) {
                return;
            }
            FlagEvaluator.processCheck(checkEvent, deny -> {
                sendFlagMsg(deny);
                cir.setReturnValue(false);
            });
        }
    }
}
