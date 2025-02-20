package de.z0rdak.yawp.mixin.flag.player;

import de.z0rdak.yawp.api.FlagEvaluator;
import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.data.region.RegionDataManager;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.portal.DimensionTransition;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.core.flag.RegionFlag.*;
import static de.z0rdak.yawp.handler.HandlerUtil.*;
import static de.z0rdak.yawp.api.MessageSender.sendFlagMsg;

@Mixin(ServerPlayer.class)
public abstract class ServerPlayerMixin {

    // This is preferred to forge ItemTossEvent, because the forge event does delete the stack
    @Inject(method = "drop(Lnet/minecraft/world/item/ItemStack;ZZ)Lnet/minecraft/world/entity/item/ItemEntity;", at = @At(value = "HEAD"), allow = 1, cancellable = true)
    private void onDropItem(ItemStack stack, boolean b1, boolean b2, CallbackInfoReturnable<ItemEntity> cir) {
        ServerPlayer player = (ServerPlayer) (Object) this;
        if (isServerSide(player)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(player.blockPosition(), ITEM_DROP, getDimKey(player), player);
            if (Services.EVENT.post(checkEvent))
                return;
            FlagEvaluator.processCheck(checkEvent, deny -> {
                sendFlagMsg(deny);
                player.addItem(stack);
                player.getInventory().setChanged();
                cir.setReturnValue(null);
            });
        }
    }

    @Inject(method = "changeDimension", at = @At(value = "HEAD"), allow = 1, cancellable = true)
    private void onchangeDimension(DimensionTransition transition, CallbackInfoReturnable<Entity> cir) {
        Player player = (Player) (Object) this;
        if (isServerSide(player)) {
            RegionDataManager.addDimKeyOnDimensionChange(player, player.level(), transition.newLevel());

            FlagCheckEvent checkEvent = new FlagCheckEvent(player.blockPosition(), USE_PORTAL_PLAYERS, getDimKey(player), player);
            if (Services.EVENT.post(checkEvent))
                return;
            FlagEvaluator.processCheck(checkEvent, deny -> {
                sendFlagMsg(deny);
                cir.setReturnValue(null);
            });

            checkEvent = new FlagCheckEvent(player.blockPosition(), ENTER_DIM, getDimKey(transition.newLevel()), player);
            if (Services.EVENT.post(checkEvent))
                return;
            FlagEvaluator.processCheck(checkEvent, deny -> {
                sendFlagMsg(deny);
                cir.setReturnValue(null);
            });
        }
    }
}