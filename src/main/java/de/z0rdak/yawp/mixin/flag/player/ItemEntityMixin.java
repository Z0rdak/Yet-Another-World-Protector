package de.z0rdak.yawp.mixin.flag.player;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import net.minecraft.entity.ItemEntity;
import net.minecraft.entity.player.PlayerEntity;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import static de.z0rdak.yawp.api.events.region.RegionEvents.post;
import static de.z0rdak.yawp.core.flag.RegionFlag.ITEM_PICKUP;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;
import static de.z0rdak.yawp.util.MessageSender.sendFlagMsg;

@Mixin(ItemEntity.class)
public abstract class ItemEntityMixin {

    @Inject(method = "onPlayerCollision", at = @At(value = "INVOKE", target = "Lnet/minecraft/item/ItemStack;getCount()I"), cancellable = true, allow = 1)
    public void onPickUpItem(PlayerEntity player, CallbackInfo ci) {
        ItemEntity itemToPickup = (ItemEntity) (Object) this;
        if (isServerSide(itemToPickup.getWorld())) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(itemToPickup.getBlockPos(), ITEM_PICKUP, getDimKey(player), player);
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
