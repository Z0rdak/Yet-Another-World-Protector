package de.z0rdak.yawp.mixin.flag.player;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.entity.player.Player;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import static de.z0rdak.yawp.api.events.region.RegionEvents.post;
import static de.z0rdak.yawp.core.flag.RegionFlag.ITEM_PICKUP;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;
import static de.z0rdak.yawp.util.text.MessageSender.sendFlagMsg;

@Mixin(ItemEntity.class)
public abstract class ItemEntityMixin {

    @Inject(method = "playerTouch", at = @At(value = "INVOKE", 
            target = "Lnet/minecraft/world/item/ItemStack;getCount()I"), cancellable = true, allow = 1)
    public void onPickUpItem(Player player, CallbackInfo ci) {
        ItemEntity itemToPickup = (ItemEntity) (Object) this;
        if (isServerSide(itemToPickup.level())) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(itemToPickup.blockPosition(), ITEM_PICKUP, getDimKey(player), player);
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
