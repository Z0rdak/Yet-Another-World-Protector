package de.z0rdak.yawp.mixin.flag.player;

import de.z0rdak.yawp.api.FlagEvaluator;
import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.entity.player.Player;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import static de.z0rdak.yawp.core.flag.RegionFlag.ITEM_PICKUP;
import static de.z0rdak.yawp.handler.HandlerUtil.*;
import static de.z0rdak.yawp.api.MessageSender.sendFlagMsg;

@Mixin(ItemEntity.class)
public abstract class ItemEntityMixin {

    @Shadow
    private int age;
    
    @Inject(method = "playerTouch", at = @At(value = "INVOKE",
            target = "Lnet/minecraft/world/item/ItemStack;getCount()I"), cancellable = true, allow = 1)
    public void onPickUpItem(Player player, CallbackInfo ci) {
        ItemEntity itemToPickup = (ItemEntity) (Object) this;
        if (isServerSide(itemToPickup.level())) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(itemToPickup.blockPosition(), ITEM_PICKUP, getDimKey(player), player);
            if (Services.EVENT.post(checkEvent)) {
                return;
            }
            FlagEvaluator.processCheck(checkEvent, deny -> {
                sendFlagMsg(deny);
                ci.cancel();
            });
        }
    }


    @Inject(method = "tick", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/entity/item/ItemEntity;discard()V", ordinal = 1), cancellable = true)
    public void onTick(CallbackInfo ci) {
        ItemEntity itemToPickup = (ItemEntity) (Object) this;
        FlagCheckEvent checkEvent = new FlagCheckEvent(itemToPickup.blockPosition(), RegionFlag.NO_ITEM_DESPAWN, getDimKey(itemToPickup.level()), null);
        if (Services.EVENT.post(checkEvent)) {
            return;
        }

        FlagEvaluator.processCheck(checkEvent, deny -> {
            this.age = 6000; // Reset age to avoid flag checking every tick.
            ci.cancel();
        });
    }

}
