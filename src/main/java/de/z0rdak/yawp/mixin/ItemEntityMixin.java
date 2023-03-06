package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.handler.flags.FlagCheckEvent;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.entity.ItemEntity;
import net.minecraft.entity.player.PlayerEntity;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;

@Mixin(ItemEntity.class)
public abstract class ItemEntityMixin {

    @Inject(method = "onPlayerCollision", at = @At(value = "INVOKE", target = "Lnet/minecraft/item/ItemStack;getCount()I"), cancellable = true, allow = 1)
    public void onPickUpItem(PlayerEntity player, CallbackInfo ci) {
        ItemEntity itemToPickup = (ItemEntity) (Object) this;
        if (!itemToPickup.world.isClient) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(player));
            FlagCheckEvent.PlayerFlagEvent flagCheck = checkPlayerEvent(player, itemToPickup.getBlockPos(), RegionFlag.ITEM_PICKUP, dimCache.getDimensionalRegion());
            if (flagCheck.isDenied()) {
                sendFlagDeniedMsg(flagCheck);
                ci.cancel();
            }
        }
    }
}
