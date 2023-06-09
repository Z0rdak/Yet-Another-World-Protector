package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.handler.flags.FlagCheckEvent;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.entity.mob.ShulkerEntity;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.core.flag.RegionFlag.SHULKER_TELEPORT_FROM_REGION;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.checkTargetEvent;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.getEntityDim;

@Mixin(ShulkerEntity.class)
public abstract class ShulkerEntityMixin {
    @Inject(method = "tryTeleport", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onShulkerTeleport(CallbackInfoReturnable<Boolean> cir) {
        ShulkerEntity self = (ShulkerEntity) (Object) this;
        if (!self.getWorld().isClient) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(self));
            FlagCheckEvent flagCheck = checkTargetEvent(self.getBlockPos(), SHULKER_TELEPORT_FROM_REGION, dimCache.getDimensionalRegion());
            if (flagCheck.isDenied()) {
                cir.setReturnValue(false);
            }
        }
    }
}
