package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.handler.flags.FlagCheckEvent;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.entity.mob.EndermanEntity;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.core.flag.RegionFlag.ENDERMAN_TELEPORT_FROM_REGION;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.checkTargetEvent;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.getEntityDim;

@Mixin(EndermanEntity.class)
public abstract class EnderManEntityMixin {
    @Inject(method = "teleportTo(DDD)Z", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onEndermanTeleport(double x, double y, double z, CallbackInfoReturnable<Boolean> cir) {
        EndermanEntity self = (EndermanEntity) (Object) this;
        if (!self.world.isClient) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(self));
            FlagCheckEvent flagCheck = checkTargetEvent(self.getBlockPos(), ENDERMAN_TELEPORT_FROM_REGION, dimCache.getDimensionalRegion());
            if (flagCheck.isDenied()) {
                cir.setReturnValue(false);
            }
        }
    }
}
