package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.handler.flags.FlagCheckEvent;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.entity.passive.TameableEntity;
import net.minecraft.entity.player.PlayerEntity;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import static de.z0rdak.yawp.core.flag.RegionFlag.ANIMAL_TAMING;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;

@Mixin(TameableEntity.class)
public abstract class TameableEntityMixin {
    @Inject(method = "setOwner", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onAnimalTame(PlayerEntity player, CallbackInfo ci) {
        TameableEntity self = (TameableEntity) (Object) this;
        if (!self.world.isClient) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(self));
            FlagCheckEvent.PlayerFlagEvent flagCheck = checkPlayerEvent(player, self.getBlockPos(), ANIMAL_TAMING, dimCache.getDimensionalRegion());
            if (flagCheck.isDenied()) {
                sendFlagDeniedMsg(flagCheck);
                ci.cancel();
            }
        }
    }
}
