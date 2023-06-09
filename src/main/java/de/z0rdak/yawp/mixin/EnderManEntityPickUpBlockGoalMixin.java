package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.handler.flags.FlagCheckEvent;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.entity.mob.EndermanEntity;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;


import static de.z0rdak.yawp.core.flag.RegionFlag.ENDERMAN_GRIEFING;
import static de.z0rdak.yawp.core.flag.RegionFlag.MOB_GRIEFING;

import static de.z0rdak.yawp.handler.flags.HandlerUtil.checkTargetEvent;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.getEntityDim;

@Mixin(targets = "net.minecraft.entity.mob.EndermanEntity$PickUpBlockGoal")
public abstract class EnderManEntityPickUpBlockGoalMixin {
    @Shadow private EndermanEntity enderman;

    @Inject(method = "canStart()Z", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onCanStart(CallbackInfoReturnable<Boolean> cir) {
        EndermanEntity self = enderman;
        DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(self));
        DimensionalRegion dimRegion = dimCache.getDimensionalRegion();

      FlagCheckEvent flagCheck = checkTargetEvent(self.getBlockPos(), ENDERMAN_GRIEFING, dimRegion);
      if (flagCheck.isDenied()) {
          cir.setReturnValue(false);
      }

        flagCheck = checkTargetEvent(self.getBlockPos(), MOB_GRIEFING, dimRegion);
        if (flagCheck.isDenied()) {
            cir.setReturnValue(false);
        }
    }
}
