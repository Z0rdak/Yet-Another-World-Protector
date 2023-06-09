package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.handler.flags.FlagCheckEvent;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.entity.boss.dragon.EnderDragonEntity;
import net.minecraft.util.math.Box;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.core.flag.RegionFlag.DRAGON_BLOCK_PROT;
import static de.z0rdak.yawp.core.flag.RegionFlag.MOB_GRIEFING;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.checkTargetEvent;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.getEntityDim;

@Mixin(EnderDragonEntity.class)
public abstract class EnderDragonEntityMixin {
    @Inject(method = "destroyBlocks", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/World;getGameRules()Lnet/minecraft/world/GameRules;"), cancellable = true, allow = 1)
    public void onDragonDestroyBlocks(Box box, CallbackInfoReturnable<Boolean> cir) {
        EnderDragonEntity self = (EnderDragonEntity) (Object) this;
        DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(self));
        FlagCheckEvent flagCheck = checkTargetEvent(self.getBlockPos(), DRAGON_BLOCK_PROT, dimCache.getDimensionalRegion());
        if (flagCheck.isDenied()) {
            cir.setReturnValue(false);
        }
        flagCheck = checkTargetEvent(self.getBlockPos(), MOB_GRIEFING, dimCache.getDimensionalRegion());
        if (flagCheck.isDenied()) {
            cir.setReturnValue(false);
        }
    }
}
