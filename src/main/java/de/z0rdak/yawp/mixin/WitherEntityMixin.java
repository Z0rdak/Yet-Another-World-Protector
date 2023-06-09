package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.handler.flags.FlagCheckEvent;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.entity.boss.WitherEntity;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import static de.z0rdak.yawp.core.flag.RegionFlag.WITHER_BLOCK_PROT;
import static de.z0rdak.yawp.core.flag.RegionFlag.MOB_GRIEFING;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.checkTargetEvent;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.getEntityDim;

@Mixin(WitherEntity.class)
public abstract class WitherEntityMixin {
    @Inject(method = "mobTick", at = @At(value = "INVOKE", target = "Lnet/minecraft/entity/boss/WitherEntity;canDestroy(Lnet/minecraft/block/BlockState;)Z"), cancellable = true, allow = 1)
    public void onWitherDestroyBlocks(CallbackInfo ci) {
        WitherEntity self = (WitherEntity) (Object) this;
        if (!self.world.isClient) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(self));
            FlagCheckEvent flagCheck = checkTargetEvent(self.getBlockPos(), WITHER_BLOCK_PROT, dimCache.getDimensionalRegion());
            if (flagCheck.isDenied()) {
                ci.cancel();
            }
            // This makes MOB_GRIEFING a bit stronger than the gamerule DO_MOB_GRIEFING which takes away
            // less of the Wither's capabilities.
            flagCheck = checkTargetEvent(self.getBlockPos(), MOB_GRIEFING, dimCache.getDimensionalRegion());
            if (flagCheck.isDenied()) {
                ci.cancel();
            }
        }
    }
}
