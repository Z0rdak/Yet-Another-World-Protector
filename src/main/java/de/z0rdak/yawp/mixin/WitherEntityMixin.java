package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.api.events.region.RegionEvents;
import de.z0rdak.yawp.handler.flags.HandlerUtil;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.MessageSender;
import net.minecraft.entity.boss.WitherEntity;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import static de.z0rdak.yawp.api.events.region.RegionEvents.post;
import static de.z0rdak.yawp.core.flag.RegionFlag.*;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.getEntityDim;

@Mixin(WitherEntity.class)
public abstract class WitherEntityMixin {
    @Inject(method = "mobTick", at = @At(value = "INVOKE", target = "Lnet/minecraft/entity/boss/WitherEntity;canDestroy(Lnet/minecraft/block/BlockState;)Z"), cancellable = true, allow = 1)
    public void onWitherDestroyBlocks(CallbackInfo ci) {
        WitherEntity self = (WitherEntity) (Object) this;
        if (!self.getWorld().isClient) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(self.getBlockPos(), WITHER_BLOCK_PROT, getEntityDim(self), null);
            if (post(checkEvent)) {
                return;
            }
            HandlerUtil.processCheck(checkEvent, null, deny -> {
                ci.cancel();
            });

            checkEvent = new FlagCheckEvent(self.getBlockPos(), MOB_GRIEFING, getEntityDim(self), null);
            if (post(checkEvent)) {
                return;
            }
            HandlerUtil.processCheck(checkEvent, null, deny -> {
                ci.cancel();
            });
        }
    }
}
