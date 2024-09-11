package de.z0rdak.yawp.mixin.flag.mobgrief;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import net.minecraft.block.BlockState;
import net.minecraft.entity.boss.WitherEntity;
import net.minecraft.util.math.BlockPos;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.LocalCapture;

import static de.z0rdak.yawp.api.events.region.RegionEvents.post;
import static de.z0rdak.yawp.core.flag.RegionFlag.*;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;

@Mixin(WitherEntity.class)
public abstract class WitherEntityMixin {

    @Inject(method = "mobTick", locals = LocalCapture.CAPTURE_FAILSOFT, at = @At(value = "INVOKE", target = "Lnet/minecraft/entity/boss/WitherEntity;canDestroy(Lnet/minecraft/block/BlockState;)Z"), cancellable = true, allow = 1)
    public void onWitherDestroyBlocks(CallbackInfo ci, int j1, int i2, int j2, boolean flag, int l2, int l, int i1, int l1, int i, int j, BlockPos blockPos, BlockState blockState) {
        WitherEntity self = (WitherEntity) (Object) this;
        if (isServerSide(self)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(blockPos, WITHER_BLOCK_PROT, getDimKey(self), null);
            if (post(checkEvent)) 
                return;
            processCheck(checkEvent, null, deny -> ci.cancel());
        }
    }

    @Inject(method = "mobTick", at = @At(value = "INVOKE", target = "Lnet/minecraft/util/math/MathHelper;floor(D)I", ordinal = 0), cancellable = true, allow = 1)
    public void onWitherAttemptGriefing(CallbackInfo ci) {
        WitherEntity self = (WitherEntity) (Object) this;
        if (isServerSide(self)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(self.getBlockPos(), MOB_GRIEFING, getDimKey(self), null);
            if (post(checkEvent))
                return;
            processCheck(checkEvent, null, deny -> ci.cancel());
        }
    }
}
