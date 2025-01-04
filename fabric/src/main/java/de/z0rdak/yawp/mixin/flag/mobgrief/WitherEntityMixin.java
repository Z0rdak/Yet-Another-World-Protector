package de.z0rdak.yawp.mixin.flag.mobgrief;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.boss.wither.WitherBoss;
import net.minecraft.world.level.block.state.BlockState;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.LocalCapture;

import static de.z0rdak.yawp.core.flag.RegionFlag.MOB_GRIEFING;
import static de.z0rdak.yawp.core.flag.RegionFlag.WITHER_BLOCK_PROT;
import static de.z0rdak.yawp.handler.HandlerUtil.*;

@Mixin(WitherBoss.class)
public abstract class WitherEntityMixin {

    @Inject(method = "customServerAiStep", locals = LocalCapture.CAPTURE_FAILSOFT, at = @At(value = "INVOKE",
            target = "Lnet/minecraft/world/entity/boss/wither/WitherBoss;canDestroy(Lnet/minecraft/world/level/block/state/BlockState;)Z"), cancellable = true, allow = 1)
    public void onWitherDestroyBlocks(CallbackInfo ci, int j1, int i2, int j2, boolean flag, int l2, int l, int i1, int l1, int i, int j, BlockPos blockPos, BlockState blockState) {
        WitherBoss self = (WitherBoss) (Object) this;
        if (isServerSide(self)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(blockPos, WITHER_BLOCK_PROT, getDimKey(self));
            if (Services.EVENT.post(checkEvent))
                return;
            processCheck(checkEvent, deny -> ci.cancel());
        }
    }

    @Inject(method = "customServerAiStep", at = @At(value = "INVOKE", target = "Lnet/minecraft/util/Mth;floor(D)I", ordinal = 0), cancellable = true, allow = 1)
    public void onWitherAttemptGriefing(CallbackInfo ci) {
        WitherBoss self = (WitherBoss) (Object) this;
        if (isServerSide(self)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(self.blockPosition(), MOB_GRIEFING, getDimKey(self));
            if (Services.EVENT.post(checkEvent))
                return;
            processCheck(checkEvent, deny -> ci.cancel());
        }
    }
}
