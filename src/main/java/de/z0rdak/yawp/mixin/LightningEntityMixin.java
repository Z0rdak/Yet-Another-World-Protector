package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.handler.flags.HandlerUtil;
import net.minecraft.entity.effect.LightningBoltEntity;
import net.minecraft.util.math.BlockPos;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.LocalCapture;

import static de.z0rdak.yawp.api.events.region.RegionEvents.post;
import static de.z0rdak.yawp.core.flag.RegionFlag.LIGHTNING_PROT;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.isServerSide;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.processCheck;

@Mixin(LightningBoltEntity.class)
public abstract class LightningEntityMixin {

    /**
     * lighting-fire-spread
     * Prevent fire spreading when lightning hits blocks
     */
    @Inject(method = "spawnFire", locals = LocalCapture.CAPTURE_FAILSOFT, at = @At(value = "INVOKE", target = "Lnet/minecraft/block/AbstractFireBlock;getState(Lnet/minecraft/world/IBlockReader;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/BlockState;", ordinal = 0), cancellable = true, allow = 1)
    public void onSpawnFireFromLightning(int spreadAttempts, CallbackInfo ci, BlockPos pos) {
        LightningBoltEntity lightningEntity = (LightningBoltEntity) (Object) this;
        if (isServerSide(lightningEntity)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(pos, LIGHTNING_PROT, HandlerUtil.getDimKey(lightningEntity));
            if (post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, deny -> {
                ci.cancel();
            });
        }
    }
}
