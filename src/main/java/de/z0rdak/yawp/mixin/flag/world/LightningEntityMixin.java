package de.z0rdak.yawp.mixin.flag.world;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import net.minecraft.entity.LightningEntity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.LocalCapture;

import static de.z0rdak.yawp.api.events.region.RegionEvents.post;
import static de.z0rdak.yawp.core.flag.RegionFlag.FALL_DAMAGE;
import static de.z0rdak.yawp.core.flag.RegionFlag.LIGHTNING_PROT;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;

@Mixin(LightningEntity.class)
public abstract class LightningEntityMixin {

    /**
     * lightning-oxidation-removal
     * Prevent oxidation removal from the block when lightning hits
     * It also prevents oxidation of nearby blocks, even when they are not protected
     */
    @Inject(method = "cleanOxidation", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/World;setBlockState(Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/BlockState;)Z"), cancellable = true, allow = 1)
    private static void cleanOxidationOnHitBlock(World world, BlockPos pos, CallbackInfo ci) {
        if (isServerSide(world)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(pos, LIGHTNING_PROT, getDimKey(world), null);
            if (post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, null, deny -> {
                ci.cancel();
            });
        }
    }

    /**
     * lighting-fire-spread
     * Prevent fire spreading when lightning hits blocks
     */
    @Inject(method = "spawnFire", locals = LocalCapture.CAPTURE_FAILSOFT, at = @At(value = "INVOKE", target = "Lnet/minecraft/block/AbstractFireBlock;getState(Lnet/minecraft/world/BlockView;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/BlockState;", ordinal = 0), cancellable = true, allow = 1)
    public void onSpawnFireFromLightning(int spreadAttempts, CallbackInfo ci, BlockPos blockPos) {
        LightningEntity lightningEntity = (LightningEntity) (Object) this;
        if (isServerSide(lightningEntity)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(blockPos, LIGHTNING_PROT, getDimKey(lightningEntity), null);
            if (post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, null, deny -> {
                ci.cancel();
            });
        }
    }
}
