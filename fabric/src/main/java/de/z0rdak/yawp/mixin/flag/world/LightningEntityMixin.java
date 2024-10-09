package de.z0rdak.yawp.mixin.flag.world;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.LightningBolt;
import net.minecraft.world.level.Level;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.LocalCapture;

import static de.z0rdak.yawp.api.events.region.FabricRegionEvents.post;
import static de.z0rdak.yawp.core.flag.RegionFlag.LIGHTNING_PROT;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;

@Mixin(LightningBolt.class)
public abstract class LightningEntityMixin {

    /**
     * lightning-oxidation-removal
     * Prevent oxidation removal from the block when lightning hits
     * It also prevents oxidation of nearby blocks, even when they are not protected
     */
    @Inject(method = "clearCopperOnLightningStrike", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/level/Level;setBlockAndUpdate(Lnet/minecraft/core/BlockPos;Lnet/minecraft/world/level/block/state/BlockState;)Z"), cancellable = true, allow = 1)
    private static void cleanOxidationOnHitBlock(Level world, BlockPos pos, CallbackInfo ci) {
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
    @Inject(method = "spawnFire", locals = LocalCapture.CAPTURE_FAILSOFT, at = @At(value = "INVOKE", target = "Lnet/minecraft/world/level/block/BaseFireBlock;getState(Lnet/minecraft/world/level/BlockGetter;Lnet/minecraft/core/BlockPos;)Lnet/minecraft/world/level/block/state/BlockState;", ordinal = 0), cancellable = true, allow = 1)
    public void onSpawnFireFromLightning(int spreadAttempts, CallbackInfo ci, BlockPos blockPos) {
        LightningBolt lightningEntity = (LightningBolt) (Object) this;
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
