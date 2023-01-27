package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.util.RandomSource;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.FireBlock;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;



@Mixin(FireBlock.class)
public abstract class FireBlockMixin {

    @Inject(method = "tryCatchFire", at = @At(value = "HEAD"), cancellable = true, remap = false)
    private void spread(Level world, BlockPos pos, int spreadFactor, RandomSource rand, int currentAge, Direction dir, CallbackInfo info) {
        if (!world.isClientSide) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(world.dimension());
            /*
            FlagCheckEvent flagCheckEvent = HandlerUtil.checkTargetEvent(pos, FIRE_SPREAD, dimCache.getDimensionalRegion());
            if (flagCheckEvent.isDenied()) {
                info.cancel();
            }

             */
        }
    }
}