package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.handler.flags.FlagCheckEvent;
import de.z0rdak.yawp.handler.flags.HandlerUtil;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.block.FireBlock;
import net.minecraft.util.Direction;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import java.util.Random;

import static de.z0rdak.yawp.core.flag.RegionFlag.FIRE_TICK;

@Mixin(FireBlock.class)
public abstract class FireBlockMixin {

    @Inject(method = "tryCatchFire", at = @At(value = "HEAD"), cancellable = true, remap = false)
    private void spread(World world, BlockPos pos, int spreadFactor, Random rand, int currentAge, Direction dir, CallbackInfo info) {
        if (!world.isClientSide) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(world.dimension());
            FlagCheckEvent flagCheckEvent = HandlerUtil.checkTargetEvent(pos, FIRE_TICK, dimCache.getDimensionalRegion());
            if (flagCheckEvent.isDenied()) {
                info.cancel();
            }
        }
    }
}