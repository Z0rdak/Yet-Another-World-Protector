package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.block.FireBlock;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import java.util.Random;

@Mixin(FireBlock.class)
public abstract class FireBlockMixin {

    @Inject(method = "trySpreadingFire", at = @At(value = "HEAD"), cancellable = true, remap = false)
    private void spread(World world, BlockPos pos, int spreadFactor, Random rand, int currentAge, CallbackInfo info) {
        if (!world.isClient) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(world.getRegistryKey());
            //FlagCheckEvent flagCheckEvent = HandlerUtil.checkTargetEvent(pos, RegionFlag.DRAGON_BLOCK_PROT, dimCache.getDimensionalRegion());
            // if (flagCheckEvent.isDenied()) {
            info.cancel();
            //}
        }
    }
}