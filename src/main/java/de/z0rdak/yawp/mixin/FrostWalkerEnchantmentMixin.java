package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.handler.flags.FlagCheckEvent;
import de.z0rdak.yawp.handler.flags.HandlerUtil;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.enchantment.FrostWalkerEnchantment;
import net.minecraft.entity.LivingEntity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import static de.z0rdak.yawp.core.flag.RegionFlag.NO_WALKER_FREEZE;

@Mixin(FrostWalkerEnchantment.class)
public class FrostWalkerEnchantmentMixin {

    @Inject(method = "onEntityMoved", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/World;setBlockAndUpdate(Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/BlockState;)Z"), cancellable = true)
    private static void onEntityMoved(LivingEntity entity, World world, BlockPos pos, int p_45022_, CallbackInfo info) {
        if (!world.isClientSide) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(world.dimension());
            FlagCheckEvent flagCheckEvent = HandlerUtil.checkTargetEvent(pos, NO_WALKER_FREEZE, dimCache.getDimensionalRegion());
            if (flagCheckEvent.isDenied()) {
                info.cancel();
            }
        }
    }

}
