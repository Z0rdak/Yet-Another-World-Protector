package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.handler.flags.FlagCheckEvent;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.block.AbstractFireBlock;
import net.minecraft.block.BlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Direction;
import net.minecraft.world.World;
import net.minecraft.world.dimension.AreaHelper;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import java.util.Optional;

import static de.z0rdak.yawp.core.flag.RegionFlag.SPAWN_PORTAL;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.checkTargetEvent;

@Mixin(AbstractFireBlock.class)
public abstract class AbstractFireBlockMixin {

    @Inject(method = "onBlockAdded", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/dimension/AreaHelper;getNewPortal(Lnet/minecraft/world/WorldAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/Direction$Axis;)Ljava/util/Optional;"), cancellable = true)
    private void onSpawnPortal(BlockState state, World world, BlockPos pos, BlockState oldState, boolean notify, CallbackInfo info) {
        if (!world.isClient) {
            Optional<AreaHelper> optional = AreaHelper.getNewPortal(world, pos, Direction.Axis.X);
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(world.getRegistryKey());
            FlagCheckEvent flagCheckEvent = checkTargetEvent(pos, SPAWN_PORTAL, dimCache.getDimensionalRegion());
            if (flagCheckEvent.isDenied()) {
                optional = Optional.empty();
            }
            optional.ifPresent(AreaHelper::createPortal);
            info.cancel();
        }
    }
}