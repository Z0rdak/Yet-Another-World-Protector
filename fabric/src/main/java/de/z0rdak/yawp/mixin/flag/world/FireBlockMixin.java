package de.z0rdak.yawp.mixin.flag.world;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import net.minecraft.core.BlockPos;
import net.minecraft.util.RandomSource;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.BaseFireBlock;
import net.minecraft.world.level.block.state.BlockState;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import static de.z0rdak.yawp.api.events.region.FabricRegionEvents.post;
import static de.z0rdak.yawp.core.flag.RegionFlag.FIRE_TICK;
import static de.z0rdak.yawp.handler.HandlerUtil.*;

@Mixin(BaseFireBlock.class)
public abstract class FireBlockMixin {

    @Inject(method = "animateTick", at = @At(value = "HEAD"), cancellable = true)
    private void spread(BlockState blockState, Level level, BlockPos blockPos, RandomSource randomSource, CallbackInfo ci) {
        if (isServerSide(level)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(blockPos, FIRE_TICK, getDimKey(level), null);
            if (post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, null, deny -> {
                ci.cancel();
            });
        }
    }
}