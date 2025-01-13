package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.handler.HandlerUtil;
import de.z0rdak.yawp.platform.Services;
import de.z0rdak.yawp.util.text.MessageSender;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.level.BlockGetter;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.material.*;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.core.flag.RegionFlag.*;

@Mixin(FlowingFluid.class)
public class FlowingFluidMixin {

    @Inject(method = "canSpreadTo", at = @At("HEAD"), cancellable = true)
    protected void canSpreadTo(BlockGetter blockGetter, BlockPos blockPos, BlockState blockState, Direction direction, BlockPos blockPos2, BlockState blockState2, FluidState fluidState, Fluid fluid, CallbackInfoReturnable<Boolean> cir) {
        if (!(blockGetter instanceof Level level)) {
            // Should never happen, but skip check if it does
            return;
        }

        FlagCheckEvent checkEvent = new FlagCheckEvent(blockPos, FLUID_FLOW, level.dimension());
        HandlerUtil.processCheck(checkEvent, deny -> {
            cir.setReturnValue(false);
        });

        FlagCheckEvent specificFluidCheckEvent = null;
        if (fluid instanceof WaterFluid) {
            specificFluidCheckEvent = new FlagCheckEvent(blockPos, WATER_FLOW, level.dimension());
        } else if (fluid instanceof LavaFluid) {
            specificFluidCheckEvent = new FlagCheckEvent(blockPos, LAVA_FLOW, level.dimension());
        }

        if (cir.isCancelled() || specificFluidCheckEvent == null) {
            return;
        }

        HandlerUtil.processCheck(specificFluidCheckEvent, deny -> {
            cir.setReturnValue(false);
        });
    }

}
