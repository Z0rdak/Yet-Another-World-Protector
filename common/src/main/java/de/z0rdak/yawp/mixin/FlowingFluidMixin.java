package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.api.FlagEvaluator;
import de.z0rdak.yawp.api.FlagRegister;
import de.z0rdak.yawp.api.events.flag.FlagCheckRequest;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.LevelAccessor;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.material.*;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(FlowingFluid.class)
public class FlowingFluidMixin {

    @Inject(method = "spreadTo", at = @At("HEAD"), cancellable = true)
    protected void canSpreadTo(LevelAccessor levelAccessor, BlockPos pos, BlockState blockState, Direction direction, FluidState fluidState, CallbackInfo ci) {
        if (!(levelAccessor instanceof Level level)) {
            // Should never happen, but skip check if it does
            return;
        }
        if (!Services.FLAG_CONFIG.isDisabledByConfig(FlagRegister.FLUID_FLOW.name())) {
            FlagCheckRequest checkEvent = new FlagCheckRequest(pos, FlagRegister.FLUID_FLOW, level.dimension());
            if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent)) {
                return;
            }
            FlagEvaluator.processCheck(checkEvent, deny -> {
                ci.cancel();
            });
            if (ci.isCancelled()) {
                return;
            }
        }

        if (!Services.FLAG_CONFIG.isDisabledByConfig(FlagRegister.WATER_FLOW.name())) {
            if ( fluidState.getType() instanceof WaterFluid) {
                FlagCheckRequest specificFluidCheckEvent = new FlagCheckRequest(pos, FlagRegister.WATER_FLOW, level.dimension());
                if (Services.FLAG_EVENT_DISPATCHER.post(specificFluidCheckEvent)) {
                    return;
                }
                FlagEvaluator.processCheck(specificFluidCheckEvent, deny -> {
                    ci.cancel();
                });
            }
        }

        if (!Services.FLAG_CONFIG.isDisabledByConfig(FlagRegister.LAVA_FLOW.name())) {
            if ( fluidState.getType() instanceof LavaFluid) {
                FlagCheckRequest specificFluidCheckEvent = new FlagCheckRequest(pos, FlagRegister.LAVA_FLOW, level.dimension());
                if (Services.FLAG_EVENT_DISPATCHER.post(specificFluidCheckEvent)) {
                    return;
                }
                FlagEvaluator.processCheck(specificFluidCheckEvent, deny -> {
                    ci.cancel();
                });
            }
        }
    }
}
