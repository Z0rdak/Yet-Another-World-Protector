package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.handler.HandlerUtil;
import de.z0rdak.yawp.platform.Services;
import de.z0rdak.yawp.util.text.MessageSender;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.BlockGetter;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.LevelAccessor;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.material.*;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.core.flag.RegionFlag.*;

@Mixin(FlowingFluid.class)
public class FlowingFluidMixin {

    @Inject(method = "spreadTo", at = @At("HEAD"), cancellable = true)
    protected void canSpreadTo(LevelAccessor levelAccessor, BlockPos pos, BlockState blockState, Direction direction, FluidState fluidState, CallbackInfo ci) {
        if (!(levelAccessor instanceof Level level)) {
            // Should never happen, but skip check if it does
            return;
        }
        FlagCheckEvent checkEvent = new FlagCheckEvent(pos, FLUID_FLOW, level.dimension());
        if (Services.EVENT.post(checkEvent)) {
            return;
        }
        HandlerUtil.processCheck(checkEvent, deny -> {
            ci.cancel();
        });
        if (ci.isCancelled()) {
            return;
        }
        
       
        FlagCheckEvent specificFluidCheckEvent = null;
        if ( fluidState.getType() instanceof WaterFluid) {
            specificFluidCheckEvent = new FlagCheckEvent(pos, WATER_FLOW, level.dimension());
        } else if ( fluidState.getType() instanceof LavaFluid) {
            specificFluidCheckEvent = new FlagCheckEvent(pos, LAVA_FLOW, level.dimension());
        }
        
        if (specificFluidCheckEvent == null || Services.EVENT.post(specificFluidCheckEvent)) {
            return;
        }
        HandlerUtil.processCheck(specificFluidCheckEvent, deny -> {
            ci.cancel();
        });
    }

}
