package de.z0rdak.yawp.mixin.flag;

import de.z0rdak.yawp.api.FlagEvaluator;
import de.z0rdak.yawp.api.events.flag.FlagCheckRequest;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.BaseFireBlock;
import net.minecraft.world.level.block.state.BlockState;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import de.z0rdak.yawp.api.FlagRegister;
import static de.z0rdak.yawp.handler.HandlerUtil.*;

@Mixin(BaseFireBlock.class)
public abstract class AbstractFireBlockMixin {

    @Inject(method = "onPlace", at = @At(value = "INVOKE",
            target = "Lnet/minecraft/world/level/portal/PortalShape;createPortalBlocks(Lnet/minecraft/world/level/LevelAccessor;)V"), cancellable = true)
    private void onSpawnPortal(BlockState state, Level world, BlockPos pos, BlockState oldState, boolean notify, CallbackInfo info) {
        if (isServerSide(world)) {
            FlagCheckRequest checkEvent = new FlagCheckRequest(pos, FlagRegister.CREATE_PORTAL, getDimKey(world));
            if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent)) {
                return;
            }
            FlagEvaluator.processCheck(checkEvent, deny -> info.cancel());
        }
    }
}