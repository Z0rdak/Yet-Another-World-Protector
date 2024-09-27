package de.z0rdak.yawp.mixin.flag;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;

import net.minecraft.core.BlockPos;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.BaseFireBlock;
import net.minecraft.world.level.block.FireBlock;
import net.minecraft.world.level.block.state.BlockState;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import static de.z0rdak.yawp.api.events.region.RegionEvents.post;
import static de.z0rdak.yawp.core.flag.RegionFlag.SPAWN_PORTAL;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;

@Mixin(BaseFireBlock.class)
public abstract class AbstractFireBlockMixin {

    @Inject(method = "onPlace", at = @At(value = "INVOKE", 
            target = "Lnet/minecraft/world/level/portal/PortalShape;createPortalBlocks()V"), cancellable = true)
    private void onSpawnPortal(BlockState state, Level world, BlockPos pos, BlockState oldState, boolean notify, CallbackInfo info) {
        if (isServerSide(world)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(pos, SPAWN_PORTAL, getDimKey(world), null);
            if (post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, null, deny -> info.cancel());
        }
    }
}