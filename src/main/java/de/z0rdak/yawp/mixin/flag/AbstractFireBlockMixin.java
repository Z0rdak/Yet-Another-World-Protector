package de.z0rdak.yawp.mixin.flag;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import net.minecraft.block.AbstractFireBlock;
import net.minecraft.block.BlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import static de.z0rdak.yawp.api.events.region.RegionEvents.post;
import static de.z0rdak.yawp.core.flag.RegionFlag.SPAWN_PORTAL;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;

@Mixin(AbstractFireBlock.class)
public abstract class AbstractFireBlockMixin {

    @Inject(method = "onBlockAdded", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/dimension/NetherPortal;createPortal()V"), cancellable = true)
    private void onSpawnPortal(BlockState state, World world, BlockPos pos, BlockState oldState, boolean notify, CallbackInfo info) {
        if (isServerSide(world)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(pos, SPAWN_PORTAL, getDimKey(world), null);
            if (post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, null, deny -> info.cancel());
        }
    }
}