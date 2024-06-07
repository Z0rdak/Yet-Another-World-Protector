package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.api.events.region.RegionEvents;
import de.z0rdak.yawp.handler.flags.HandlerUtil;
import net.minecraft.block.AbstractFireBlock;
import net.minecraft.block.BlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import static de.z0rdak.yawp.core.flag.RegionFlag.SPAWN_PORTAL;

@Mixin(AbstractFireBlock.class)
public abstract class AbstractFireBlockMixin {

    @Inject(method = "onBlockAdded", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/dimension/NetherPortal;createPortal()V"), cancellable = true)
    private void onSpawnPortal(BlockState state, World world, BlockPos pos, BlockState oldState, boolean notify, CallbackInfo info) {
        if (!world.isClient) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(pos, SPAWN_PORTAL, world.getRegistryKey(), null);
            if (RegionEvents.CHECK_FLAG.invoker().checkFlag(checkEvent)) {
                return;
            }
            HandlerUtil.processCheck(checkEvent, null, deny -> info.cancel());
        }
    }
}