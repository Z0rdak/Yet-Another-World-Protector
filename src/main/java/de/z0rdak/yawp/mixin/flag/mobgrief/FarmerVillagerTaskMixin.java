package de.z0rdak.yawp.mixin.flag.mobgrief;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import net.minecraft.entity.ai.brain.task.FarmerVillagerTask;
import net.minecraft.server.world.ServerWorld;
import net.minecraft.util.math.BlockPos;

import static de.z0rdak.yawp.api.events.region.RegionEvents.post;
import static de.z0rdak.yawp.core.flag.RegionFlag.MOB_GRIEFING;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.processCheck;

@Mixin(FarmerVillagerTask.class)
public class FarmerVillagerTaskMixin {

    /**
     * Check if the provided BlockPos is a suitable target for the farmer villager to harvest.
     * Tells the farmer villager ai that the given block is not suitable for farming, preventing the mob griefing in the progress
     * (considering the flag state)
     * This way of handling the position in this particular case is way better than forge does with its mobgriefing event.
     */
    @Inject(method = "isSuitableTarget", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    void isBlockSuitableTarget(BlockPos pos, ServerWorld world, CallbackInfoReturnable<Boolean> cir) {
        FlagCheckEvent checkEvent = new FlagCheckEvent(pos, MOB_GRIEFING, world.getRegistryKey(), null);
        if (post(checkEvent))
            return;
        processCheck(checkEvent, null, deny -> {
            cir.setReturnValue(false);
        });
    }
}
