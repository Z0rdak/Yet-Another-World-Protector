package de.z0rdak.yawp.mixin.flag.mobgrief;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.ai.behavior.HarvestFarmland;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.core.flag.RegionFlag.MOB_GRIEFING;
import static de.z0rdak.yawp.api.FlagEvaluator.processCheck;

@Mixin(HarvestFarmland.class)
public class HarvestFarmlandMixin {

    /**
     * Check if the provided BlockPos is a suitable target for the farmer villager to harvest.
     * Tells the farmer villager ai that the given block is not suitable for farming, preventing the mob griefing in the progress
     * (considering the flag state)
     * This way of handling the position in this particular case is way better than forge does with its mobgriefing event.
     */
    @Inject(method = "validPos", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    void isBlockSuitableTarget(BlockPos pos, ServerLevel world, CallbackInfoReturnable<Boolean> cir) {
        FlagCheckEvent checkEvent = new FlagCheckEvent(pos, MOB_GRIEFING, world.dimension(), null);
        if (Services.EVENT.post(checkEvent))
            return;
        processCheck(checkEvent, deny -> {
            cir.setReturnValue(false);
        });
    }
}
