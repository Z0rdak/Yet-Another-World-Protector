package de.z0rdak.yawp.mixin.flag.mobgrief;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import net.minecraft.world.entity.monster.EnderMan;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.api.events.region.FabricRegionEvents.post;
import static de.z0rdak.yawp.core.flag.RegionFlag.ENDERMAN_GRIEFING;
import static de.z0rdak.yawp.core.flag.RegionFlag.MOB_GRIEFING;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.getDimKey;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.processCheck;

@Mixin(targets = "net.minecraft.world.entity.monster.EnderMan$EndermanLeaveBlockGoal")
public abstract class EndermanLeaveBlockGoalMixin {
    @Final
    @Shadow
    private EnderMan enderman;

    @Inject(method = "canUse", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onCanStart(CallbackInfoReturnable<Boolean> cir) {
        EnderMan self = enderman;
        FlagCheckEvent checkEvent = new FlagCheckEvent(self.blockPosition(), ENDERMAN_GRIEFING, getDimKey(self), null);
        if (post(checkEvent)) {
            return;
        }
        processCheck(checkEvent, null, deny -> {
            cir.setReturnValue(false);
        });

        checkEvent = new FlagCheckEvent(self.blockPosition(), MOB_GRIEFING, getDimKey(self), null);
        if (post(checkEvent)) {
            return;
        }
        processCheck(checkEvent, null, deny -> {
            cir.setReturnValue(false);
        });
    }
}
