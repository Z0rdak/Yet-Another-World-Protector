package de.z0rdak.yawp.mixin.flag.mobgrief;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.world.entity.monster.EnderMan;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.core.flag.RegionFlag.ENDERMAN_GRIEFING;
import static de.z0rdak.yawp.core.flag.RegionFlag.MOB_GRIEFING;
import static de.z0rdak.yawp.handler.HandlerUtil.getDimKey;
import static de.z0rdak.yawp.api.FlagEvaluator.processCheck;

@Mixin(targets = "net.minecraft.world.entity.monster.EnderMan$EndermanTakeBlockGoal")
public abstract class EndermanTakeBlockGoalMixin {
    @Final
    @Shadow
    private EnderMan enderman;

    @Inject(method = "canUse", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onCanStart(CallbackInfoReturnable<Boolean> cir) {
        EnderMan self = enderman;
        // Early return for the common case
        if (this.enderman.getCarriedBlock() == null) {
            cir.setReturnValue(false);
        }
        // TODO: Hook into tick method to get the position of the block, not the entity
        FlagCheckEvent checkEvent = new FlagCheckEvent(self.blockPosition(), ENDERMAN_GRIEFING, getDimKey(self));
        if (Services.EVENT.post(checkEvent)) {
            return;
        }
        processCheck(checkEvent, deny -> {
            cir.setReturnValue(false);
        });

        checkEvent = new FlagCheckEvent(self.blockPosition(), MOB_GRIEFING, getDimKey(self));
        if (Services.EVENT.post(checkEvent)) {
            return;
        }
        processCheck(checkEvent, deny -> {
            cir.setReturnValue(false);
        });
    }
}
