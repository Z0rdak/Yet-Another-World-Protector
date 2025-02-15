package de.z0rdak.yawp.mixin.flag.mobgrief;


import de.z0rdak.yawp.api.FlagEvaluator;
import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.goal.DoorInteractGoal;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.core.flag.RegionFlag.MOB_GRIEFING;
import static de.z0rdak.yawp.core.flag.RegionFlag.ZOMBIE_DOOR_PROT;
import static de.z0rdak.yawp.handler.HandlerUtil.*;


@Mixin(net.minecraft.world.entity.ai.goal.BreakDoorGoal.class)
public abstract class BreakDoorGoal extends DoorInteractGoal {
    public BreakDoorGoal(Mob mob) {
        super(mob);
    }

    @Inject(method = "canUse", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onCanStart(CallbackInfoReturnable<Boolean> cir) {
        if (isServerSide(mob.level())) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(mob.blockPosition(), ZOMBIE_DOOR_PROT, getDimKey(mob));
            if (Services.EVENT.post(checkEvent)) {
                return;
            }
            FlagEvaluator.processCheck(checkEvent, deny -> {
                cir.setReturnValue(false);
            });

            checkEvent = new FlagCheckEvent(mob.blockPosition(), MOB_GRIEFING, getDimKey(mob));
            if (Services.EVENT.post(checkEvent)) {
                return;
            }
            FlagEvaluator.processCheck(checkEvent, deny -> {
                cir.setReturnValue(null);
            });
        }
    }
}
