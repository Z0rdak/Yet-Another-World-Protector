package de.z0rdak.yawp.mixin.flag.mobgrief;


import static de.z0rdak.yawp.api.events.region.RegionEvents.post;
import static de.z0rdak.yawp.core.flag.RegionFlag.*;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import net.minecraft.entity.ai.goal.BreakDoorGoal;
import net.minecraft.entity.ai.goal.DoorInteractGoal;
import net.minecraft.entity.mob.MobEntity;


@Mixin(BreakDoorGoal.class)
public abstract class EntityAiBreakDoorGoalMixin extends DoorInteractGoal {
    public EntityAiBreakDoorGoalMixin(MobEntity mob) {
        super(mob);
    }

    @Inject(method = "canStart", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onCanStart(CallbackInfoReturnable<Boolean> cir) {
        if (isServerSide(mob.getWorld())) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(mob.getBlockPos(), ZOMBIE_DOOR_PROT, getDimKey(mob), null);
            if (post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, null, deny -> {
                cir.setReturnValue(false);
            });

            checkEvent = new FlagCheckEvent(mob.getBlockPos(), MOB_GRIEFING, getDimKey(mob), null);
            if (post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, null, deny -> {
                cir.setReturnValue(null);
            });
        }
    }
}
