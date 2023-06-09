package de.z0rdak.yawp.mixin;


import static de.z0rdak.yawp.core.flag.RegionFlag.ZOMBIE_DOOR_PROT;
import static de.z0rdak.yawp.core.flag.RegionFlag.MOB_GRIEFING;

import static de.z0rdak.yawp.handler.flags.HandlerUtil.checkTargetEvent;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.getEntityDim;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import de.z0rdak.yawp.handler.flags.FlagCheckEvent;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;

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
        if (!mob.getWorld().isClient) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(mob));

            FlagCheckEvent flagCheck = checkTargetEvent(mob.getBlockPos(), ZOMBIE_DOOR_PROT, dimCache.getDimensionalRegion());
            if (flagCheck.isDenied()) {
                cir.setReturnValue(false);
            }
            flagCheck = checkTargetEvent(mob.getBlockPos(), MOB_GRIEFING, dimCache.getDimensionalRegion());
            if (flagCheck.isDenied()) {
                cir.setReturnValue(false);
            }
        }
    }
}
