package de.z0rdak.yawp.mixin.flag.mobgrief;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.goal.EatBlockGoal;
import net.minecraft.world.level.Level;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import static de.z0rdak.yawp.core.flag.RegionFlag.MOB_GRIEFING;
import static de.z0rdak.yawp.api.FlagEvaluator.processCheck;

@Mixin(EatBlockGoal.class)
public class EatBlockGoalMixin {
    @Final
    @Shadow
    private Mob mob;

    @Final
    @Shadow
    private Level level;

    @Inject(method = "tick()V", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/level/Level;destroyBlock(Lnet/minecraft/core/BlockPos;Z)Z"), cancellable = true, allow = 1)
    void onEatGrass(CallbackInfo ci) {
        BlockPos blockPos = mob.blockPosition();
        FlagCheckEvent checkEvent = new FlagCheckEvent(blockPos, MOB_GRIEFING, level.dimension());
        if (Services.EVENT.post(checkEvent))
            return;
        processCheck(checkEvent, deny -> {
            // mob.onEatingGrass();
            ci.cancel();
        });
    }

    @Inject(method = "tick()V", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/level/Level;levelEvent(ILnet/minecraft/core/BlockPos;I)V"), cancellable = true, allow = 1)
    void onEatGrassBlock(CallbackInfo ci) {
        BlockPos blockPos = mob.blockPosition().below();
        FlagCheckEvent checkEvent = new FlagCheckEvent(blockPos, MOB_GRIEFING, level.dimension());
        if (Services.EVENT.post(checkEvent))
            return;
        processCheck(checkEvent, deny -> {
            // mob.onEatingGrass();  
            ci.cancel();
        });
    }
}
