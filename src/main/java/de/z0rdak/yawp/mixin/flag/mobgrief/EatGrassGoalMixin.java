package de.z0rdak.yawp.mixin.flag.mobgrief;

import static de.z0rdak.yawp.core.flag.RegionFlag.MOB_GRIEFING;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.api.events.region.RegionEvents;
import de.z0rdak.yawp.handler.flags.HandlerUtil;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import net.minecraft.entity.ai.goal.EatGrassGoal;
import net.minecraft.entity.mob.MobEntity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

@Mixin(EatGrassGoal.class)
public class EatGrassGoalMixin {
    @Shadow
    private MobEntity mob;

    @Shadow
    private World world;

    @Inject(method = "tick()V", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/World;getGameRules()Lnet/minecraft/world/GameRules;"), cancellable = true, allow = 2)
    void onTick(CallbackInfo ci) { 
        BlockPos blockPos = mob.getBlockPos();
        FlagCheckEvent checkEvent = new FlagCheckEvent(blockPos, MOB_GRIEFING, world.getRegistryKey(), null);
        if (post(checkEvent))
            return;
        processCheck(checkEvent, null, deny -> {
            // mob.onEatingGrass();
            ci.cancel();
        });
    }
}
