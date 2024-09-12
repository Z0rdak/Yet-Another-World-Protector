package de.z0rdak.yawp.mixin.flag.mobgrief;

import static de.z0rdak.yawp.api.events.region.RegionEvents.post;
import static de.z0rdak.yawp.core.flag.RegionFlag.MOB_GRIEFING;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.processCheck;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.api.events.region.RegionEvents;
import de.z0rdak.yawp.handler.flags.HandlerUtil;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import net.minecraft.entity.ai.goal.EatGrassGoal;
import net.minecraft.entity.mob.MobEntity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.injection.callback.LocalCapture;

@Mixin(EatGrassGoal.class)
public class EatGrassGoalMixin {
    @Final
    @Shadow
    private MobEntity mob;

    @Final
    @Shadow
    private World world;

    @Inject(method = "tick()V", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/World;breakBlock(Lnet/minecraft/util/math/BlockPos;Z)Z"), cancellable = true, allow = 1)
    void onEatGrass(CallbackInfo ci) { 
        BlockPos blockPos = mob.getBlockPos();
        FlagCheckEvent checkEvent = new FlagCheckEvent(blockPos, MOB_GRIEFING, world.getRegistryKey(), null);
        if (post(checkEvent))
            return;
        processCheck(checkEvent, null, deny -> {
            // mob.onEatingGrass();
            ci.cancel();
        });
    }

    @Inject(method = "tick()V", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/World;syncWorldEvent(ILnet/minecraft/util/math/BlockPos;I)V"), cancellable = true, allow = 1)
    void onEatGrassBlock(CallbackInfo ci) {
        BlockPos blockPos = mob.getBlockPos().down();
        FlagCheckEvent checkEvent = new FlagCheckEvent(blockPos, MOB_GRIEFING, world.getRegistryKey(), null);
        if (post(checkEvent))
            return;
        processCheck(checkEvent, null, deny -> {
            // mob.onEatingGrass();  
            ci.cancel();
        });
    }
}
