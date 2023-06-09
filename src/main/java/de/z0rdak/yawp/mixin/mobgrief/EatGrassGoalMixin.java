package de.z0rdak.yawp.mixin.mobgrief;

import static de.z0rdak.yawp.handler.flags.HandlerUtil.checkTargetEvent;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
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
        DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(world.getRegistryKey());
        if (dimCache == null) {
            return;
        }
        DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
        BlockPos blockPos = mob.getBlockPos();
        if (checkTargetEvent(blockPos, RegionFlag.MOB_GRIEFING, dimRegion).isDenied()) {
            mob.onEatingGrass();
            ci.cancel();
        }

    }
}
