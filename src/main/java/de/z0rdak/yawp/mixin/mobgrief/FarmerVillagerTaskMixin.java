package de.z0rdak.yawp.mixin.mobgrief;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.entity.ai.brain.task.FarmerVillagerTask;
import net.minecraft.entity.passive.VillagerEntity;
import net.minecraft.server.world.ServerWorld;
import net.minecraft.util.math.BlockPos;

import static de.z0rdak.yawp.handler.flags.HandlerUtil.checkTargetEvent;

@Mixin(FarmerVillagerTask.class)
public class FarmerVillagerTaskMixin {
    @Shadow
    private List<BlockPos> targetPositions;

    @Inject(method = "shouldRun(Lnet/minecraft/server/world/ServerWorld;Lnet/minecraft/entity/passive/VillagerEntity;)Z", at = @At(value = "INVOKE", target = "Lnet/minecraft/entity/ai/brain/task/FarmerVillagerTask;chooseRandomTarget(Lnet/minecraft/server/world/ServerWorld;)Lnet/minecraft/util/math/BlockPos;"), cancellable = false, allow = 1)
    void onShouldRun(ServerWorld serverWorld, VillagerEntity villagerEntity, CallbackInfoReturnable<Boolean> cir) {
        DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(serverWorld.getRegistryKey());
        if (dimCache != null) {
            DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
            Set<BlockPos> protectedBlocks = 
                    this.targetPositions.stream()
                    .filter(blockPos -> checkTargetEvent(blockPos, RegionFlag.MOB_GRIEFING, dimRegion).isDenied())
                    .collect(Collectors.toSet());
            this.targetPositions.removeAll(protectedBlocks);
        }
    }
}
