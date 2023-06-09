package de.z0rdak.yawp.mixin.mobgrief;

import static de.z0rdak.yawp.handler.flags.HandlerUtil.checkTargetEvent;

import org.jetbrains.annotations.Nullable;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.Redirect;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.MobGriefingHelper;

import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;

import net.minecraft.block.Block;
import net.minecraft.block.BlockState;
import net.minecraft.block.InfestedBlock;
import net.minecraft.entity.mob.SilverfishEntity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.random.Random;
import net.minecraft.world.World;
import net.minecraft.world.GameRules;

@Mixin(targets = "net.minecraft.entity.mob.SilverfishEntity$CallForHelpGoal")
public abstract class SilverfishEntityCallForHelpGoalMixin {

    @Shadow
    private SilverfishEntity silverfish;
    @Shadow
    private int delay;

    @Inject(method = "tick()V", at = @At(value = "HEAD"), cancellable = true)
    public void onTick(CallbackInfo ci) {
        --this.delay;
        if (this.delay <= 0) {
            World world = this.silverfish.world;
            Random random = this.silverfish.getRandom();
            BlockPos blockPos = this.silverfish.getBlockPos();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(world.getRegistryKey());
            DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
            int i = 0;
            block0: while (i <= 5 && i >= -5) {
                int j = 0;
                while (j <= 10 && j >= -10) {
                    int k = 0;
                    while (k <= 10 && k >= -10) {
                        BlockPos blockPos2 = blockPos.add(j, i, k);
                        BlockState blockState = world.getBlockState(blockPos2);
                        Block block = blockState.getBlock();
                        if (block instanceof InfestedBlock) {
                            if (world.getGameRules().getBoolean(GameRules.DO_MOB_GRIEFING) &&
                                    ! checkTargetEvent(blockPos2, RegionFlag.MOB_GRIEFING, dimRegion).isDenied() ) {
                                world.breakBlock(blockPos2, true, this.silverfish);
                            } else {
                                world.setBlockState(blockPos2, ((InfestedBlock)block).toRegularState(world.getBlockState(blockPos2)), Block.NOTIFY_ALL);
                            }
                            if (random.nextBoolean()) break block0;
                        }
                        k = (k <= 0 ? 1 : 0) - k;
                    }
                    j = (j <= 0 ? 1 : 0) - j;
                }
                i = (i <= 0 ? 1 : 0) - i;
            }
        }
        ci.cancel();
    }

}
