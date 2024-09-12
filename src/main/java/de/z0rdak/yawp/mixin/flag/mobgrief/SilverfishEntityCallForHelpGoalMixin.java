package de.z0rdak.yawp.mixin.flag.mobgrief;

import static de.z0rdak.yawp.api.events.region.RegionEvents.post;
import static de.z0rdak.yawp.core.flag.RegionFlag.MOB_GRIEFING;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.isServerSide;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.processCheck;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.core.flag.FlagState;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.Redirect;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import de.z0rdak.yawp.core.flag.RegionFlag;

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
import org.spongepowered.asm.mixin.injection.callback.LocalCapture;

@Mixin(targets = "net.minecraft.entity.mob.SilverfishEntity$CallForHelpGoal")
public abstract class SilverfishEntityCallForHelpGoalMixin {

    @Final
    @Shadow
    private SilverfishEntity silverfish;

    // TODO: FIX implementation by stripping most of vanilla code    
    @Inject(method = "tick()V",  locals = LocalCapture.CAPTURE_FAILSOFT, at = @At(value = "INVOKE", target = "Lnet/minecraft/world/World;breakBlock(Lnet/minecraft/util/math/BlockPos;ZLnet/minecraft/entity/Entity;)Z"))
    public void onSilverFishDestroyBlock(CallbackInfo ci, World world, Random random, BlockPos blockPos, int i) {
        if (isServerSide(silverfish)) {
            block0: while (i <= 5 && i >= -5) {
                int j = 0;
                while (j <= 10 && j >= -10) {
                    int k = 0;
                    while (k <= 10 && k >= -10) {
                        BlockPos blockPos2 = blockPos.add(j, i, k);
                        BlockState blockState = world.getBlockState(blockPos2);
                        Block block = blockState.getBlock();
                        if (block instanceof InfestedBlock) {
                            FlagCheckEvent checkEvent = new FlagCheckEvent(blockPos2, MOB_GRIEFING, world.getRegistryKey(), null);
                            boolean isCanceled = post(checkEvent);
                            FlagState flagState = processCheck(checkEvent, null, null);
                            boolean isDenied = flagState == FlagState.DENIED;
                            if (isCanceled) {
                                isDenied = false;
                            }
                            if (!world.getGameRules().getBoolean(GameRules.DO_MOB_GRIEFING) || isDenied) {
                                world.setBlockState(blockPos2, ((InfestedBlock)block).toRegularState(world.getBlockState(blockPos2)), Block.NOTIFY_ALL);
                            } else {
                                world.breakBlock(blockPos2, true, this.silverfish);
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
    }
}
