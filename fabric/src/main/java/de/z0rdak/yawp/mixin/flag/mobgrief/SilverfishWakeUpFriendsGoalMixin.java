package de.z0rdak.yawp.mixin.flag.mobgrief;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.util.RandomSource;
import net.minecraft.world.entity.monster.Silverfish;
import net.minecraft.world.level.GameRules;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.InfestedBlock;
import net.minecraft.world.level.block.state.BlockState;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.LocalCapture;

import static de.z0rdak.yawp.core.flag.RegionFlag.MOB_GRIEFING;
import static de.z0rdak.yawp.handler.HandlerUtil.isServerSide;
import static de.z0rdak.yawp.api.FlagEvaluator.processCheck;

@Mixin(targets = "net.minecraft.world.entity.monster.Silverfish$SilverfishWakeUpFriendsGoal")
public abstract class SilverfishWakeUpFriendsGoalMixin {

    @Final
    @Shadow
    private Silverfish silverfish;

    // TODO: FIX implementation by stripping most of vanilla code    
    @Inject(method = "tick()V", locals = LocalCapture.CAPTURE_FAILSOFT, at = @At(value = "INVOKE", target = "Lnet/minecraft/world/level/Level;destroyBlock(Lnet/minecraft/core/BlockPos;ZLnet/minecraft/world/entity/Entity;)Z"))
    public void onSilverFishDestroyBlock(CallbackInfo ci, Level level, RandomSource randomSource, BlockPos blockPos, int i) {
        if (isServerSide(silverfish)) {
            block0:
            while (i <= 5 && i >= -5) {
                int j = 0;
                while (j <= 10 && j >= -10) {
                    int k = 0;
                    while (k <= 10 && k >= -10) {
                        BlockPos blockPos2 = blockPos.offset(j, i, k);
                        BlockState blockState = level.getBlockState(blockPos2);
                        Block block = blockState.getBlock();
                        if (block instanceof InfestedBlock) {
                            FlagCheckEvent checkEvent = new FlagCheckEvent(blockPos2, MOB_GRIEFING, level.dimension());
                            boolean isCanceled = Services.EVENT.post(checkEvent);
                            FlagState flagState = processCheck(checkEvent);
                            boolean isDenied = flagState == FlagState.DENIED;
                            if (isCanceled) {
                                isDenied = false;
                            }
                            if (!((ServerLevel)level).getGameRules().getBoolean(GameRules.RULE_MOBGRIEFING) || isDenied) {
                                level.destroyBlock(blockPos2, true, this.silverfish);
                            } else {
                                level.setBlock(blockPos2, ((InfestedBlock) block).hostStateByInfested(level.getBlockState(blockPos2)), 3);
                            }
                            if (randomSource.nextBoolean()) break block0;
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
