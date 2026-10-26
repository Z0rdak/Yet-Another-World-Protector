package de.z0rdak.yawp.mixin.flag.mobgrief;

import de.z0rdak.yawp.api.events.flag.FlagCheckRequest;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.core.BlockPos;
import net.minecraft.util.RandomSource;
import net.minecraft.world.entity.monster.Enderman;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraft.world.phys.Vec3;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.LocalCapture;


import static de.z0rdak.yawp.handler.HandlerUtil.getDimKey;
import static de.z0rdak.yawp.api.FlagEvaluator.processCheck;
import static de.z0rdak.yawp.handler.HandlerUtil.isServerSide;

@Mixin(targets = "net.minecraft.world.entity.monster.Enderman$EndermanTakeBlockGoal")
public abstract class EndermanTakeBlockGoalMixin {
    @Final
    @Shadow
    private Enderman enderman;

    @Inject(method = "tick", locals = LocalCapture.CAPTURE_FAILSOFT, at = @At(value = "INVOKE", target = "Lnet/minecraft/world/level/Level;removeBlock(Lnet/minecraft/core/BlockPos;Z)Z"), cancellable = true, allow = 1)
    public void onAttemptPickupBlock(CallbackInfo ci, RandomSource random, Level level, int xt, int yt, int zt, BlockPos pos, BlockState blockState, Vec3 from, Vec3 _to, BlockHitResult result, boolean reachable) {
        Enderman self = enderman;
        if (isServerSide(self)) {
            FlagCheckRequest checkEvent = new FlagCheckRequest(pos, RegionFlag.ENDERMAN_GRIEFING, getDimKey(self));
            if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, deny -> ci.cancel());

            checkEvent = new FlagCheckRequest(pos, RegionFlag.MOB_GRIEFING, getDimKey(self));
            if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, deny -> ci.cancel());
        }
    }
}
