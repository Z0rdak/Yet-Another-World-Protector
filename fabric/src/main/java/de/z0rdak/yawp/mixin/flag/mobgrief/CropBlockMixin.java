package de.z0rdak.yawp.mixin.flag.mobgrief;

import de.z0rdak.yawp.api.events.flag.FlagCheckRequest;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.InsideBlockEffectApplier;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.CropBlock;
import net.minecraft.world.level.block.state.BlockState;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import static de.z0rdak.yawp.core.flag.RegionFlag.MOB_GRIEFING;
import static de.z0rdak.yawp.api.FlagEvaluator.processCheck;

@Mixin(CropBlock.class)
public class CropBlockMixin {

    @Inject(method = "entityInside", at = @At(value = "INVOKE", target = "Lnet/minecraft/server/level/ServerLevel;destroyBlock(Lnet/minecraft/core/BlockPos;ZLnet/minecraft/world/entity/Entity;)Z"), cancellable = true, allow = 1)
    public void onEntityCollision(BlockState blockState, Level level, BlockPos pos, Entity entity, InsideBlockEffectApplier insideBlockEffectApplier, boolean bl, CallbackInfo ci) {
        FlagCheckRequest checkEvent = new FlagCheckRequest(pos, MOB_GRIEFING, level.dimension(), null);
        if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent))
            return;
        processCheck(checkEvent, deny -> ci.cancel());
    }

    /*
    @Inject(method = "growCrops", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onCropGrow(Level level, BlockPos blockPos, BlockState blockState, CallbackInfo ci) {
        FlagCheckEvent checkEvent = new FlagCheckEvent(blockPos, CROP_TICK, level.dimension());
        if (Services.FLAG_EVENT_DISPATCHER.postCheck(checkEvent))
            return;
        processCheck(checkEvent, deny -> ci.cancel());
    }
     */


}
