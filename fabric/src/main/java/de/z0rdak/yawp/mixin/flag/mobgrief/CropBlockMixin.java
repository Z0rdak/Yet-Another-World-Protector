package de.z0rdak.yawp.mixin.flag.mobgrief;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.Entity;
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

    @Inject(method = "entityInside", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/level/Level;destroyBlock(Lnet/minecraft/core/BlockPos;ZLnet/minecraft/world/entity/Entity;)Z"), cancellable = true, allow = 1)
    public void onEntityCollision(BlockState state, Level world, BlockPos pos, Entity entity, CallbackInfo ci) {
        FlagCheckEvent checkEvent = new FlagCheckEvent(pos, MOB_GRIEFING, world.dimension(), null);
        if (Services.EVENT.post(checkEvent))
            return;
        processCheck(checkEvent, deny -> ci.cancel());
    }

    /*
    @Inject(method = "growCrops", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onCropGrow(Level level, BlockPos blockPos, BlockState blockState, CallbackInfo ci) {
        FlagCheckEvent checkEvent = new FlagCheckEvent(blockPos, CROP_TICK, level.dimension());
        if (Services.EVENT.post(checkEvent))
            return;
        processCheck(checkEvent, deny -> ci.cancel());
    }
     */


}
