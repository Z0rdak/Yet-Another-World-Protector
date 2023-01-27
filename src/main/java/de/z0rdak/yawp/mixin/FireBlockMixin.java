package de.z0rdak.yawp.mixin;

import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.FireBlock;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import java.util.Random;


@Mixin(FireBlock.class)
public abstract class FireBlockMixin {

    @Inject(method = "tryCatchFire", at = @At(value = "HEAD"), cancellable = true, remap = false)
    private void spread(Level world, BlockPos pos, int spreadFactor, Random rand, int currentAge, Direction dir, CallbackInfo info) {
        info.cancel();
    }
}