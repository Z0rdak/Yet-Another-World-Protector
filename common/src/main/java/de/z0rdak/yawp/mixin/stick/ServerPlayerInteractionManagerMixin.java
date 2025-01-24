package de.z0rdak.yawp.mixin.stick;

import de.z0rdak.yawp.handler.MarkerStickHandler;
import de.z0rdak.yawp.util.StickType;
import de.z0rdak.yawp.util.StickUtil;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.server.level.ServerPlayerGameMode;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.BlockHitResult;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import java.util.Objects;

import static de.z0rdak.yawp.handler.HandlerUtil.isServerSide;

@Mixin(ServerPlayerGameMode.class)
public class ServerPlayerInteractionManagerMixin {

    @Inject(method = "useItemOn", at = @At("HEAD"), cancellable = true, allow = 1)
    public void useItemOn(ServerPlayer serverPlayer, Level level, ItemStack itemStack, InteractionHand interactionHand, BlockHitResult blockHitResult, CallbackInfoReturnable<InteractionResult> cir) {
        if (isServerSide(level)) {
            if (StickUtil.isMarker(itemStack)) {
                BlockPos blockpos = blockHitResult.getBlockPos();
                MarkerStickHandler.onMarkBlock(serverPlayer, itemStack, blockpos);
                cir.setReturnValue(InteractionResult.SUCCESS);
            }
        }
    }
}
