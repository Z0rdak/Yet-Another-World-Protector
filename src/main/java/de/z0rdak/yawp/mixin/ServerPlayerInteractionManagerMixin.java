package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.handler.stick.MarkerStickHandler;
import de.z0rdak.yawp.util.StickType;
import net.minecraft.item.ItemStack;
import net.minecraft.server.network.ServerPlayerEntity;
import net.minecraft.server.network.ServerPlayerInteractionManager;
import net.minecraft.util.ActionResult;
import net.minecraft.util.Hand;
import net.minecraft.util.hit.BlockHitResult;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import java.util.Objects;

import static de.z0rdak.yawp.util.StickUtil.getStickType;
import static de.z0rdak.yawp.util.StickUtil.isVanillaStick;

@Mixin(ServerPlayerInteractionManager.class)
public class ServerPlayerInteractionManagerMixin {

    @Inject(method = "interactBlock", at = @At("HEAD"), cancellable = true, allow = 1)
    public void useItemOn(ServerPlayerEntity player, World world, ItemStack involvedItemStack, Hand hand, BlockHitResult blockHitResult, CallbackInfoReturnable<ActionResult> cir) {
        if (!world.isClient) {
            // TODO: Maybe check if player is allowed to mark block
            BlockPos blockpos = blockHitResult.getBlockPos();
            if (isVanillaStick(involvedItemStack)) {
                StickType stickType = getStickType(involvedItemStack);
                if (Objects.requireNonNull(stickType) == StickType.MARKER) {
                    MarkerStickHandler.onMarkBlock(player, involvedItemStack, blockpos);
                    cir.setReturnValue(ActionResult.SUCCESS);
                }
            }
        }

    }

}
