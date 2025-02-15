package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.core.BlockPos;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.SignBlock;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.BlockHitResult;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.core.flag.RegionFlag.NO_SIGN_EDIT;
import static de.z0rdak.yawp.api.FlagEvaluator.processCheck;
import static de.z0rdak.yawp.api.MessageSender.sendFlagMsg;

@Mixin(SignBlock.class)
public class SignBlockMixin {

    @Inject(method = "useWithoutItem", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/level/block/SignBlock;openTextEdit(Lnet/minecraft/world/entity/player/Player;Lnet/minecraft/world/level/block/entity/SignBlockEntity;Z)V"), cancellable = true)
    public void use(BlockState blockState, Level level, BlockPos blockPos, Player player, BlockHitResult blockHitResult, CallbackInfoReturnable<InteractionResult> cir) {
        FlagCheckEvent checkEvent = new FlagCheckEvent(blockPos, NO_SIGN_EDIT, level.dimension(), player);
        if (Services.EVENT.post(checkEvent)) {
            return;
        }
        processCheck(checkEvent, deny -> {
            cir.setReturnValue(InteractionResult.FAIL);
            sendFlagMsg(deny);
        });
    }
}
