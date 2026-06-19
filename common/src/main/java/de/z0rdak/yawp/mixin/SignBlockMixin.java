package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.api.FlagRegister;
import de.z0rdak.yawp.api.events.flag.FlagCheckRequest;
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

import static de.z0rdak.yawp.api.FlagEvaluator.processCheck;
import static de.z0rdak.yawp.api.MessageSender.sendFlagMsg;

@Mixin(SignBlock.class)
public class SignBlockMixin {

    @Inject(method = "useWithoutItem", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/level/block/SignBlock;openTextEdit(Lnet/minecraft/world/entity/player/Player;Lnet/minecraft/world/level/block/entity/SignBlockEntity;Z)V"), cancellable = true)
    public void use(BlockState state, Level level, BlockPos pos, Player player, BlockHitResult hitResult, CallbackInfoReturnable<InteractionResult> cir) {
        FlagCheckRequest checkEvent = new FlagCheckRequest(pos, FlagRegister.PLAYER_EDIT_SIGNS, level.dimension(), player);
        if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent)) {
            return;
        }
        processCheck(checkEvent, deny -> {
            cir.setReturnValue(InteractionResult.FAIL);
            sendFlagMsg(deny);
        });
    }
}
