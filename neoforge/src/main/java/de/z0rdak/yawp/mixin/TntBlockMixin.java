package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.api.FlagRegister;
import de.z0rdak.yawp.api.events.flag.FlagCheckRequest;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.core.BlockPos;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.TntBlock;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.BlockHitResult;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.api.FlagEvaluator.processCheck;
import static de.z0rdak.yawp.api.MessageSender.sendFlagMsg;
import static de.z0rdak.yawp.handler.HandlerUtil.isServerSide;

@Mixin(TntBlock.class)
public class TntBlockMixin {

    @Inject(method = "useItemOn", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/level/block/TntBlock;onCaughtFire(Lnet/minecraft/world/level/block/state/BlockState;Lnet/minecraft/world/level/Level;Lnet/minecraft/core/BlockPos;Lnet/minecraft/core/Direction;Lnet/minecraft/world/entity/LivingEntity;)Z"), cancellable = true, allow = 1)
    public void onPlayerIgniteExplosive(ItemStack itemStack, BlockState state, Level level, BlockPos pos, Player player2, InteractionHand hand, BlockHitResult hit, CallbackInfoReturnable<InteractionResult> cir) {
        if (isServerSide(level)) {
            FlagCheckRequest checkEvent = new FlagCheckRequest(pos, FlagRegister.PLAYER_IGNITE_EXPLOSIVES, level.dimension(), player2);
            if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, deny -> {
                cir.setReturnValue(InteractionResult.CONSUME);
                sendFlagMsg(deny);
            });
        }
    }
}
