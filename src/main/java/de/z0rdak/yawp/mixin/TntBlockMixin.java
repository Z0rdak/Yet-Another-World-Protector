package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.handler.flags.HandlerUtil;
import de.z0rdak.yawp.util.MessageSender;
import net.minecraft.core.BlockPos;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.TntBlock;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.BlockHitResult;
import net.neoforged.neoforge.common.NeoForge;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.core.flag.RegionFlag.IGNITE_EXPLOSIVES;

@Mixin(TntBlock.class)
public class TntBlockMixin {

    @Inject(method = "use", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/level/block/TntBlock;onCaughtFire(Lnet/minecraft/world/level/block/state/BlockState;Lnet/minecraft/world/level/Level;Lnet/minecraft/core/BlockPos;Lnet/minecraft/core/Direction;Lnet/minecraft/world/entity/LivingEntity;)V"), cancellable = true, allow = 1)
    public void onUseFlintAndSteel(BlockState state, Level world, BlockPos pos, Player player2, InteractionHand hand, BlockHitResult hit, CallbackInfoReturnable<InteractionResult> cir) {
        if (!world.isClientSide) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(pos, IGNITE_EXPLOSIVES, world.dimension(), player2);
            if (NeoForge.EVENT_BUS.post(checkEvent).isCanceled()) {
                return;
            }
            HandlerUtil.processCheck(checkEvent, null, denyResult -> {
                cir.setReturnValue(InteractionResult.CONSUME);
                MessageSender.sendFlagMsg(denyResult);
            });
        }
    }
}
