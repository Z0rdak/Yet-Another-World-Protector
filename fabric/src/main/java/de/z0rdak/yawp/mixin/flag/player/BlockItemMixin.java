package de.z0rdak.yawp.mixin.flag.player;

import de.z0rdak.yawp.api.FlagEvaluator;
import de.z0rdak.yawp.api.FlagRegister;
import de.z0rdak.yawp.api.MessageSender;
import de.z0rdak.yawp.api.events.flag.FlagCheckRequest;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.core.BlockPos;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.Identifier;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.item.BlockItem;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.level.Level;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(BlockItem.class)
public abstract class BlockItemMixin {

    @Inject(method = "place", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/item/BlockItem;placeBlock(Lnet/minecraft/world/item/context/BlockPlaceContext;Lnet/minecraft/world/level/block/state/BlockState;)Z"), cancellable = true)
    private void checkPlacement(BlockPlaceContext placeContext, CallbackInfoReturnable<net.minecraft.world.InteractionResult> cir) {
        Level level = placeContext.getLevel();
        if (level.isClientSide()) {
            return;
        }
        if (!(placeContext.getPlayer() instanceof ServerPlayer player)) {
            return;
        }

        BlockPos pos = placeContext.getClickedPos();
        FlagCheckRequest request = new FlagCheckRequest(pos, FlagRegister.PLAYER_PLACE_BLOCKS, level.dimension(), player);

        Identifier id = BuiltInRegistries.ITEM.getKey(placeContext.getItemInHand().getItem());
        // TODO Evaluate Id being allowed/denied/default

        if (Services.FLAG_EVENT_DISPATCHER.post(request)) {
            return;
        }
        FlagState state = FlagEvaluator.processCheck(request, MessageSender::sendFlagMsg);
        if (state == FlagState.DENIED) {
            cir.setReturnValue(net.minecraft.world.InteractionResult.FAIL);
        }
    }
}