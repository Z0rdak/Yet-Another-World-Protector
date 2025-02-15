package de.z0rdak.yawp.mixin.flag.player;

import de.z0rdak.yawp.api.FlagEvaluator;
import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.BucketItem;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.BlockHitResult;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;
import org.spongepowered.asm.mixin.injection.callback.LocalCapture;

import static de.z0rdak.yawp.core.flag.RegionFlag.PLACE_FLUIDS;
import static de.z0rdak.yawp.core.flag.RegionFlag.SCOOP_FLUIDS;
import static de.z0rdak.yawp.handler.HandlerUtil.*;
import static de.z0rdak.yawp.api.MessageSender.sendFlagMsg;

@Mixin(BucketItem.class)
public abstract class BucketItemMixin {

    @Inject(method = "use", locals = LocalCapture.CAPTURE_FAILSOFT,
            at = @At(value = "INVOKE", target = "Lnet/minecraft/world/level/Level;getBlockState(Lnet/minecraft/core/BlockPos;)Lnet/minecraft/world/level/block/state/BlockState;", ordinal = 0), cancellable = true, allow = 1)
    public void onFillBucket(Level world, Player user, InteractionHand hand, CallbackInfoReturnable<InteractionResult> cir, ItemStack itemStack, BlockHitResult blockHitResult, BlockPos blockPos, Direction direction, BlockPos blockPos2) {
        if (isServerSide(world)) {
            if (user != null) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(blockPos, SCOOP_FLUIDS, getDimKey(world), user);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                FlagEvaluator.processCheck(checkEvent, deny -> {
                    sendFlagMsg(deny);
                    cir.setReturnValue(InteractionResult.FAIL);
                });
            }
        }
    }

    @Inject(method = "use", locals = LocalCapture.CAPTURE_FAILSOFT,
            at = @At(value = "INVOKE", target = "Lnet/minecraft/world/item/BucketItem;emptyContents(Lnet/minecraft/world/entity/player/Player;Lnet/minecraft/world/level/Level;Lnet/minecraft/core/BlockPos;Lnet/minecraft/world/phys/BlockHitResult;)Z"), cancellable = true, allow = 1)
    public void onEmptyBucket(Level world, Player user, InteractionHand hand, CallbackInfoReturnable<InteractionResult> cir, ItemStack itemStack, BlockHitResult blockHitResult, BlockPos blockPos, Direction direction, BlockPos blockPos2, BlockState blockState, BlockPos blockPos3) {
        if (isServerSide(world)) {
            if (user != null) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(blockPos3, PLACE_FLUIDS, getDimKey(world), user);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                FlagEvaluator.processCheck(checkEvent, deny -> {
                    sendFlagMsg(deny);
                    cir.setReturnValue(InteractionResult.FAIL);
                });
            }
        }
    }
}