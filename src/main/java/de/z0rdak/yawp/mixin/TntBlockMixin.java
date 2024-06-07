package de.z0rdak.yawp.mixin;

import static de.z0rdak.yawp.core.flag.RegionFlag.BREAK_BLOCKS;
import static de.z0rdak.yawp.core.flag.RegionFlag.IGNITE_EXPLOSIVES;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.api.events.region.RegionEvents;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.util.MessageSender;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.block.BlockState;
import net.minecraft.block.TntBlock;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.util.ActionResult;
import net.minecraft.util.Hand;
import net.minecraft.util.hit.BlockHitResult;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

@Mixin(TntBlock.class)
public class TntBlockMixin {

    @Inject(method = "onUse", at = @At(value = "INVOKE", target = "Lnet/minecraft/block/TntBlock;primeTnt(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/entity/LivingEntity;)V"), cancellable = true, allow = 1)
    public void onPlayerIgniteExplosive(BlockState state, World world, BlockPos pos, PlayerEntity player2, Hand hand, BlockHitResult hit, CallbackInfoReturnable<ActionResult> cir) {
        if (isServerSide(world)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(pos, IGNITE_EXPLOSIVES, world.getRegistryKey(), player2);
            if (RegionEvents.CHECK_FLAG.invoker().checkFlag(checkEvent)) {
                return;
            }
            processCheck(checkEvent, null, result -> {
                MessageSender.sendFlagMsg(result);
                cir.setReturnValue(ActionResult.CONSUME);
            });
        }
    }
}
