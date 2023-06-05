package de.z0rdak.yawp.mixin;

import static de.z0rdak.yawp.core.flag.RegionFlag.IGNITE_EXPLOSIVES;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.checkPlayerEvent;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.sendFlagDeniedMsg;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import de.z0rdak.yawp.handler.flags.FlagCheckEvent;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.block.BlockState;
import net.minecraft.block.TntBlock;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.item.Items;
import net.minecraft.util.ActionResult;
import net.minecraft.util.Hand;
import net.minecraft.util.hit.BlockHitResult;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

@Mixin(TntBlock.class)
public class TntBlockMixin {

    @Inject(method = "onUse", at = @At("HEAD"), cancellable = true, allow = 1)
    public void onUseFlintAndSteel(BlockState state, World world, BlockPos pos, PlayerEntity player2, Hand hand, BlockHitResult hit, CallbackInfoReturnable<ActionResult> cir) {
        ItemStack itemStack = player2.getStackInHand(hand);
        if (itemStack.isOf(Items.FLINT_AND_STEEL) || itemStack.isOf(Items.FIRE_CHARGE)) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(world.getRegistryKey());
            FlagCheckEvent flagCheck = checkPlayerEvent(player2, pos, IGNITE_EXPLOSIVES, dimCache.getDimensionalRegion());
            if (flagCheck.isDenied()) {
                sendFlagDeniedMsg(flagCheck, player2);
                cir.setReturnValue(ActionResult.CONSUME);
            }
        }
    }
}
