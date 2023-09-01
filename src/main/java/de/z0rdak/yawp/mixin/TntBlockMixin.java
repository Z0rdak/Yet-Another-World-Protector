package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.handler.flags.FlagCheckEvent;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.core.BlockPos;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.TntBlock;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.BlockHitResult;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.core.flag.RegionFlag.IGNITE_EXPLOSIVES;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.checkEvent;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.sendFlagMsg;

@Mixin(TntBlock.class)
public class TntBlockMixin {

    @Inject(method = "use", at = @At("HEAD"), cancellable = true, allow = 1)
    public void onUseFlintAndSteel(BlockState state, Level world, BlockPos pos, Player player2, InteractionHand hand, BlockHitResult hit, CallbackInfoReturnable<InteractionResult> cir) {
        ItemStack itemStack = player2.getItemInHand(hand);
        if (itemStack.sameItemStackIgnoreDurability(Items.FLINT_AND_STEEL.getDefaultInstance()) || itemStack.is(Items.FIRE_CHARGE)) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(world.dimension());
            FlagCheckEvent flagCheck = checkEvent(pos, IGNITE_EXPLOSIVES, dimCache.getDimensionalRegion(), player2);
            if (flagCheck.isDenied()) {
                sendFlagMsg(flagCheck);
                cir.setReturnValue(InteractionResult.CONSUME);
            }
        }
    }
}
