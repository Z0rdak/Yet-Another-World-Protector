package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.handler.flags.FlagCheckEvent;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.block.BlockState;
import net.minecraft.block.TNTBlock;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.item.Items;
import net.minecraft.util.ActionResultType;
import net.minecraft.util.Hand;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.BlockRayTraceResult;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.core.flag.RegionFlag.IGNITE_EXPLOSIVES;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;

@Mixin(TNTBlock.class)
public class TntBlockMixin {

    @Inject(method = "use", at = @At("HEAD"), cancellable = true, allow = 1)
    public void onUseFlintAndSteel(BlockState state, World world, BlockPos pos, PlayerEntity player2, Hand hand, BlockRayTraceResult hit, CallbackInfoReturnable<ActionResultType> cir) {
        if (!world.isClientSide) {
            ItemStack itemStack = player2.getItemInHand(hand);
            if (itemStack.sameItemStackIgnoreDurability(Items.FLINT_AND_STEEL.getDefaultInstance()) || itemStack.sameItem(Items.FIRE_CHARGE.getDefaultInstance())) {
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(world.dimension());
                FlagCheckEvent flagCheck = checkEvent(pos, IGNITE_EXPLOSIVES, dimCache.getDimensionalRegion(), player2);
                if (flagCheck.isDenied()) {
                    sendFlagMsg(flagCheck);
                    cir.setReturnValue(ActionResultType.CONSUME);
                }
            }
        }
    }
}
