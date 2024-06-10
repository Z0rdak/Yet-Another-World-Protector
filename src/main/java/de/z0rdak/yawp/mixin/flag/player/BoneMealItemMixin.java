package de.z0rdak.yawp.mixin.flag.player;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.BoneMealItem;
import net.minecraft.item.ItemUsageContext;
import net.minecraft.util.ActionResult;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.api.events.region.RegionEvents.post;
import static de.z0rdak.yawp.core.flag.RegionFlag.USE_BONEMEAL;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;
import static de.z0rdak.yawp.util.MessageSender.sendFlagMsg;

@Mixin(BoneMealItem.class)
public abstract class BoneMealItemMixin {

    @Inject(method = "useOnBlock", at = @At(value = "INVOKE", target = "Lnet/minecraft/item/BoneMealItem;useOnFertilizable(Lnet/minecraft/item/ItemStack;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Z"), cancellable = true, allow = 1)
    public void onFertilizeBlock(ItemUsageContext context, CallbackInfoReturnable<ActionResult> cir) {
        World world = context.getWorld();
        BlockPos pos = context.getBlockPos();
        PlayerEntity player = context.getPlayer();
        if (isServerSide(world) && player != null) {   
            FlagCheckEvent checkEvent = new FlagCheckEvent(pos, USE_BONEMEAL, getDimKey(world), player);
            if (post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, null, deny -> {
                sendFlagMsg(deny);
                cir.setReturnValue(ActionResult.PASS);
            });
        }
    }
}
