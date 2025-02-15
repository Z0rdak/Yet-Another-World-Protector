package de.z0rdak.yawp.mixin.flag.player;

import de.z0rdak.yawp.api.FlagEvaluator;
import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.core.BlockPos;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.BoneMealItem;
import net.minecraft.world.item.context.UseOnContext;
import net.minecraft.world.level.Level;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.core.flag.RegionFlag.USE_BONEMEAL;
import static de.z0rdak.yawp.handler.HandlerUtil.*;
import static de.z0rdak.yawp.api.MessageSender.sendFlagMsg;

@Mixin(BoneMealItem.class)
public abstract class BoneMealItemMixin {

    @Inject(method = "useOn", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/item/BoneMealItem;growCrop(Lnet/minecraft/world/item/ItemStack;Lnet/minecraft/world/level/Level;Lnet/minecraft/core/BlockPos;)Z"), cancellable = true, allow = 1)
    public void onFertilizeBlock(UseOnContext context, CallbackInfoReturnable<InteractionResult> cir) {
        Level world = context.getLevel();
        BlockPos pos = context.getClickedPos();
        Player player = context.getPlayer();
        if (isServerSide(world) && player != null) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(pos, USE_BONEMEAL, getDimKey(world), player);
            if (Services.EVENT.post(checkEvent)) {
                return;
            }
            FlagEvaluator.processCheck(checkEvent, deny -> {
                sendFlagMsg(deny);
                cir.setReturnValue(InteractionResult.PASS);
            });
        }
    }
}
