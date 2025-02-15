package de.z0rdak.yawp.mixin.flag.player;

import de.z0rdak.yawp.api.FlagEvaluator;
import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.core.BlockPos;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ShovelItem;
import net.minecraft.world.item.context.UseOnContext;
import net.minecraft.world.level.Level;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.core.flag.RegionFlag.SHOVEL_PATH;
import static de.z0rdak.yawp.core.flag.RegionFlag.TOOL_SECONDARY_USE;
import static de.z0rdak.yawp.handler.HandlerUtil.*;
import static de.z0rdak.yawp.api.MessageSender.sendFlagMsg;

@Mixin(ShovelItem.class)
public abstract class ShovelItemMixin {

    @Inject(method = "useOn", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/item/context/UseOnContext;getPlayer()Lnet/minecraft/world/entity/player/Player;", ordinal = 0), cancellable = true, allow = 1)
    public void onUseShovelOnBlock(UseOnContext context, CallbackInfoReturnable<InteractionResult> cir) {
        Level world = context.getLevel();
        BlockPos pos = context.getClickedPos();
        Player player = context.getPlayer();
        if (isServerSide(world) && player != null) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(pos, TOOL_SECONDARY_USE, getDimKey(player), player);
            if (Services.EVENT.post(checkEvent))
                return;
            FlagEvaluator.processCheck(checkEvent, deny -> {
                sendFlagMsg(deny);
                cir.setReturnValue(InteractionResult.PASS);
            });
            checkEvent = new FlagCheckEvent(pos, SHOVEL_PATH, getDimKey(player), player);
            if (Services.EVENT.post(checkEvent))
                return;
            FlagEvaluator.processCheck(checkEvent, deny -> {
                sendFlagMsg(deny);
                cir.setReturnValue(InteractionResult.PASS);
            });
        }
    }
}
