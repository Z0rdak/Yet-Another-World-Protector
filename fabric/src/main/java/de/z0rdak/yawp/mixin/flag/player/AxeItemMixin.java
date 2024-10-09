package de.z0rdak.yawp.mixin.flag.player;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import net.minecraft.core.BlockPos;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.AxeItem;
import net.minecraft.world.item.context.UseOnContext;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.api.events.region.FabricRegionEvents.post;
import static de.z0rdak.yawp.core.flag.RegionFlag.AXE_STRIP;
import static de.z0rdak.yawp.core.flag.RegionFlag.TOOL_SECONDARY_USE;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;
import static de.z0rdak.yawp.util.text.MessageSender.sendFlagMsg;

@Mixin(AxeItem.class)
public abstract class AxeItemMixin {

    @Inject(method = "useOn", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onUseAxeOnBlock(UseOnContext context, CallbackInfoReturnable<InteractionResult> cir) {
        BlockPos pos = context.getClickedPos();
        Player player = context.getPlayer();
        if (isServerSide(context.getLevel()) && player != null) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(pos, TOOL_SECONDARY_USE, getDimKey(context.getLevel()), player);
            if (post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, null, deny -> {
                sendFlagMsg(deny);
                cir.setReturnValue(InteractionResult.PASS);
            });

            checkEvent = new FlagCheckEvent(pos, AXE_STRIP, getDimKey(context.getLevel()), player);
            if (post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, null, deny -> {
                sendFlagMsg(deny);
                cir.setReturnValue(InteractionResult.PASS);
            });
        }
    }
}
