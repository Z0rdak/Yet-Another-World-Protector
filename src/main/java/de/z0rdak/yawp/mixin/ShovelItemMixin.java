package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.handler.flags.HandlerUtil;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.MessageSender;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemUsageContext;
import net.minecraft.item.ShovelItem;
import net.minecraft.util.ActionResult;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.api.events.region.RegionEvents.post;
import static de.z0rdak.yawp.core.flag.RegionFlag.*;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;

@Mixin(ShovelItem.class)
public abstract class ShovelItemMixin {

    @Inject(method = "useOnBlock", at = @At(value = "INVOKE", target = "Lnet/minecraft/item/ItemUsageContext;getPlayer()Lnet/minecraft/entity/player/PlayerEntity;", ordinal = 0), cancellable = true, allow = 1)
    public void onUseShovelOnBlock(ItemUsageContext context, CallbackInfoReturnable<ActionResult> cir) {
        World world = context.getWorld();
        BlockPos pos = context.getBlockPos();
        PlayerEntity player = context.getPlayer();
        if (isServerSide(world)) {
            if (player != null) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(pos, TOOL_SECONDARY_USE, getEntityDim(player), player);
                if (post(checkEvent)) {
                    return;
                }
                HandlerUtil.processCheck(checkEvent, null, deny -> {
                    MessageSender.sendFlagMsg(deny);
                    cir.setReturnValue(ActionResult.PASS);
                });
                checkEvent = new FlagCheckEvent(pos, SHOVEL_PATH, getEntityDim(player), player);
                if (post(checkEvent)) {
                    return;
                }
                HandlerUtil.processCheck(checkEvent, null, deny -> {
                    MessageSender.sendFlagMsg(deny);
                    cir.setReturnValue(ActionResult.PASS);
                });
            }
        }
    }
}
