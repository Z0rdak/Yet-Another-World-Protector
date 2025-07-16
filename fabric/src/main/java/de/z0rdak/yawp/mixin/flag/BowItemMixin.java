
package de.z0rdak.yawp.mixin.flag;

import de.z0rdak.yawp.api.FlagEvaluator;
import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.BowItem;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.api.MessageSender.sendFlagMsg;
import static de.z0rdak.yawp.core.flag.RegionFlag.FIRE_BOW;
import static de.z0rdak.yawp.handler.HandlerUtil.getDimKey;
import static de.z0rdak.yawp.handler.HandlerUtil.isServerSide;

@Mixin({BowItem.class})
public abstract class BowItemMixin {
    
    @Inject(method = "releaseUsing", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/item/BowItem;getPowerForTime(I)F"), allow = 1, cancellable = true)
    void onLooseArrow(ItemStack stack, Level level, LivingEntity entityLiving, int timeLeft, CallbackInfoReturnable<Boolean> cir) {
        Player player = (Player) entityLiving;
        if (isServerSide(player.level())) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(player.blockPosition(), FIRE_BOW, getDimKey(player), player);
            if (Services.EVENT.post(checkEvent)) {
                return;
            }
            FlagEvaluator.processCheck(checkEvent, denyResult -> {
                cir.setReturnValue(false);
                sendFlagMsg(denyResult);
            });
        }
    }
}