package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.api.events.region.FlagCheckResult;
import de.z0rdak.yawp.handler.flags.HandlerUtil;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.item.enchantment.FrostWalkerEnchantment;
import net.minecraft.world.level.Level;
import net.minecraftforge.common.MinecraftForge;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import static de.z0rdak.yawp.core.flag.RegionFlag.NO_WALKER_FREEZE;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.handleAndSendMsg;

@Mixin(FrostWalkerEnchantment.class)
public class FrostWalkerEnchantmentMixin {

    @Inject(method = "onEntityMoved", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/level/Level;setBlockAndUpdate(Lnet/minecraft/core/BlockPos;Lnet/minecraft/world/level/block/state/BlockState;)Z"), cancellable = true)
    private static void onEntityMoved(LivingEntity entity, Level world, BlockPos pos, int p_45022_, CallbackInfo info) {
        if (!world.isClientSide) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(pos, NO_WALKER_FREEZE, world.dimension(), null);
            if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                return;
            }
            FlagCheckResult result = HandlerUtil.evaluate(checkEvent);
            MinecraftForge.EVENT_BUS.post(result);
            handleAndSendMsg(result, null, denyResult -> {
                info.cancel();
            });
        }
    }
}
