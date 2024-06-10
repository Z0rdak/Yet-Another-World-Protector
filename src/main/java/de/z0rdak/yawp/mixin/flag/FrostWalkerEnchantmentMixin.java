package de.z0rdak.yawp.mixin.flag;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import net.minecraft.enchantment.FrostWalkerEnchantment;
import net.minecraft.entity.LivingEntity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import static de.z0rdak.yawp.api.events.region.RegionEvents.post;
import static de.z0rdak.yawp.core.flag.RegionFlag.NO_WALKER_FREEZE;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;

@Mixin(FrostWalkerEnchantment.class)
public class FrostWalkerEnchantmentMixin {

    @Inject(method = "freezeWater", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/World;setBlockState(Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/BlockState;)Z"), cancellable = true)
    private static void onEntityMoved(LivingEntity entity, World world, BlockPos pos, int level, CallbackInfo ci) {
        if (isServerSide(world)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(pos, NO_WALKER_FREEZE, getDimKey(world), null);
            if (post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, null, deny -> {
                ci.cancel();
            });
        }
    }

}
