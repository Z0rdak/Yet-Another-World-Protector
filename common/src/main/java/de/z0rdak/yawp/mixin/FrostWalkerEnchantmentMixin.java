package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.item.enchantment.EnchantedItemInUse;
import net.minecraft.world.item.enchantment.effects.ReplaceDisk;
import net.minecraft.world.phys.Vec3;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import static de.z0rdak.yawp.core.flag.RegionFlag.NO_WALKER_FREEZE;
import static de.z0rdak.yawp.handler.HandlerUtil.isServerSide;
import static de.z0rdak.yawp.api.FlagEvaluator.processCheck;

@Mixin(ReplaceDisk.class)
public class FrostWalkerEnchantmentMixin {

    @Inject(method = "apply", at = @At(value = "INVOKE", target = "Lnet/minecraft/server/level/ServerLevel;setBlockAndUpdate(Lnet/minecraft/core/BlockPos;Lnet/minecraft/world/level/block/state/BlockState;)Z"), cancellable = true)
    private void apply(ServerLevel world, int level, EnchantedItemInUse itemInUse, Entity entity, Vec3 pos, CallbackInfo info) {
        if (isServerSide(world)) {
            BlockPos blockPos = new BlockPos((int) pos.x, (int) pos.x, (int) pos.z);
            FlagCheckEvent checkEvent = new FlagCheckEvent(blockPos, NO_WALKER_FREEZE, world.dimension(), null);
            if (Services.EVENT.post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, denyResult -> {
                info.cancel();
            });
        }
    }
}
