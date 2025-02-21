package de.z0rdak.yawp.mixin.integrations.snowrealmagic;

import de.z0rdak.yawp.api.FlagEvaluator;
import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.handler.HandlerUtil;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerLevel;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import snownee.snow.WorldTickHandler;

@Mixin(WorldTickHandler.class)
public class SRMWorldTickHandlerMixin {

    @Inject(method = "doSnow", at = @At(value = "INVOKE", target = "Lsnownee/snow/Hooks;convert(Lnet/minecraft/world/level/LevelAccessor;Lnet/minecraft/core/BlockPos;Lnet/minecraft/world/level/block/state/BlockState;IIZ)Z", ordinal = 0), cancellable = true, remap = false)
    private static void doSnow(ServerLevel level, BlockPos.MutableBlockPos pos, CallbackInfo ci) {
        FlagCheckEvent checkEvent = new FlagCheckEvent(pos, RegionFlag.SNOW_FALL, level.dimension());
        if (Services.EVENT.post(checkEvent)){
            return;
        }
        FlagEvaluator.processCheck(checkEvent, deny -> {
            ci.cancel();
        });
    }
}
