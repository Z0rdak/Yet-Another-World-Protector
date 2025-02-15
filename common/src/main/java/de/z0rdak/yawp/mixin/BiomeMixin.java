package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.api.FlagEvaluator;
import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.LevelReader;
import net.minecraft.world.level.WorldGenLevel;
import net.minecraft.world.level.biome.Biome;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(Biome.class)
public class BiomeMixin {

    @Inject(method = "shouldSnow", at = @At(value = "RETURN", opcode = 1), cancellable = true)
    public void onShouldSnow(LevelReader levelReader, BlockPos blockPos, CallbackInfoReturnable<Boolean> cir)
    {
        if (!(levelReader instanceof WorldGenLevel worldGenLevel)) {
            return;
        }
        ServerLevel level = worldGenLevel.getLevel();
        FlagCheckEvent checkEvent = new FlagCheckEvent(blockPos, RegionFlag.SNOW_FALL, level.dimension());
        if (Services.EVENT.post(checkEvent)){
            return;
        }
        FlagEvaluator.processCheck(checkEvent, deny -> {
            cir.setReturnValue(false);
        });
    }
}
