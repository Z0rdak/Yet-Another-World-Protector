package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.api.events.flag.FlagCheckRequest;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.LilyPadBlock;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

import static de.z0rdak.yawp.api.FlagEvaluator.processCheck;
import static de.z0rdak.yawp.handler.HandlerUtil.isServerSide;

@Mixin(LilyPadBlock.class)
public class LilypadBlockMixin {

    @Redirect(method = "entityInside", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/level/Level;destroyBlock(Lnet/minecraft/core/BlockPos;ZLnet/minecraft/world/entity/Entity;)Z"))
    private boolean destroyBlock(Level level, BlockPos pos, boolean drop, Entity entity) {
        if (isServerSide(level)) {
            FlagCheckRequest checkEvent = new FlagCheckRequest(pos, RegionFlag.BREAK_BLOCKS, level.dimension());
            if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent) || processCheck(checkEvent) == FlagState.DENIED) {
                return false;
            }
        }

        return level.destroyBlock(pos, drop, entity);
    }
}
