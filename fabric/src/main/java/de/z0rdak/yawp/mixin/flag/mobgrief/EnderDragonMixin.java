package de.z0rdak.yawp.mixin.flag.mobgrief;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.boss.enderdragon.EnderDragon;
import net.minecraft.world.phys.AABB;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;
import org.spongepowered.asm.mixin.injection.callback.LocalCapture;

import static de.z0rdak.yawp.core.flag.RegionFlag.DRAGON_BLOCK_PROT;
import static de.z0rdak.yawp.core.flag.RegionFlag.MOB_GRIEFING;
import static de.z0rdak.yawp.handler.HandlerUtil.getDimKey;
import static de.z0rdak.yawp.api.FlagEvaluator.processCheck;


@Mixin(EnderDragon.class)
public abstract class EnderDragonMixin {

    @Inject(method = "checkWalls", locals = LocalCapture.CAPTURE_FAILSOFT, at = @At(value = "INVOKE", target = "Lnet/minecraft/world/level/Level;removeBlock(Lnet/minecraft/core/BlockPos;Z)Z"), allow = 1)
    public void onDragonDestroyBlocks(AABB box, CallbackInfoReturnable<Boolean> cir, int i, int j, int k, int l, int m, int n, boolean bl, boolean bl2, int o, int p, int q, BlockPos blockPos) {
        EnderDragon self = (EnderDragon) (Object) this;
        FlagCheckEvent checkEvent = new FlagCheckEvent(blockPos, DRAGON_BLOCK_PROT, getDimKey(self));
        if (Services.EVENT.post(checkEvent)) {
            return;
        }
        FlagState flagStateDragonProt = processCheck(checkEvent);

        checkEvent = new FlagCheckEvent(blockPos, MOB_GRIEFING, getDimKey(self));
        if (Services.EVENT.post(checkEvent)) {
            return;
        }
        FlagState flagStateGriefing = processCheck(checkEvent);

        if (flagStateGriefing != FlagState.DENIED && flagStateDragonProt != FlagState.DENIED) {
            bl2 = self.level().removeBlock(blockPos, false) || bl2;
        }
    }
}
