package de.z0rdak.yawp.mixin.flag.mobgrief;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.core.flag.FlagState;
import net.minecraft.block.BlockState;
import net.minecraft.entity.boss.dragon.EnderDragonEntity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Box;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;
import org.spongepowered.asm.mixin.injection.callback.LocalCapture;

import static de.z0rdak.yawp.api.events.region.RegionEvents.post;
import static de.z0rdak.yawp.core.flag.RegionFlag.DRAGON_BLOCK_PROT;
import static de.z0rdak.yawp.core.flag.RegionFlag.MOB_GRIEFING;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.getDimKey;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.processCheck;


@Mixin(EnderDragonEntity.class)
public abstract class EnderDragonEntityMixin {
    
    @Inject(method = "destroyBlocks", locals = LocalCapture.CAPTURE_FAILSOFT, at = @At(value = "INVOKE", target = "Lnet/minecraft/world/World;removeBlock(Lnet/minecraft/util/math/BlockPos;Z)Z"), allow = 1)
    public void onDragonDestroyBlocks(Box box, CallbackInfoReturnable<Boolean> cir, int i, int j, int k, int l, int m, int n, boolean bl, boolean bl2, int o, int p, int q, BlockPos blockPos) {
        EnderDragonEntity self = (EnderDragonEntity) (Object) this;
     
        FlagCheckEvent checkEvent = new FlagCheckEvent(blockPos, DRAGON_BLOCK_PROT, getDimKey(self), null);
        if (post(checkEvent)) {
            return;
        }
        FlagState flagStateDragonPort = processCheck(checkEvent,  null, null);

        checkEvent = new FlagCheckEvent(blockPos, MOB_GRIEFING, getDimKey(self), null);
        if (post(checkEvent)) {
            return;
        }
        FlagState flagStateGriefing = processCheck(checkEvent, null, null);

        if (flagStateGriefing != FlagState.DENIED && flagStateDragonPort != FlagState.DENIED) {
            bl2 = self.getWorld().removeBlock(blockPos, false) || bl2;
        }
    }
}
