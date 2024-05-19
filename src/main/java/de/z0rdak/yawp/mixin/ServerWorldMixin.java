package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.handler.flags.HandlerUtil;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.damagesource.DamageSource;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.level.Explosion;
import net.minecraft.world.level.ExplosionDamageCalculator;
import net.minecraft.world.level.Level;
import net.minecraftforge.common.MinecraftForge;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import javax.annotation.Nullable;

import static de.z0rdak.yawp.core.flag.RegionFlag.IGNITE_EXPLOSIVES;
import static de.z0rdak.yawp.core.flag.RegionFlag.MOB_GRIEFING;

@Mixin(ServerLevel.class)
public class ServerWorldMixin {

    /**
     * Returning a null explosion will cause this event to be canceled.
     * An arrow on fire or fire charge shot by an e.g. dispenser will cause the type of the explosion to be ExplosionSourceType.TNT
     */
    @Inject(method = "explode", at = @At("HEAD"), cancellable = true, allow = 1)
    public void onIgniteExplosive(@Nullable Entity entity, @Nullable DamageSource damageSource, @Nullable ExplosionDamageCalculator behavior, double x, double y, double z, float power, boolean createFire, Level.ExplosionInteraction explosionMode, CallbackInfoReturnable<Explosion> cir) {
        ServerLevel world = (ServerLevel) (Object) this;
        if (!world.isClientSide) {
            if (explosionMode == Level.ExplosionInteraction.TNT ||
                    explosionMode == Level.ExplosionInteraction.BLOCK) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(new BlockPos((int) x, (int) y, (int) z), IGNITE_EXPLOSIVES, world.dimension(), null);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
                HandlerUtil.processCheck(checkEvent, null, denyResult -> {
                    cir.setReturnValue(null);
                });
            }
            if (explosionMode == Level.ExplosionInteraction.MOB) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(new BlockPos((int) x, (int) y, (int) z), MOB_GRIEFING, world.dimension(), null);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
                HandlerUtil.processCheck(checkEvent, null, denyResult -> {
                    cir.setReturnValue(null);
                });
            }
        }
    }
}
