package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.handler.flags.HandlerUtil;
import net.minecraft.entity.Entity;
import net.minecraft.util.DamageSource;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.Explosion;
import net.minecraft.world.ExplosionContext;
import net.minecraft.world.server.ServerWorld;
import net.minecraftforge.common.MinecraftForge;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import javax.annotation.Nullable;

import static de.z0rdak.yawp.core.flag.RegionFlag.IGNITE_EXPLOSIVES;

@Mixin(ServerWorld.class)
public class ServerWorldMixin {

    /**
     * Returning a null explosion will cause this event to be canceled.
     * An arrow on fire or fire charge shot by an e.g. dispenser will cause the type of the explosion to be ExplosionSourceType.TNT
     * TODO: Test if this is needed or ignition is fully covered by events
     */
    @Inject(method = "explode", at = @At("HEAD"), cancellable = true, allow = 1)
    public void onIgniteExplosive(@Nullable Entity entity, @Nullable DamageSource damageSource, @Nullable ExplosionContext behavior, double x, double y, double z, float power, boolean createFire, Explosion.Mode explosionMode, CallbackInfoReturnable<Explosion> cir) {
        ServerWorld world = (ServerWorld) (Object) this;
        if (!world.isClientSide) {
            if (explosionMode == Explosion.Mode.BREAK || explosionMode == Explosion.Mode.DESTROY) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(new BlockPos((int) x, (int) y, (int) z), IGNITE_EXPLOSIVES, world.dimension(), null);
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
