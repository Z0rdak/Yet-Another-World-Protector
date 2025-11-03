package de.z0rdak.yawp.mixin.flag;

import de.z0rdak.yawp.api.FlagEvaluator;
import de.z0rdak.yawp.api.events.flag.FlagCheckRequest;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.monster.Creeper;
import net.minecraft.world.level.Explosion;
import net.minecraft.world.level.Level;
import org.jetbrains.annotations.Nullable;
import org.spongepowered.asm.mixin.*;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.LocalCapture;

import java.util.List;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.core.flag.RegionFlag.*;
import static de.z0rdak.yawp.handler.HandlerUtil.isServerSide;

@Mixin(Explosion.class)
public abstract class ExplosionMixin {

    @Final
    @Shadow
    private Level level;

    @Final
    @Shadow
    private @Nullable Entity source;

    @Unique
    private static void filterExplosionTargets(Explosion explosion, Level world, List<Entity> affectedEntities) {
        Predicate<FlagCheckRequest> isProtected = (fce) -> {
            if (Services.FLAG_EVENT_DISPATCHER.post(fce)) {
                return true;
            }
            return FlagEvaluator.processCheck(fce) == FlagState.DENIED;
        };
        BiFunction<List<BlockPos>, RegionFlag, Set<BlockPos>> filterBlocks = (in, flag) -> in.stream()
                .filter(blockPos -> isProtected.test(new FlagCheckRequest(blockPos, flag, world.dimension())))
                .collect(Collectors.toSet());
        BiFunction<List<Entity>, RegionFlag, Set<Entity>> filterEntities = (in, flag) -> in.stream()
                .filter(entity -> isProtected.test(new FlagCheckRequest(entity.blockPosition(), flag, world.dimension())))
                .collect(Collectors.toSet());

        explosion.getToBlow().removeAll(filterBlocks.apply(explosion.getToBlow(), EXPLOSION_BLOCK));
        affectedEntities.removeAll(filterEntities.apply(affectedEntities, EXPLOSION_ENTITY));

        if (explosion.getIndirectSourceEntity() != null) {
            boolean explosionTriggeredByCreeper = (explosion.getIndirectSourceEntity() instanceof Creeper);
            if (explosionTriggeredByCreeper) {
                explosion.getToBlow().removeAll(filterBlocks.apply(explosion.getToBlow(), EXPLOSION_CREEPER_BLOCK));
                affectedEntities.removeAll(filterEntities.apply(affectedEntities, EXPLOSION_CREEPER_ENTITY));
            }
        }
    }

    @Inject(method = "explode", locals = LocalCapture.CAPTURE_FAILSOFT, at = @At(value = "INVOKE", target = "Lnet/minecraft/world/phys/Vec3;<init>(DDD)V", ordinal = 1), allow = 1)
    public void onExplosion(CallbackInfo ci, Set<BlockPos> set, int i, float q, int k, int l, int r, int s, int t, int u, List<Entity> list) {
        /* List<Entity> list is a local variable - the affectedEntities - which, 
        is captured and provided as argument here through the LocalCapture feature 
        */
        Explosion explosion = (Explosion) (Object) this;
        if (this.level != null && isServerSide(this.level)) {
            if (this.source != null) {
                // flag check
                filterExplosionTargets(explosion, this.level, list);
            }
        }
    }
}
