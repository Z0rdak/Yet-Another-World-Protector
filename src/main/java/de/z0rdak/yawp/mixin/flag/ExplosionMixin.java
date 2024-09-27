package de.z0rdak.yawp.mixin.flag;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.core.flag.RegionFlag;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.monster.Creeper;
import net.minecraft.world.level.Explosion;
import net.minecraft.world.level.Level;
import org.jetbrains.annotations.Nullable;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.LocalCapture;

import java.util.List;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.api.events.region.RegionEvents.post;
import static de.z0rdak.yawp.core.flag.RegionFlag.*;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;

@Mixin(Explosion.class)
public abstract class ExplosionMixin {
    
    @Unique
    @Final
    private Level world;
    
    @Unique
    @Final
    private @Nullable Entity entity;

    @Unique
    private static void filterExplosionTargets(Explosion explosion, Level world, List<Entity> affectedEntities) {
        Predicate<FlagCheckEvent> isProtected = (fce) -> {
            if (post(fce)) {
                return true;
            }
            return processCheck(fce, null, null) == FlagState.DENIED;
        };
        BiFunction<List<BlockPos>, RegionFlag, Set<BlockPos>> filterBlocks = (in, flag) -> in.stream()
                .filter(blockPos -> isProtected.test(new FlagCheckEvent(blockPos, flag, getDimKey(world), null)))
                .collect(Collectors.toSet());
        BiFunction<List<Entity>, RegionFlag, Set<Entity>> filterEntities = (in, flag) -> in.stream()
                .filter(entity -> isProtected.test(new FlagCheckEvent(entity.blockPosition(), flag, getDimKey(world), null)))
                .collect(Collectors.toSet());

        explosion.getToBlow().removeAll(filterBlocks.apply(explosion.getToBlow(), EXPLOSION_BLOCK));
        affectedEntities.removeAll(filterEntities.apply(affectedEntities, EXPLOSION_ENTITY));

        if (explosion.getIndirectSourceEntity() != null) {
            boolean explosionTriggeredByCreeper = (explosion.getIndirectSourceEntity() instanceof Creeper);
            if (explosionTriggeredByCreeper) {
                explosion.getToBlow().removeAll(filterBlocks.apply(explosion.getToBlow(), EXPLOSION_CREEPER_BLOCK));
                affectedEntities.removeAll(filterEntities.apply(affectedEntities, EXPLOSION_CREEPER_ENTITY));
            } else {
                explosion.getToBlow().removeAll(filterBlocks.apply(explosion.getToBlow(), EXPLOSION_OTHER_BLOCKS));
                affectedEntities.removeAll(filterEntities.apply(affectedEntities, EXPLOSION_OTHER_ENTITY));
            }
        }
    }

    @Inject(method = "explode", locals = LocalCapture.CAPTURE_FAILSOFT, at = @At(value = "INVOKE", target = "Lnet/minecraft/world/phys/Vec3;<init>(DDD)V", ordinal = 1), allow = 1)
    public void onExplosion(CallbackInfo ci, Set<BlockPos> set, int i, float q, int k, int l, int r, int s, int t, int u, List<Entity> list) {
        /* List<Entity> list is a local variable - the affectedEntities - which, 
        is captured and provided as argument here through the LocalCapture feature 
        */
        Explosion explosion = (Explosion) (Object) this;
        if (isServerSide(world)) {
            if (this.entity != null) {
                // flag check
                filterExplosionTargets(explosion, this.world, list);
            }
        }
    }
}
