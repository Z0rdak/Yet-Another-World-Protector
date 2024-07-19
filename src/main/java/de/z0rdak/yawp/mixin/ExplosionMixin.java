package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.entity.Entity;
import net.minecraft.entity.mob.CreeperEntity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraft.world.explosion.Explosion;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.LocalCapture;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.handler.flags.HandlerUtil.checkTargetEvent;

@Mixin(Explosion.class)
public abstract class ExplosionMixin {

    @Shadow
    @Final
    private World world;

    @Unique
    private static void filterExplosionTargets(Explosion explosion, World world, List<Entity> affectedEntities) {
        DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(world.getRegistryKey());
        if (dimCache != null) {
            DimensionalRegion dimRegion = dimCache.getDimensionalRegion();

            Set<BlockPos> protectedBlocks = explosion.getAffectedBlocks().stream()
                    .filter(blockPos -> checkTargetEvent(blockPos, RegionFlag.EXPLOSION_BLOCK, dimRegion).isDenied())
                    .collect(Collectors.toSet());
            Set<Entity> protectedEntities = affectedEntities.stream()
                    .filter(entity -> checkTargetEvent(entity.getBlockPos(), RegionFlag.EXPLOSION_ENTITY, dimRegion).isDenied())
                    .collect(Collectors.toSet());

            explosion.getAffectedBlocks().removeAll(protectedBlocks);
            affectedEntities.removeAll(protectedEntities);

            if (explosion.getCausingEntity() != null) {
                boolean explosionTriggeredByCreeper = (explosion.getCausingEntity() instanceof CreeperEntity);
                if (explosionTriggeredByCreeper) {
                    protectedBlocks = explosion.getAffectedBlocks().stream()
                            .filter(blockPos -> checkTargetEvent(blockPos, RegionFlag.EXPLOSION_CREEPER_BLOCK, dimRegion).isDenied())
                            .collect(Collectors.toSet());
                    protectedEntities = affectedEntities.stream()
                            .filter(entity -> checkTargetEvent(entity.getBlockPos(), RegionFlag.EXPLOSION_CREEPER_ENTITY, dimRegion).isDenied())
                            .collect(Collectors.toSet());
                } else {
                    protectedBlocks = explosion.getAffectedBlocks().stream()
                            .filter(blockPos -> checkTargetEvent(blockPos, RegionFlag.EXPLOSION_OTHER_BLOCKS, dimRegion).isDenied())
                            .collect(Collectors.toSet());
                    protectedEntities = affectedEntities.stream()
                            .filter(entity -> checkTargetEvent(entity.getBlockPos(), RegionFlag.EXPLOSION_OTHER_ENTITY, dimRegion).isDenied())
                            .collect(Collectors.toSet());
                }
                explosion.getAffectedBlocks().removeAll(protectedBlocks);
                affectedEntities.removeAll(protectedEntities);
            }
        }
    }

    @Inject(method = "collectBlocksAndDamageEntities", locals = LocalCapture.CAPTURE_FAILSOFT, at = @At(value = "INVOKE", target = "Lnet/minecraft/util/math/Vec3d;<init>(DDD)V", ordinal = 1), allow = 1)
    public void onExplosion(CallbackInfo ci, Set<BlockPos> set, int i, float q, int k, int l, int r, int s, int t, int u, List<Entity> list) {
        /* List<Entity> list is a local variable - the affectedEntities - which, 
        is captured and provided as argument here through the LocalCapture feature 
        */
        Explosion explosion = (Explosion) (Object) this;
        if (!this.world.isClient) {
            filterExplosionTargets(explosion, this.world, list);
        }
    }
}
