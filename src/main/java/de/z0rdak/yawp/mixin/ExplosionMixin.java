package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.enchantment.ProtectionEnchantment;
import net.minecraft.entity.Entity;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.TntEntity;
import net.minecraft.entity.damage.DamageSource;
import net.minecraft.entity.mob.CreeperEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Box;
import net.minecraft.util.math.MathHelper;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.World;
import net.minecraft.world.explosion.Explosion;
import org.jetbrains.annotations.Nullable;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.handler.flags.HandlerUtil.checkTargetEvent;
import static net.minecraft.world.explosion.Explosion.getExposure;

@Mixin(Explosion.class)
public abstract class ExplosionMixin {

    @Shadow
    @Final
    private World world;
    @Shadow
    @Final
    private float power;
    @Shadow
    @Final
    private double x;
    @Shadow
    @Final
    private double y;
    @Shadow
    @Final
    private double z;
    @Shadow
    @Final
    @Nullable
    private Entity entity;
    @Shadow
    @Final
    private Map<PlayerEntity, Vec3d> affectedPlayers;

    @Shadow
    @Final
    private DamageSource damageSource;

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

    @Inject(method = "collectBlocksAndDamageEntities", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/World;getOtherEntities(Lnet/minecraft/entity/Entity;Lnet/minecraft/util/math/Box;)Ljava/util/List;"), cancellable = true, allow = 1)
    public void onExplosion(CallbackInfo ci) {
        Explosion explosion = (Explosion) (Object) this;
        // get needed info by copying from vanilla code
        float q = this.power * 2.0F;
        int k = MathHelper.floor(this.x - (double) q - 1.0);
        int l = MathHelper.floor(this.x + (double) q + 1.0);
        int r = MathHelper.floor(this.y - (double) q - 1.0);
        int s = MathHelper.floor(this.y + (double) q + 1.0);
        int t = MathHelper.floor(this.z - (double) q - 1.0);
        int u = MathHelper.floor(this.z + (double) q + 1.0);
        List<Entity> affectedEntities = this.world.getOtherEntities(this.entity, new Box(k, r, t, l, s, u));

        if (!this.world.isClient) {
            // flag check
            filterExplosionTargets(explosion, this.world, affectedEntities);

            // vanilla code continues
            Vec3d vec3d = new Vec3d(this.x, this.y, this.z);

            for (int v = 0; v < affectedEntities.size(); ++v) {
                Entity entity = affectedEntities.get(v);
                if (!entity.isImmuneToExplosion(explosion)) {
                    double w = Math.sqrt(entity.squaredDistanceTo(vec3d)) / (double) q;
                    if (w <= 1.0) {
                        double x = entity.getX() - this.x;
                        double y = (entity instanceof TntEntity ? entity.getY() : entity.getEyeY()) - this.y;
                        double z = entity.getZ() - this.z;
                        double aa = Math.sqrt(x * x + y * y + z * z);
                        if (aa != 0.0) {
                            x /= aa;
                            y /= aa;
                            z /= aa;
                            double ab = getExposure(vec3d, entity);
                            double ac = (1.0 - w) * ab;
                            entity.damage(this.damageSource, (float) ((int) ((ac * ac + ac) / 2.0 * 7.0 * (double) q + 1.0)));
                            double ad = ac;
                            if (entity instanceof LivingEntity) {
                                ad = ProtectionEnchantment.transformExplosionKnockback((LivingEntity) entity, ac);
                            }

                            entity.setVelocity(entity.getVelocity().add(x * ad, y * ad, z * ad));
                            if (entity instanceof PlayerEntity playerEntity) {
                                if (!playerEntity.isSpectator() && (!playerEntity.isCreative() || !playerEntity.getAbilities().flying)) {
                                    this.affectedPlayers.put(playerEntity, new Vec3d(x * ac, y * ac, z * ac));
                                }
                            }
                        }
                    }
                }
            }
            // cancel further processing of injected method, because we copied it anyway
            ci.cancel();
        }
    }
}
