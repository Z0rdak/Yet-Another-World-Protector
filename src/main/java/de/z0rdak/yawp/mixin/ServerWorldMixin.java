package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.api.events.region.FlagCheckResult;
import de.z0rdak.yawp.api.events.region.RegionEvents;
import de.z0rdak.yawp.handler.flags.HandlerUtil;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.MessageSender;
import net.minecraft.entity.Entity;
import net.minecraft.entity.ExperienceOrbEntity;
import net.minecraft.entity.damage.DamageSource;
import net.minecraft.entity.mob.MobEntity;
import net.minecraft.entity.mob.SlimeEntity;
import net.minecraft.entity.passive.IronGolemEntity;
import net.minecraft.entity.passive.SnowGolemEntity;
import net.minecraft.entity.passive.VillagerEntity;
import net.minecraft.entity.passive.WanderingTraderEntity;
import net.minecraft.server.world.ServerWorld;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World.ExplosionSourceType;
import net.minecraft.world.explosion.Explosion;
import net.minecraft.world.explosion.ExplosionBehavior;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.api.events.region.RegionEvents.post;
import static de.z0rdak.yawp.core.flag.RegionFlag.*;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;

@Mixin(ServerWorld.class)
public class ServerWorldMixin {

    @Inject(method = "addEntity", at = @At("HEAD"), cancellable = true, allow = 1)
    public void onSpawnEntity(Entity entity, CallbackInfoReturnable<Boolean> cir) {
        if (!entity.getWorld().isClient) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(entity.getBlockPos(), SPAWNING_ALL, getEntityDim(entity), null);
            if (post(checkEvent)) {
                return;
            }
            HandlerUtil.processCheck(checkEvent, null, deny -> {
                cir.setReturnValue(false);
            });

            checkEvent = new FlagCheckEvent(entity.getBlockPos(), SPAWNING_MONSTER, getEntityDim(entity), null);
            if (post(checkEvent)) {
                return;
            }
            HandlerUtil.processCheck(checkEvent, null, deny -> {
                cir.setReturnValue(false);
            });
            
            checkEvent = new FlagCheckEvent(entity.getBlockPos(), SPAWNING_ANIMAL, getEntityDim(entity), null);
            if (post(checkEvent)) {
                return;
            }
            HandlerUtil.processCheck(checkEvent, null, deny -> {
                cir.setReturnValue(false);
            });
            
            checkEvent = new FlagCheckEvent(entity.getBlockPos(), SPAWNING_VILLAGER, getEntityDim(entity), null);
            if (post(checkEvent)) {
                return;
            }
            HandlerUtil.processCheck(checkEvent, null, deny -> {
                cir.setReturnValue(false);
            });
            
            checkEvent = new FlagCheckEvent(entity.getBlockPos(), SPAWNING_TRADER, getEntityDim(entity), null);
            if (post(checkEvent)) {
                return;
            }
            HandlerUtil.processCheck(checkEvent, null, deny -> {
                cir.setReturnValue(false);
            });
            
            checkEvent = new FlagCheckEvent(entity.getBlockPos(), SPAWNING_GOLEM, getEntityDim(entity), null);
            if (post(checkEvent)) {
                return;
            }
            HandlerUtil.processCheck(checkEvent, null, deny -> {
                cir.setReturnValue(false);
            });
            
            checkEvent = new FlagCheckEvent(entity.getBlockPos(), SPAWNING_SLIME, getEntityDim(entity), null);
            if (post(checkEvent)) {
                return;
            }
            HandlerUtil.processCheck(checkEvent, null, deny -> {
                cir.setReturnValue(false);
            });
            
            checkEvent = new FlagCheckEvent(entity.getBlockPos(), SPAWNING_XP, getEntityDim(entity), null);
            if (post(checkEvent)) {
                return;
            }
            HandlerUtil.processCheck(checkEvent, null, deny -> {
                cir.setReturnValue(false);
            });
        }
    }

    /**
     * Returning a null explosion will cause this event to be canceled.
     * An arrow on fire or fire charge shot by an e.g. dispenser will cause the type of the explosion to be ExplosionSourceType.TNT
     */
    @Inject(method = "createExplosion", at = @At("HEAD"), cancellable = true, allow = 1)
    public void onIgniteExplosive(Entity entity, DamageSource damageSource, ExplosionBehavior behavior, double x, double y, double z, float power, boolean createFire, ExplosionSourceType explosionSourceType, CallbackInfoReturnable<Explosion> cir) {
        ServerWorld world = (ServerWorld) (Object) this;
        if (isServerSide(world)) {         
            if (explosionSourceType == ExplosionSourceType.TNT || explosionSourceType == ExplosionSourceType.BLOCK) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(new BlockPos((int) x, (int) y, (int) z), IGNITE_EXPLOSIVES, getEntityDim(entity), null);
                if (post(checkEvent)) {
                    return;
                }
                HandlerUtil.processCheck(checkEvent, null, deny -> {
                    cir.setReturnValue(null);
                });
            }
            if (explosionSourceType == ExplosionSourceType.MOB) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(new BlockPos((int) x, (int) y, (int) z), MOB_GRIEFING, getEntityDim(entity), null);
                if (post(checkEvent)) {
                    return;
                }
                HandlerUtil.processCheck(checkEvent, null, deny -> {
                    cir.setReturnValue(null);
                });
            }
        }
    }
}
