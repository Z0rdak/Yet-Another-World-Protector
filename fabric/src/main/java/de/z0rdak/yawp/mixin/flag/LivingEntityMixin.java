package de.z0rdak.yawp.mixin.flag;

import de.z0rdak.yawp.api.FlagEvaluator;
import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.damagesource.DamageSource;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.boss.wither.WitherBoss;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.state.BlockState;
import org.jetbrains.annotations.Nullable;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;
import org.spongepowered.asm.mixin.injection.callback.LocalCapture;

import static de.z0rdak.yawp.core.flag.RegionFlag.*;
import static de.z0rdak.yawp.handler.HandlerUtil.*;
import static de.z0rdak.yawp.api.MessageSender.sendFlagMsg;

@Mixin(LivingEntity.class)
public abstract class LivingEntityMixin {

    @Unique
    @Nullable
    protected Player attackingPlayer;

    @Inject(method = "knockback", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onKnockback(double strength, double x, double z, CallbackInfo ci) {
        LivingEntity target = (LivingEntity) (Object) this;
        if (isServerSide(target)) {
            if (target instanceof Player) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(target.blockPosition(), KNOCKBACK_PLAYERS, getDimKey(target));
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                FlagEvaluator.processCheck(checkEvent, deny -> {
                    ci.cancel();
                });
                checkEvent = new FlagCheckEvent(target.blockPosition(), INVINCIBLE, getDimKey(target));
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                FlagEvaluator.processCheck(checkEvent, deny -> {
                    ci.cancel();
                });
            }
        }
    }

    // FIXME: Separate flags for dropLoot -> mobs, etc AND dropInventory ->
    @Inject(method = "dropAllDeathLoot", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onDrop(ServerLevel level, DamageSource source, CallbackInfo ci) {
        LivingEntity target = (LivingEntity) (Object) this;
        if (isServerSide(target)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(target.blockPosition(), DROP_LOOT_ALL, getDimKey(target));
            if (Services.EVENT.post(checkEvent)) {
                return;
            }
            FlagEvaluator.processCheck(checkEvent, deny -> {
                ci.cancel();
            });
            if (source.getEntity() instanceof Player player) {
                checkEvent = new FlagCheckEvent(target.blockPosition(), DROP_LOOT_PLAYER, getDimKey(target), player);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                FlagEvaluator.processCheck(checkEvent, deny -> {
                    sendFlagMsg(deny);
                    ci.cancel();
                });
            }
        }
    }

    @Inject(method = "causeFallDamage", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onFallDamage(float fallDistance, float damageMultiplier, DamageSource damageSource, CallbackInfoReturnable<Boolean> cir) {
        LivingEntity self = (LivingEntity) (Object) this;
        if (isServerSide(self)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(self.blockPosition(), FALL_DAMAGE, getDimKey(self));
            if (Services.EVENT.post(checkEvent)) {
                return;
            }
            FlagEvaluator.processCheck(checkEvent, deny -> {
                cir.setReturnValue(false);
            });
            if (isMonster(self)) {
                checkEvent = new FlagCheckEvent(self.blockPosition(), FALL_DAMAGE_MONSTERS, getDimKey(self));
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                FlagEvaluator.processCheck(checkEvent, deny -> {
                    cir.setReturnValue(false);
                });
            }
            if (isAnimal(self)) {
                checkEvent = new FlagCheckEvent(self.blockPosition(), FALL_DAMAGE_ANIMALS, getDimKey(self));
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                FlagEvaluator.processCheck(checkEvent, deny -> {
                    cir.setReturnValue(false);
                });
            }
            if (isVillager(self)) {
                checkEvent = new FlagCheckEvent(self.blockPosition(), FALL_DAMAGE_VILLAGERS, getDimKey(self));
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                FlagEvaluator.processCheck(checkEvent, deny -> {
                    cir.setReturnValue(false);
                });
            }
            if (isPlayer(self)) {
                checkEvent = new FlagCheckEvent(self.blockPosition(), FALL_DAMAGE_PLAYERS, getDimKey(self), (Player) self);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                FlagEvaluator.processCheck(checkEvent, deny -> {
                    sendFlagMsg(deny);
                    cir.setReturnValue(false);
                });
            }
        }
    }

    @Inject(method = "dropExperience", at = @At(value = "INVOKE",
            target = "Lnet/minecraft/world/entity/ExperienceOrb;award(Lnet/minecraft/server/level/ServerLevel;Lnet/minecraft/world/phys/Vec3;I)V"), cancellable = true, allow = 1)
    public void onXpDrop(CallbackInfo ci) {
        LivingEntity self = (LivingEntity) (Object) this;
        if (this.attackingPlayer != null) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(self.blockPosition(), XP_DROP_ALL, getDimKey(self));
            if (Services.EVENT.post(checkEvent))
                return;
            FlagEvaluator.processCheck(checkEvent, deny -> {
                ci.cancel();
            });
            
            // if this entity is killed by a player, prevent xp dropping
            checkEvent = new FlagCheckEvent(self.blockPosition(), XP_DROP_PLAYER, getDimKey(self), this.attackingPlayer);
            if (Services.EVENT.post(checkEvent))
                return;
            FlagEvaluator.processCheck(checkEvent, deny -> {
                sendFlagMsg(deny);
                ci.cancel();
            });
            
            if (isMonster(self)) {
                checkEvent = new FlagCheckEvent(self.blockPosition(), XP_DROP_MONSTER, getDimKey(self));
                if (Services.EVENT.post(checkEvent))
                    return;
                FlagEvaluator.processCheck(checkEvent, deny -> {
                    sendFlagMsg(deny);
                    ci.cancel();
                });
            } else {
                checkEvent = new FlagCheckEvent(self.blockPosition(), XP_DROP_OTHER, getDimKey(self));
                if (Services.EVENT.post(checkEvent))
                    return;
                FlagEvaluator.processCheck(checkEvent, deny -> {
                    sendFlagMsg(deny);
                    ci.cancel();
                });
            }
        }
        
        if (self instanceof Player) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(self.blockPosition(), RegionFlag.KEEP_XP, getDimKey(self));
            if (Services.EVENT.post(checkEvent))
                return;
            FlagEvaluator.processCheck(checkEvent, deny -> {
                ci.cancel();
            });
        }
    }

    /**
     * If a corresponding flag is set, this injection prevents the placing of a wither rose as a block and drops it as ItemEntity
     * as vanilla would do it when the gamerule doMobgrief is set to false
     */
    @Inject(method = "createWitherRose", locals = LocalCapture.CAPTURE_FAILSOFT,
            at = @At(value = "INVOKE", target = "Lnet/minecraft/world/level/Level;setBlock(Lnet/minecraft/core/BlockPos;Lnet/minecraft/world/level/block/state/BlockState;I)Z"), cancellable = true, allow = 1)
    public void onCreateWitherRose(@Nullable LivingEntity adversary, CallbackInfo ci, boolean bl, BlockPos pos, BlockState blockState) {
        LivingEntity self = (LivingEntity) (Object) this;
        Level world = self.level();
        if (isServerSide(world)) {
            ServerLevel serverLevel = (ServerLevel) self.level();
            if (adversary instanceof WitherBoss) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(pos, MOB_GRIEFING, serverLevel.dimension(), null);
                if (Services.EVENT.post(checkEvent))
                    return;
                FlagEvaluator.processCheck(checkEvent, deny -> {
                    // prevent the rose to be placed as block, but spawn it as item-entity as vanilla does it
                    ci.cancel();
                    ItemEntity itemEntity = new ItemEntity(serverLevel, self.getX(), self.getY(), self.getZ(), new ItemStack(Items.WITHER_ROSE));
                    serverLevel.addFreshEntity(itemEntity);
                });
            }
        }
    }
}
