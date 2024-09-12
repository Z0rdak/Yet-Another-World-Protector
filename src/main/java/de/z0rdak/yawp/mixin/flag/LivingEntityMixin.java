package de.z0rdak.yawp.mixin.flag;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.handler.flags.HandlerUtil;
import net.minecraft.block.BlockState;
import net.minecraft.entity.ItemEntity;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.boss.WitherEntity;
import net.minecraft.entity.damage.DamageSource;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.item.Items;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.jetbrains.annotations.Nullable;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;
import org.spongepowered.asm.mixin.injection.callback.LocalCapture;

import static de.z0rdak.yawp.api.events.region.RegionEvents.post;
import static de.z0rdak.yawp.core.flag.RegionFlag.*;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;
import static de.z0rdak.yawp.util.MessageSender.sendFlagMsg;

@Mixin(LivingEntity.class)
public abstract class LivingEntityMixin {

    @Shadow
    @Nullable
    protected PlayerEntity attackingPlayer;

    @Inject(method = "takeKnockback", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onKnockback(double strength, double x, double z, CallbackInfo ci) {
        LivingEntity target = (LivingEntity) (Object) this;
        if (isServerSide(target)) {
            if (target instanceof PlayerEntity) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(target.getBlockPos(), KNOCKBACK_PLAYERS, getDimKey(target), null);
                if (post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, null, deny -> {
                    ci.cancel();
                });
                checkEvent = new FlagCheckEvent(target.getBlockPos(), INVINCIBLE, getDimKey(target), null);
                if (post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, null, deny -> {
                    ci.cancel();
                });
            }
        }
    }

    // FIXME: Separate flags for dropLoot -> mobs, etc AND dropInventory ->
    @Inject(method = "drop", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onDrop(DamageSource source, CallbackInfo ci) {
        LivingEntity target = (LivingEntity) (Object) this;
        if (isServerSide(target)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(target.getBlockPos(), DROP_LOOT_ALL, getDimKey(target), null);
            if (post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, null, deny -> {
                ci.cancel();
            });
            if (source.getSource() instanceof PlayerEntity player) {
                checkEvent = new FlagCheckEvent(target.getBlockPos(), DROP_LOOT_PLAYER, getDimKey(target), player);
                if (post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, null, deny -> {
                    sendFlagMsg(deny);
                    ci.cancel();
                });
            }
        }
    }

    @Inject(method = "handleFallDamage", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onFallDamage(float fallDistance, float damageMultiplier, DamageSource damageSource, CallbackInfoReturnable<Boolean> cir) {
        LivingEntity self = (LivingEntity) (Object) this;
        if (isServerSide(self)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(self.getBlockPos(), FALL_DAMAGE, getDimKey(self), null);
            if (post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, null, deny -> {
                cir.setReturnValue(false);
            });
            if (isMonster(self)) {
                checkEvent = new FlagCheckEvent(self.getBlockPos(), FALL_DAMAGE_MONSTERS, getDimKey(self), null);
                if (post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, null, deny -> {
                    cir.setReturnValue(false);
                });
            }
            if (isAnimal(self)) {
                checkEvent = new FlagCheckEvent(self.getBlockPos(), FALL_DAMAGE_ANIMALS, getDimKey(self), null);
                if (post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, null, deny -> {
                    cir.setReturnValue(false);
                });
            }
            if (isVillager(self)) {
                checkEvent = new FlagCheckEvent(self.getBlockPos(), FALL_DAMAGE_VILLAGERS, getDimKey(self), null);
                if (post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, null, deny -> {
                    cir.setReturnValue(false);
                });
            }
            if (isPlayer(self)) {
                checkEvent = new FlagCheckEvent(self.getBlockPos(), FALL_DAMAGE_VILLAGERS, getDimKey(self), (PlayerEntity) self);
                if (post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, null, deny -> {
                    sendFlagMsg(deny);
                    cir.setReturnValue(false);
                });
            }
        }
    }

    @Inject(method = "dropXp", at = @At(value = "INVOKE", target = "Lnet/minecraft/entity/ExperienceOrbEntity;spawn(Lnet/minecraft/server/world/ServerWorld;Lnet/minecraft/util/math/Vec3d;I)V"), cancellable = true, allow = 1)
    public void onXpDrop(CallbackInfo ci) {
        LivingEntity self = (LivingEntity) (Object) this;
        FlagCheckEvent checkEvent = new FlagCheckEvent(self.getBlockPos(), XP_DROP_ALL, getDimKey(self), null);
        if (post(checkEvent))
            return;
        processCheck(checkEvent, null, deny -> {
            ci.cancel();
        });
        if (this.attackingPlayer != null) {
            checkEvent = new FlagCheckEvent(self.getBlockPos(), XP_DROP_PLAYER, getDimKey(self), this.attackingPlayer);
            if (post(checkEvent))
                return;
            processCheck(checkEvent, null, deny -> {
                sendFlagMsg(deny);
                ci.cancel();
            });
        }
        if (isMonster(self)) {
            checkEvent = new FlagCheckEvent(self.getBlockPos(), XP_DROP_MONSTER, getDimKey(self), null);
            if (post(checkEvent))
                return;
            processCheck(checkEvent, null, deny -> {
                sendFlagMsg(deny);
                ci.cancel();
            });
        } else {
            checkEvent = new FlagCheckEvent(self.getBlockPos(), XP_DROP_OTHER, getDimKey(self), null);
            if (post(checkEvent))
                return;
            processCheck(checkEvent, null, deny -> {
                sendFlagMsg(deny);
                ci.cancel();
            });
        }
    }

    /**
     * If a corresponding flag is set, this injection prevents the placing of a wither rose as a block and drops it as ItemEntity
     * as vanilla would do it when the gamerule doMobgrief is set to false
     */
    @Inject(method = "onKilledBy(Lnet/minecraft/entity/LivingEntity;)V", locals = LocalCapture.CAPTURE_FAILSOFT, at = @At(value = "INVOKE", target = "Lnet/minecraft/world/World;setBlockState(Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/BlockState;I)Z"), cancellable = true, allow = 1)
    public void onCreateWitherRose(@Nullable LivingEntity adversary, CallbackInfo ci, boolean bl, BlockPos pos, BlockState blockState) {
        LivingEntity self = (LivingEntity) (Object) this;
        World world = self.getWorld();
        if (isServerSide(world)) {
            if (adversary instanceof WitherEntity) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(pos, MOB_GRIEFING, world.getRegistryKey(), null);
                if (post(checkEvent))
                    return;
                processCheck(checkEvent, null, deny -> {
                    // prevent the rose to be placed as block, but spawn it as item-entity as vanilla does it
                    ci.cancel();
                    ItemEntity itemEntity = new ItemEntity(world, self.getX(), self.getY(), self.getZ(), new ItemStack(Items.WITHER_ROSE));
                    world.spawnEntity(itemEntity);
                });
            }            
        }
    }
}
