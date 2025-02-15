package de.z0rdak.yawp.mixin.flag.player;

import de.z0rdak.yawp.api.FlagEvaluator;
import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.config.server.FlagConfig;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.damagesource.DamageSource;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.npc.Villager;
import net.minecraft.world.entity.npc.WanderingTrader;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import java.util.Set;

import static de.z0rdak.yawp.core.flag.RegionFlag.*;
import static de.z0rdak.yawp.handler.HandlerUtil.*;
import static de.z0rdak.yawp.api.MessageSender.sendFlagMsg;

@Mixin(Player.class)
public abstract class PlayerMixin {

    // TODO: This does not seem to be triggered
    @Inject(method = "drop(Lnet/minecraft/world/item/ItemStack;ZZ)Lnet/minecraft/world/entity/item/ItemEntity;", at = @At(value = "HEAD"), allow = 1, cancellable = true)
    private void onDropItem(ItemStack stack, boolean b1, boolean retainOwnership, CallbackInfoReturnable<ItemStack> cir) {
        Player player = (Player) (Object) this;
        if (isServerSide(player)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(player.blockPosition(), ITEM_DROP, getDimKey(player), player);
            if (Services.EVENT.post(checkEvent))
                return;
            FlagEvaluator.processCheck(checkEvent, deny -> {
                sendFlagMsg(deny);
                player.addItem(stack);
                cir.setReturnValue(null);
            });
        }
    }

    @Inject(method = "giveExperienceLevels", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onGainLevels(int levels, CallbackInfo ci) {
        Player player = (Player) (Object) this;
        if (isServerSide(player)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(player.blockPosition(), LEVEL_FREEZE, getDimKey(player), player);
            if (Services.EVENT.post(checkEvent))
                return;
            FlagEvaluator.processCheck(checkEvent, deny -> {
                sendFlagMsg(deny);
                ci.cancel();
            });
        }
    }

    @Inject(method = "giveExperiencePoints", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onGainExperience(int experience, CallbackInfo ci) {
        Player player = (Player) (Object) this;
        if (isServerSide(player)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(player.blockPosition(), XP_FREEZE, getDimKey(player), player);
            if (Services.EVENT.post(checkEvent))
                return;
            FlagEvaluator.processCheck(checkEvent, deny -> {
                sendFlagMsg(deny);
                ci.cancel();
            });
        }
    }

    @Inject(method = "actuallyHurt", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/entity/player/Player;getDamageAfterArmorAbsorb(Lnet/minecraft/world/damagesource/DamageSource;F)F"), cancellable = true, allow = 1)
    public void onHurt(DamageSource source, float amount, CallbackInfo ci) {
        Player self = (Player) (Object) this;
        if (isServerSide(self)) {
            if (source.getEntity() instanceof Player attackingPlayer) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(self.blockPosition(), NO_PVP, getDimKey(self), attackingPlayer);
                if (Services.EVENT.post(checkEvent))
                    return;
                FlagEvaluator.processCheck(checkEvent, deny -> {
                    sendFlagMsg(deny);
                    ci.cancel();
                });
            }
            FlagCheckEvent checkEvent = new FlagCheckEvent(self.blockPosition(), INVINCIBLE, getDimKey(self));
            if (Services.EVENT.post(checkEvent))
                return;
            FlagEvaluator.processCheck(checkEvent, deny -> {
                ci.cancel();
            });
        }
    }

    @Inject(method = "actuallyHurt", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/entity/player/Player;setAbsorptionAmount(F)V"), cancellable = true, allow = 1)
    public void onReceiveDamage(DamageSource damageSource, float amount, CallbackInfo ci) {
        Player player = (Player) (Object) this;
        if (isServerSide(player)) {
            // TODO: meele-player flag
        }
    }


    @Inject(method = "attack", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onAttackEntity(Entity target, CallbackInfo ci) {
        if (isServerSide(target)) {
            Player player = (Player) (Object) this;
            if (target == null) return;
            if (target instanceof Player) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(target.blockPosition(), MELEE_PLAYERS, getDimKey(player), player);
                if (Services.EVENT.post(checkEvent))
                    return;
                FlagEvaluator.processCheck(checkEvent, deny -> {
                    sendFlagMsg(deny);
                    ci.cancel();
                });
            } else {
                if (isAnimal(target)) {
                    FlagCheckEvent checkEvent = new FlagCheckEvent(target.blockPosition(), MELEE_ANIMALS, getDimKey(player), player);
                    if (Services.EVENT.post(checkEvent))
                        return;
                    FlagEvaluator.processCheck(checkEvent, deny -> {
                        sendFlagMsg(deny);
                        ci.cancel();
                    });
                }
                if (isMonster(target)) {
                    FlagCheckEvent checkEvent = new FlagCheckEvent(target.blockPosition(), MELEE_MONSTERS, getDimKey(player), player);
                    if (Services.EVENT.post(checkEvent))
                        return;
                    FlagEvaluator.processCheck(checkEvent, deny -> {
                        sendFlagMsg(deny);
                        ci.cancel();
                    });
                }
                if (target instanceof Villager) {
                    FlagCheckEvent checkEvent = new FlagCheckEvent(target.blockPosition(), MELEE_VILLAGERS, getDimKey(player), player);
                    if (Services.EVENT.post(checkEvent))
                        return;
                    FlagEvaluator.processCheck(checkEvent, deny -> {
                        sendFlagMsg(deny);
                        ci.cancel();
                    });
                }
                if (target instanceof WanderingTrader) {
                    FlagCheckEvent checkEvent = new FlagCheckEvent(target.blockPosition(), MELEE_WANDERING_TRADER, getDimKey(player), player);
                    if (Services.EVENT.post(checkEvent))
                        return;
                    FlagEvaluator.processCheck(checkEvent, deny -> {
                        sendFlagMsg(deny);
                        ci.cancel();
                    });
                }

                // check every other entity if it is in the list of entities to protect
                // this is for BlockEntities which are not covered by the block breaking flag
                Set<String> entityTags = FlagConfig.getCoveredBlockEntityTags();
                boolean isCoveredByTag = entityTags.stream().anyMatch(entityTag -> {
                    ResourceLocation tagRl = ResourceLocation.parse(entityTag);
                    return target.getTags().contains(tagRl.getPath());
                });
                Set<String> entities = FlagConfig.getCoveredBlockEntities();
                boolean isBlockEntityCovered = entities.stream().anyMatch(entity -> {
                    ResourceLocation entityRl = ResourceLocation.parse(entity);
                    ResourceLocation targetRl = EntityType.getKey(target.getType());
                    return targetRl != null && targetRl.equals(entityRl);
                });
                if (isBlockEntityCovered || isCoveredByTag) {
                    FlagCheckEvent checkEvent = new FlagCheckEvent(target.blockPosition(), BREAK_BLOCKS, getDimKey(player), player);
                    if (Services.EVENT.post(checkEvent))
                        return;
                    FlagEvaluator.processCheck(checkEvent, null, onDeny -> {
                        ci.cancel();
                        sendFlagMsg(onDeny);
                    });
                }
            }
        }
    }

    @Inject(method = "tick", at = @At(value = "TAIL"), allow = 1)
    private void onUseElytraTick(CallbackInfo ci) {
        Player player = (Player) (Object) this;
        if (isServerSide(player)) {
            if (player.isFallFlying()) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(player.blockPosition(), NO_FLIGHT, getDimKey(player), player);
                if (Services.EVENT.post(checkEvent))
                    return;
                FlagEvaluator.processCheck(checkEvent, deny -> {
                    sendFlagMsg(deny);
                    player.stopFallFlying();
                });
            }
        }
    }
}
