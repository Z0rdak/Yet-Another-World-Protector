package de.z0rdak.yawp.mixin.flag.player;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.config.server.FlagConfig;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityType;
import net.minecraft.entity.damage.DamageSource;
import net.minecraft.entity.passive.VillagerEntity;
import net.minecraft.entity.passive.WanderingTraderEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.util.Identifier;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import java.util.Set;

import static de.z0rdak.yawp.api.events.region.RegionEvents.post;
import static de.z0rdak.yawp.core.flag.RegionFlag.*;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;
import static de.z0rdak.yawp.util.MessageSender.sendFlagMsg;

@Mixin(PlayerEntity.class)
public abstract class PlayerMixin {

    /**
     * ITEM_DROP
     *
     * @param stack
     * @param retainOwnership
     * @param cir
     */
    @Inject(method = "dropItem(Lnet/minecraft/item/ItemStack;Z)Lnet/minecraft/entity/ItemEntity;", at = @At(value = "TAIL"), allow = 1, cancellable = true)
    private void onDropItem(ItemStack stack, boolean retainOwnership, CallbackInfoReturnable<ItemStack> cir) {
        PlayerEntity player = (PlayerEntity) (Object) this;
        if (isServerSide(player)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(player.getBlockPos(), ITEM_DROP, getDimKey(player), player);
            if (post(checkEvent))
                return;
            processCheck(checkEvent, null, deny -> {
                sendFlagMsg(deny);
                cir.setReturnValue(null);
            });
        }
    }

    /**
     * LEVEL_FREEZE
     *
     * @param levels
     * @param ci
     */
    @Inject(method = "addExperienceLevels", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onGainLevels(int levels, CallbackInfo ci) {
        PlayerEntity player = (PlayerEntity) (Object) this;
        if (isServerSide(player)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(player.getBlockPos(), LEVEL_FREEZE, getDimKey(player), player);
            if (post(checkEvent))
                return;
            processCheck(checkEvent, null, deny -> {
                sendFlagMsg(deny);
                ci.cancel();
            });
        }
    }

    /**
     * TODO: add keep-inventory flag enum
     * As seen below this is already implemented. It just need to be tested and 
     * @param ci
     */
    @Inject(method = "dropInventory", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onDropInventory(CallbackInfo ci) {
        PlayerEntity player = (PlayerEntity) (Object) this;
        /*
        if (isServerSide(player)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(player.getBlockPos(), KEEP_INVENTORY, getEntityDim(player), player);
            if (post(checkEvent))
                return;
            processCheck(checkEvent, null, deny -> {
                sendFlagMsg(deny);
                ci.cancel();
            });
        }
        */
    }

    /**
     * XP_FREEZE
     *
     * @param experience
     * @param ci
     */
    @Inject(method = "addExperience", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onGainExperience(int experience, CallbackInfo ci) {
        PlayerEntity player = (PlayerEntity) (Object) this;
        if (isServerSide(player)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(player.getBlockPos(), XP_FREEZE, getDimKey(player), player);
            if (post(checkEvent))
                return;
            processCheck(checkEvent, null, deny -> {
                sendFlagMsg(deny);
                ci.cancel();
            });
        }
    }

    /**
     * TODO: add no-hunger flag enum
     * As seen below this is already implemented. It just need to be tested and 
     * @param exhaustion
     * @param ci
     */
    @Inject(method = "addExhaustion", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onGainHunger(float exhaustion, CallbackInfo ci) {
        PlayerEntity player = (PlayerEntity) (Object) this;
        if (isServerSide(player)) {
            /*
            FlagCheckEvent checkEvent = new FlagCheckEvent(player.getBlockPos(), NO_HUNGER, getEntityDim(player), player);
            if (post(checkEvent))
                return;
            processCheck(checkEvent, null, deny -> {
                sendFlagMsg(deny);
                ci.cancel();
            });          
             */
        }
    }


    @Inject(method = "applyDamage", at = @At(value = "INVOKE", target = "Lnet/minecraft/entity/player/PlayerEntity;applyArmorToDamage(Lnet/minecraft/entity/damage/DamageSource;F)F"), cancellable = true, allow = 1)
    public void onHurt(DamageSource source, float amount, CallbackInfo ci) {
        PlayerEntity player = (PlayerEntity) (Object) this;
        if (isServerSide(player)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(player.getBlockPos(), NO_PVP, getDimKey(player), player);
            if (post(checkEvent))
                return;
            processCheck(checkEvent, null, deny -> {
                sendFlagMsg(deny);
                ci.cancel();
            });
            checkEvent = new FlagCheckEvent(player.getBlockPos(), INVINCIBLE, getDimKey(player), player);
            if (post(checkEvent))
                return;
            processCheck(checkEvent, null, deny -> {
                ci.cancel();
            });
        }
    }

    @Inject(method = "applyDamage", at = @At(value = "INVOKE", target = "Lnet/minecraft/entity/player/PlayerEntity;setAbsorptionAmount(F)V"), cancellable = true, allow = 1)
    public void onReceiveDamage(DamageSource source, float amount, CallbackInfo ci) {
        PlayerEntity player = (PlayerEntity) (Object) this;
        if (isServerSide(player)) {
            // TODO: meele-player flag
        }
    }


    @Inject(method = "attack", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onAttackEntity(Entity target, CallbackInfo ci) {
        if (isServerSide(target)) {
            PlayerEntity player = (PlayerEntity) (Object) this;
            if (target instanceof PlayerEntity) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(player.getBlockPos(), MELEE_PLAYERS, getDimKey(player), player);
                if (post(checkEvent))
                    return;
                processCheck(checkEvent, null, deny -> {
                    sendFlagMsg(deny);
                    ci.cancel();
                });
            } else {
                if (isAnimal(target)) {
                    FlagCheckEvent checkEvent = new FlagCheckEvent(player.getBlockPos(), MELEE_ANIMALS, getDimKey(player), player);
                    if (post(checkEvent))
                        return;
                    processCheck(checkEvent, null, deny -> {
                        sendFlagMsg(deny);
                        ci.cancel();
                    });
                }
                if (isMonster(target)) {
                    FlagCheckEvent checkEvent = new FlagCheckEvent(player.getBlockPos(), MELEE_MONSTERS, getDimKey(player), player);
                    if (post(checkEvent))
                        return;
                    processCheck(checkEvent, null, deny -> {
                        sendFlagMsg(deny);
                        ci.cancel();
                    });
                }
                if (target instanceof VillagerEntity) {
                    FlagCheckEvent checkEvent = new FlagCheckEvent(player.getBlockPos(), MELEE_VILLAGERS, getDimKey(player), player);
                    if (post(checkEvent))
                        return;
                    processCheck(checkEvent, null, deny -> {
                        sendFlagMsg(deny);
                        ci.cancel();
                    });
                }
                if (target instanceof WanderingTraderEntity) {
                    FlagCheckEvent checkEvent = new FlagCheckEvent(player.getBlockPos(), MELEE_WANDERING_TRADER, getDimKey(player), player);
                    if (post(checkEvent))
                        return;
                    processCheck(checkEvent, null, deny -> {
                        sendFlagMsg(deny);
                        ci.cancel();
                    });
                }

                // check every other entity if it is in the list of entities to protect
                // this is for BlockEntities which are not covered by the block breaking flag
                // FIXME: Tags are not yet considered
                Set<String> entityTags = FlagConfig.getCoveredBlockEntityTags();
                Set<String> entities = FlagConfig.getCoveredBlockEntities();
                boolean isBlockEntityCovered = entities.stream()
                        .anyMatch(entity -> EntityType.getId(target.getType()).equals(new Identifier(entity)));
                if (isBlockEntityCovered) {
                    FlagCheckEvent checkEvent = new FlagCheckEvent(player.getBlockPos(), BREAK_BLOCKS, getDimKey(player), player);
                    if (post(checkEvent))
                        return;
                    processCheck(checkEvent, null, deny -> {
                        sendFlagMsg(deny);
                        ci.cancel();
                    });
                }
            }
        }
    }

    @Inject(method = "tick", at = @At(value = "TAIL"), allow = 1)
    private void onUseElytraTick(CallbackInfo ci) {
        PlayerEntity player = (PlayerEntity) (Object) this;
        if (isServerSide(player)) {
            if (player.isFallFlying()) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(player.getBlockPos(), NO_FLIGHT, getDimKey(player), player);
                if (post(checkEvent))
                    return;
                processCheck(checkEvent, null, deny -> {
                    sendFlagMsg(deny);
                    player.stopFallFlying();
                });
            }
        }
    }
}
