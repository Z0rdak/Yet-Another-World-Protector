package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.config.server.FlagConfig;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.handler.flags.FlagCheckEvent;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
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

import static de.z0rdak.yawp.core.flag.RegionFlag.*;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;

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
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(player));
            FlagCheckEvent.PlayerFlagEvent flagCheck = checkPlayerEvent(player, player.getBlockPos(), RegionFlag.ITEM_DROP, dimCache.getDimensionalRegion());
            if (flagCheck.isDenied()) {
                sendFlagDeniedMsg(flagCheck);
                cir.setReturnValue(null);
            }
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
        PlayerEntity target = (PlayerEntity) (Object) this;
        if (isServerSide(target)) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(target));
            FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(target, target.getBlockPos(), LEVEL_FREEZE, dimCache.getDimensionalRegion());
            if (flagCheckEvent.isDenied()) {
                sendFlagDeniedMsg(flagCheckEvent);
                ci.cancel();
            }
        }
    }

    /**
     * TODO: Flag KEEP_INVENTORY
     *
     * @param ci
     */
    @Inject(method = "dropInventory", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onDropInventory(CallbackInfo ci) {
        PlayerEntity target = (PlayerEntity) (Object) this;
        if (isServerSide(target)) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(target));
            // FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkTargetEvent(target.getBlockPos(), , dimCache.getDimensionalRegion());
            //if (flagCheckEvent.isDenied()) {
            //    sendFlagDeniedMsg(flagCheckEvent);
            //    ci.cancel();
            //}
        }
    }

    /**
     * XP_FREEZE
     *
     * @param experience
     * @param ci
     */
    @Inject(method = "addExperience", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onGainExperience(int experience, CallbackInfo ci) {
        PlayerEntity target = (PlayerEntity) (Object) this;
        if (isServerSide(target)) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(target));
            FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(target, target.getBlockPos(), XP_FREEZE, dimCache.getDimensionalRegion());
            if (flagCheckEvent.isDenied()) {
                sendFlagDeniedMsg(flagCheckEvent);
                ci.cancel();
            }
        }
    }

    /**
     * no-hunger
     *
     * @param exhaustion
     * @param ci
     */
    @Inject(method = "addExhaustion", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onGainHunger(float exhaustion, CallbackInfo ci) {

    }


    @Inject(method = "applyDamage", at = @At(value = "INVOKE", target = "Lnet/minecraft/entity/player/PlayerEntity;applyArmorToDamage(Lnet/minecraft/entity/damage/DamageSource;F)F"), cancellable = true, allow = 1)
    public void onHurt(DamageSource source, float amount, CallbackInfo ci) {
        PlayerEntity target = (PlayerEntity) (Object) this;
        if (isServerSide(target)) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(target));
            if (source.getAttacker() instanceof PlayerEntity attacker) {
                FlagCheckEvent flagCheckEvent = checkTargetEvent(target.getBlockPos(), NO_PVP, dimCache.getDimensionalRegion());
                if (flagCheckEvent.isDenied()) {
                    sendFlagDeniedMsg(flagCheckEvent, attacker);
                    ci.cancel();
                }
            }
            FlagCheckEvent flagCheckEvent = checkTargetEvent(target.getBlockPos(), INVINCIBLE, dimCache.getDimensionalRegion());
            if (flagCheckEvent.isDenied()) {
                ci.cancel();
            }
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
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(player));
            // player attacking other player
            if (target instanceof PlayerEntity) {
                FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, target.getBlockPos(), MELEE_PLAYERS, dimCache.getDimensionalRegion());
                handleAndSendMsg(flagCheckEvent);
                if (flagCheckEvent.isDenied()) {
                    ci.cancel();
                }
            }
            // player attacking other entities
            if (isAnimal(target)) {
                FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, target.getBlockPos(), MELEE_ANIMALS, dimCache.getDimensionalRegion());
                handleAndSendMsg(flagCheckEvent);
                if (flagCheckEvent.isDenied()) {
                    ci.cancel();
                }
            }
            if (isMonster(target)) {
                FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, target.getBlockPos(), MELEE_MONSTERS, dimCache.getDimensionalRegion());
                handleAndSendMsg(flagCheckEvent);
                if (flagCheckEvent.isDenied()) {
                    ci.cancel();
                }
            }
            if (target instanceof VillagerEntity) {
                FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, target.getBlockPos(), MELEE_VILLAGERS, dimCache.getDimensionalRegion());
                handleAndSendMsg(flagCheckEvent);
                if (flagCheckEvent.isDenied()) {
                    ci.cancel();
                }
            }
            if (target instanceof WanderingTraderEntity) {
                FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, target.getBlockPos(), MELEE_WANDERING_TRADER, dimCache.getDimensionalRegion());
                handleAndSendMsg(flagCheckEvent);
                if (flagCheckEvent.isDenied()) {
                    ci.cancel();
                }
            }

            // check every other entity if it is in the list of entities to protect
            // this is for BlockEntities which are not covered by the block breaking flag
            // FIXME: Tags are not yet considered
            Set<String> entityTags = FlagConfig.getBreakFlagEntityTags();
            Set<String> entities = FlagConfig.getBreakFlagEntities();
            boolean isBlockEntityCovered = entities.stream()
                    .anyMatch(entity -> EntityType.getId(target.getType()).equals(Identifier.of(entity)));
            if (isBlockEntityCovered) {
                FlagCheckEvent.PlayerFlagEvent flagCheck = checkPlayerEvent(player, target.getBlockPos(), BREAK_BLOCKS, dimCache.getDimensionalRegion());
                handleAndSendMsg(flagCheck);
                if (flagCheck.isDenied()) {
                    ci.cancel();
                }
            }
        }
    }

    @Inject(method = "tick", at = @At(value = "TAIL"), allow = 1)
    private void onUseElytraTick(CallbackInfo ci) {
        PlayerEntity player = (PlayerEntity) (Object) this;
        if (isServerSide(player)) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(player));
            if (player.isFallFlying()) {
                FlagCheckEvent.PlayerFlagEvent flagCheckEvent = checkPlayerEvent(player, player.getBlockPos(), NO_FLIGHT, dimCache.getDimensionalRegion());
                if (flagCheckEvent.isDenied()) {
                    handleAndSendMsg(flagCheckEvent);
                    player.stopFallFlying();
                }
            }

        }
    }
}
