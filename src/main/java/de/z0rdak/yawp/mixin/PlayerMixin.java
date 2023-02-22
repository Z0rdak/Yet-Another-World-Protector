package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.config.server.FlagConfig;
import de.z0rdak.yawp.handler.flags.FlagCheckEvent;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityType;
import net.minecraft.entity.passive.VillagerEntity;
import net.minecraft.entity.passive.WanderingTraderEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.util.Identifier;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import java.util.List;

import static de.z0rdak.yawp.core.flag.RegionFlag.*;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;

@Mixin(PlayerEntity.class)
public abstract class PlayerMixin {

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
            List<? extends String> entities = FlagConfig.BREAK_FLAG_ENTITIES.get();
            boolean isBlockEntityCovered = entities.stream()
                    .anyMatch(entity -> EntityType.getId(target.getType()).equals(new Identifier(entity)));
            if (isBlockEntityCovered) {
                FlagCheckEvent.PlayerFlagEvent flagCheck = checkPlayerEvent(player, target.getBlockPos(), BREAK_ENTITIES, dimCache.getDimensionalRegion());
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
