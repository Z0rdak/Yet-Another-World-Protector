package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.handler.flags.FlagCheckEvent;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.entity.Entity;
import net.minecraft.entity.ItemEntity;
import net.minecraft.entity.passive.MerchantEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.vehicle.AbstractMinecartEntity;
import net.minecraft.server.world.ServerWorld;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.core.flag.RegionFlag.*;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;

@Mixin(Entity.class)
public abstract class EntityMixin {

    @Inject(method = "startRiding(Lnet/minecraft/entity/Entity;Z)Z", at = @At(value = "INVOKE", target = "Lnet/minecraft/entity/Entity;canStartRiding(Lnet/minecraft/entity/Entity;)Z"), cancellable = true, allow = 1)
    public void spawnChildFromBreeding(Entity vehicle, boolean force, CallbackInfoReturnable<Boolean> cir) {
        Entity rider = (Entity) (Object) this;
        if (!rider.world.isClient) {
            if (rider instanceof PlayerEntity player) {
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(vehicle));
                FlagCheckEvent.PlayerFlagEvent flagCheck = checkPlayerEvent(player, vehicle.getBlockPos(), RegionFlag.ANIMAL_MOUNTING, dimCache.getDimensionalRegion());
                if (flagCheck.isDenied()) {
                    sendFlagDeniedMsg(flagCheck);
                    cir.setReturnValue(false);
                }
            }
        }
    }

    @Inject(method = "dismountVehicle", at = @At(value = "FIELD", target = "Lnet/minecraft/entity/Entity;vehicle:Lnet/minecraft/entity/Entity;", ordinal = 2), cancellable = true, allow = 1)
    public void spawnChildFromBreeding(CallbackInfo ci) {
        Entity rider = (Entity) (Object) this;
        if (!rider.world.isClient) {
            if (rider instanceof PlayerEntity player) {
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(rider));
                FlagCheckEvent.PlayerFlagEvent flagCheck = checkPlayerEvent(player, player.getBlockPos(), RegionFlag.ANIMAL_UNMOUNTING, dimCache.getDimensionalRegion());
                if (flagCheck.isDenied()) {
                    sendFlagDeniedMsg(flagCheck);
                    ci.cancel();
                }
            }
        }
    }

    @Inject(method = "moveToWorld", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onChangeDimension(ServerWorld destination, CallbackInfoReturnable<Entity> cir) {
        Entity self = (Entity) (Object) this;
        if (!self.getWorld().isClient) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(self));
            DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
            // Note: does not seem to trigger for players, which is fine
            FlagCheckEvent flagCheckEvent = checkTargetEvent(self.getBlockPos(), USE_PORTAL, dimRegion);
            if (flagCheckEvent.isDenied()) {
                cir.setReturnValue(null);
                return;
            }
            if (self instanceof PlayerEntity player) {
                FlagCheckEvent.PlayerFlagEvent flagCheck = checkPlayerEvent(player, player.getBlockPos(), RegionFlag.USE_PORTAL_PLAYERS, dimCache.getDimensionalRegion());
                if (flagCheck.isDenied()) {
                    sendFlagDeniedMsg(flagCheck);
                    cir.setReturnValue(null);
                    return;
                }
            }
            if (self instanceof ItemEntity) {
                flagCheckEvent = checkTargetEvent(self.getBlockPos(), USE_PORTAL_ITEMS, dimRegion);
                if (flagCheckEvent.isDenied()) {
                    cir.setReturnValue(null);
                    return;
                }
            }
            if (isAnimal(self)) {
                flagCheckEvent = checkTargetEvent(self.getBlockPos(), USE_PORTAL_ANIMALS, dimRegion);
                if (flagCheckEvent.isDenied()) {
                    cir.setReturnValue(null);
                    return;
                }
            }
            if (isMonster(self)) {
                flagCheckEvent = checkTargetEvent(self.getBlockPos(), USE_PORTAL_MONSTERS, dimRegion);
                if (flagCheckEvent.isDenied()) {
                    cir.setReturnValue(null);
                    return;
                }
            }
            if (self instanceof MerchantEntity) {
                flagCheckEvent = checkTargetEvent(self.getBlockPos(), USE_PORTAL_VILLAGERS, dimRegion);
                if (flagCheckEvent.isDenied()) {
                    cir.setReturnValue(null);
                    return;
                }
            }
            if (self instanceof AbstractMinecartEntity) {
                flagCheckEvent = checkTargetEvent(self.getBlockPos(), USE_PORTAL_MINECARTS, dimRegion);
                if (flagCheckEvent.isDenied()) {
                    cir.setReturnValue(null);
                }
            }
        }

    }
}
