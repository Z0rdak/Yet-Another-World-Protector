package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.handler.flags.FlagCheckEvent;
import de.z0rdak.yawp.handler.flags.HandlerUtil;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.entity.EntityType;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.core.flag.RegionFlag.USE_ELYTRA;

@Mixin({PlayerEntity.class})
abstract class PlayerEntityMixin extends LivingEntity {
    PlayerEntityMixin(EntityType<? extends LivingEntity> entityType, World world) {
        super(entityType, world);
        throw new AssertionError();
    }

    @Inject(at = @At(value = "FIELD", target = "Lnet/minecraft/inventory/EquipmentSlotType;CHEST:Lnet/minecraft/inventory/EquipmentSlotType;"), method = "tryToStartFallFlying()Z", allow = 1, cancellable = true)
    void injectElytraCheck(CallbackInfoReturnable<Boolean> cir) {
        if (!level.isClientSide) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(level.dimension());
            FlagCheckEvent flagCheckEvent = HandlerUtil.checkTargetEvent(this.blockPosition(), USE_ELYTRA, dimCache.getDimensionalRegion());
            if (flagCheckEvent.isDenied()) {
                cir.setReturnValue(false);
            }
        }
    }
}