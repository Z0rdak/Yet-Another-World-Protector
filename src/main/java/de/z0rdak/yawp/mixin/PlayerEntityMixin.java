package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.api.events.region.FlagCheckResult;
import de.z0rdak.yawp.handler.flags.HandlerUtil;
import net.minecraft.entity.EntityType;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.world.World;
import net.minecraftforge.common.MinecraftForge;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.core.flag.RegionFlag.USE_ELYTRA;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.handleAndSendMsg;

@Mixin({PlayerEntity.class})
public abstract class PlayerEntityMixin extends LivingEntity {

    PlayerEntityMixin(EntityType<? extends LivingEntity> entityType, World world) {
        super(entityType, world);
        throw new AssertionError();
    }

    @Inject(at = @At(value = "FIELD", target = "Lnet/minecraft/inventory/EquipmentSlotType;CHEST:Lnet/minecraft/inventory/EquipmentSlotType;"), method = "tryToStartFallFlying()Z", allow = 1, cancellable = true)
    void injectElytraCheck(CallbackInfoReturnable<Boolean> cir) {
        if (!level.isClientSide) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(this.blockPosition(), USE_ELYTRA, this.level.dimension(), null);
            if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                return;
            }
            FlagCheckResult result = HandlerUtil.evaluate(checkEvent);
            MinecraftForge.EVENT_BUS.post(result);
            handleAndSendMsg(result, null, denyResult -> {
                cir.setReturnValue(false);
            });
        }
    }
}