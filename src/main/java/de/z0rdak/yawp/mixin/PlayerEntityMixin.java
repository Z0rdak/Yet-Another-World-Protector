package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.handler.flags.HandlerUtil;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.minecraftforge.common.MinecraftForge;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.core.flag.RegionFlag.USE_ELYTRA;

@Mixin({Player.class})
public abstract class PlayerEntityMixin extends LivingEntity {

    PlayerEntityMixin(EntityType<? extends LivingEntity> entityType, Level world) {
        super(entityType, world);
        throw new AssertionError();
    }

    @Inject(at = @At(value = "FIELD", target = "Lnet/minecraft/world/entity/EquipmentSlot;CHEST:Lnet/minecraft/world/entity/EquipmentSlot;"), method = "tryToStartFallFlying()Z", allow = 1, cancellable = true)
    void injectElytraCheck(CallbackInfoReturnable<Boolean> cir) {
        if (!level().isClientSide) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(this.blockPosition(), USE_ELYTRA, this.level().dimension(), null);
            if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                return;
            }
            HandlerUtil.processCheck(checkEvent, null, denyResult -> {
                cir.setReturnValue(false);
            });
        }
    }
}
