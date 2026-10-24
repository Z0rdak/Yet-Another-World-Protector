package de.z0rdak.yawp.mixin;


import com.llamalad7.mixinextras.injector.wrapoperation.Operation;
import com.llamalad7.mixinextras.injector.wrapoperation.WrapOperation;

import de.z0rdak.yawp.core.flag.PortalTransferFlags;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.level.portal.TeleportTransition;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;

/**
 * Adds portal-transfer context around the entity's portal-triggered teleportation.
 *
 * <p>This allows downstream teleport checks to distinguish portal transfers
 * from other dimension changes, such as those initiated by teleport commands,
 * even though both use the same underlying teleport method.</p>
 */
@Mixin(Entity.class)
public abstract class EntityMixin {

    /**
     * Wraps the teleport call made by {@code Entity.handlePortal()}.
     *
     * <p>For server players, marks the teleport as portal-triggered while the
     * original operation executes. The previous context is restored afterward,
     * including when the operation throws an exception.</p>
     *
     * <p>Non-player entities are passed through without changing the context.</p>
     */
    @WrapOperation(method = "handlePortal", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/entity/Entity;teleport(Lnet/minecraft/world/level/portal/TeleportTransition;)Lnet/minecraft/world/entity/Entity;"))
    private Entity markPortalTransfer(Entity entity, TeleportTransition transition, Operation<Entity> original) {
        if (!(entity instanceof ServerPlayer player)) {
            return original.call(entity, transition);
        }
        ServerPlayer previous = PortalTransferFlags.enter(player);
        try {
            return original.call(entity, transition);
        } finally {
            PortalTransferFlags.exit(previous);
        }
    }
}