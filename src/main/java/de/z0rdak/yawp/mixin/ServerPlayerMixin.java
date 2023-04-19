package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.handler.flags.FlagCheckEvent;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.entity.Entity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.server.network.ServerPlayerEntity;
import net.minecraft.server.world.ServerWorld;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.core.flag.RegionFlag.ENTER_DIM;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;

@Mixin(ServerPlayerEntity.class)
public abstract class ServerPlayerMixin {

    @Inject(method = "dropSelectedItem", at = @At(value = "TAIL"), allow = 1, cancellable = true)
    private void onDropItem(boolean entireStack, CallbackInfoReturnable<Boolean> cir) {
        ServerPlayerEntity player = (ServerPlayerEntity) (Object) this;
        if (isServerSide(player)) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(player));
            FlagCheckEvent.PlayerFlagEvent flagCheck = checkPlayerEvent(player, player.getBlockPos(), RegionFlag.ITEM_DROP, dimCache.getDimensionalRegion());
            if (flagCheck.isDenied()) {
                sendFlagDeniedMsg(flagCheck);
                cir.setReturnValue(false);
            }
        }
    }

    @Inject(method = "moveToWorld", at = @At(value = "HEAD"), allow = 1, cancellable = true)
    private void onChangeDimension(ServerWorld destination, CallbackInfoReturnable<Entity> cir) {
        PlayerEntity player = (PlayerEntity) (Object) this;
        if (isServerSide(player)) {
            RegionDataManager.onPlayerChangeWorldAddDimKey(player, (ServerWorld) player.world, destination);
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(player));
            DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
            FlagCheckEvent.PlayerFlagEvent flagCheck = checkPlayerEvent(player, player.getBlockPos(), RegionFlag.USE_PORTAL_PLAYERS, dimCache.getDimensionalRegion());
            if (flagCheck.isDenied()) {
                sendFlagDeniedMsg(flagCheck);
                cir.setReturnValue(null);
            }
            /*
                FIXME: Get target position correctly - until then flag only works for dimension
                WorldBorder worldborder = targetServerLevel.getWorldBorder();
                double tpPosScale = DimensionType.getTeleportationScale(player.level.dimensionType(), targetServerLevel.dimensionType());
                BlockPos targetPos = worldborder.clampToBounds(player.getX() * tpPosScale, player.getY(), player.getZ() * tpPosScale);
             */
            FlagCheckEvent.PlayerFlagEvent playerFlagCheckEvent = new FlagCheckEvent.PlayerFlagEvent(player, dimRegion, null, ENTER_DIM);
            playerFlagCheckEvent.setDeniedLocal(false);
            if (dimRegion.isActive()) {
                if (dimRegion.containsFlag(ENTER_DIM) && !dimRegion.permits(player)) {
                    IFlag flag = dimRegion.getFlag(ENTER_DIM.name);
                    // TODO: Check state with allowed
                    playerFlagCheckEvent.setDeniedInDim(flag.isActive());
                } else {
                    playerFlagCheckEvent.setDeniedInDim(false);
                }
            } else {
                playerFlagCheckEvent.setDeniedInDim(false);
            }
            playerFlagCheckEvent.setDenied(playerFlagCheckEvent.isDeniedInDim());
            if (playerFlagCheckEvent.isDenied()) {
                sendFlagDeniedMsg(playerFlagCheckEvent);
                cir.setReturnValue(null);
            }


        }
    }

    @Inject(method = "teleport", at = @At(value = "INVOKE", target = "Lnet/minecraft/server/network/ServerPlayerEntity;getWorld()Lnet/minecraft/server/world/ServerWorld;"), allow = 1, cancellable = true)
    private void onTeleportToDimension(ServerWorld destination, double x, double y, double z, float yaw, float pitch, CallbackInfo ci) {
        PlayerEntity player = (PlayerEntity) (Object) this;
        if (isServerSide(player)) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(player));
            FlagCheckEvent.PlayerFlagEvent flagCheck = checkPlayerEvent(player, player.getBlockPos(), RegionFlag.USE_PORTAL_PLAYERS, dimCache.getDimensionalRegion());
            if (flagCheck.isDenied()) {
                sendFlagDeniedMsg(flagCheck);
                ci.cancel();
            }
        }
    }
}
