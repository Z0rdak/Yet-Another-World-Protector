package de.z0rdak.yawp.mixin.flag;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LightningBolt;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.entity.vehicle.AbstractMinecart;
import net.minecraft.world.item.trading.Merchant;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.api.events.region.RegionEvents.post;
import static de.z0rdak.yawp.core.flag.RegionFlag.*;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;
import static de.z0rdak.yawp.util.text.MessageSender.sendFlagMsg;

@Mixin(Entity.class)
public abstract class EntityMixin {

    @Inject(method = "startRiding(Lnet/minecraft/world/entity/Entity;Z)Z", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void spawnChildFromBreeding(Entity vehicle, boolean force, CallbackInfoReturnable<Boolean> cir) {
        Entity rider = (Entity) (Object) this;
        if (isServerSide(rider)) {
            if (rider instanceof Player player) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(vehicle.blockPosition(), ANIMAL_MOUNTING, getDimKey(vehicle), player);
                if (post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, null, deny -> {
                    sendFlagMsg(deny);
                    cir.setReturnValue(false);
                });
            }
        }
    }

    @Inject(method = "thunderHit", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onHitByLightning(ServerLevel world, LightningBolt lightning, CallbackInfo ci) {
        Entity poorSoul = (Entity) (Object) this;
        if (isServerSide(poorSoul)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(poorSoul.blockPosition(), LIGHTNING_PROT, getDimKey(poorSoul), null);
            if (post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, null, deny -> {
                ci.cancel();
            });
        }
    }

    @Inject(method = "stopRiding", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void spawnChildFromBreeding(CallbackInfo ci) {
        Entity rider = (Entity) (Object) this;
        if (isServerSide(rider)) {
            if (rider instanceof Player player) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(player.blockPosition(), ANIMAL_UNMOUNTING, getDimKey(player), player);
                if (post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, null, deny -> {
                    sendFlagMsg(deny);
                    ci.cancel();
                });
            }
        }
    }

    /**
     * Covers USE_PORTAL* flags
     * Note: does not seem to trigger for players, which is fine
     */
    @Inject(method = "changeDimension", at = @At(value = "HEAD"), cancellable = true, allow = 1)
    public void onChangeDimension(ServerLevel destination, CallbackInfoReturnable<Entity> cir) {
        Entity self = (Entity) (Object) this;
        if (isServerSide(self.level())) {
            RegionDataManager.onPlayerChangeWorldAddDimKey(null, (ServerLevel) self.level(), destination);
            FlagCheckEvent checkEvent = new FlagCheckEvent(self.blockPosition(), USE_PORTAL, getDimKey(self), null);
            if (post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, null, deny -> {
                cir.setReturnValue(null);
            });
            if (self instanceof Player player) {
                checkEvent = new FlagCheckEvent(self.blockPosition(), USE_PORTAL_PLAYERS, getDimKey(self), player);
                if (post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, null, deny -> {
                    sendFlagMsg(deny);
                    cir.setReturnValue(null);
                });
            }
            if (self instanceof ItemEntity) {
                checkEvent = new FlagCheckEvent(self.blockPosition(), USE_PORTAL_ITEMS, getDimKey(self), null);
                if (post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, null, deny -> {
                    cir.setReturnValue(null);
                });
            }
            if (isAnimal(self)) {
                checkEvent = new FlagCheckEvent(self.blockPosition(), USE_PORTAL_ANIMALS, getDimKey(self), null);
                if (post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, null, deny -> {
                    cir.setReturnValue(null);
                });
            }
            if (isMonster(self)) {
                checkEvent = new FlagCheckEvent(self.blockPosition(), USE_PORTAL_MONSTERS, getDimKey(self), null);
                if (post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, null, deny -> {
                    cir.setReturnValue(null);
                });
            }
            if (self instanceof Merchant) {
                checkEvent = new FlagCheckEvent(self.blockPosition(), USE_PORTAL_VILLAGERS, getDimKey(self), null);
                if (post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, null, deny -> {
                    cir.setReturnValue(null);
                });
            }
            if (self instanceof AbstractMinecart) {
                checkEvent = new FlagCheckEvent(self.blockPosition(), USE_PORTAL_MINECARTS, getDimKey(self), null);
                if (post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, null, deny -> {
                    cir.setReturnValue(null);
                });
            }
        }
    }
}
