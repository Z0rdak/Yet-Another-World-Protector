package de.z0rdak.yawp.mixin.flag.player.breeding;


import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.animal.Animal;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import static de.z0rdak.yawp.api.events.region.FabricRegionEvents.post;
import static de.z0rdak.yawp.core.flag.RegionFlag.ANIMAL_BREEDING;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.isServerSide;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.processCheck;

@Mixin(Animal.class)
public abstract class AnimalMixin {

    @Inject(method = "spawnChildFromBreeding", at = @At("HEAD"), cancellable = true, allow = 1)
    public void spawnChildFromBreeding(ServerLevel world, Animal parentB, CallbackInfo ci) {
        if (isServerSide(world)) {
            Animal parentA = (Animal) (Object) this;
            FlagCheckEvent checkEvent = new FlagCheckEvent(parentA.blockPosition(), ANIMAL_BREEDING, world.dimension(), null);
            if (post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, null, deny -> {
                parentA.setAge(6000);
                parentB.setAge(6000);
                parentA.resetLove();
                parentB.resetLove();
                ci.cancel();
            });
        }
    }
}
