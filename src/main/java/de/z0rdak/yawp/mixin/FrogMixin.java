package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.handler.flags.FlagCheckEvent;
import de.z0rdak.yawp.handler.flags.HandlerUtil;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.entity.passive.AnimalEntity;
import net.minecraft.entity.passive.FrogEntity;
import net.minecraft.server.world.ServerWorld;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(FrogEntity.class)
public abstract class FrogMixin {

    @Inject(method = "breed", at = @At("HEAD"), cancellable = true, allow = 1)
    public void spawnChildFromBreeding(ServerWorld world, AnimalEntity parentB, CallbackInfo ci) {
        AnimalEntity parentA = (AnimalEntity) (Object) this;
        DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(world.getRegistryKey());
        FlagCheckEvent flagCheck = HandlerUtil.checkTargetEvent(parentA.getBlockPos(), RegionFlag.ANIMAL_BREEDING, dimCache.getDimensionalRegion());
        if (flagCheck.isDenied()) {
            parentA.setBreedingAge(6000);
            parentB.setBreedingAge(6000);
            parentA.resetLoveTicks();
            parentB.resetLoveTicks();
            ci.cancel();
        }
    }
}
