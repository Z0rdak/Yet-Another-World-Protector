package de.z0rdak.yawp.mixin;

import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.*;
import net.minecraft.world.level.Level;
import net.minecraftforge.common.ForgeHooks;
import org.spongepowered.asm.mixin.Mixin;

import javax.annotation.Nullable;

@Mixin(AgeableMob.class)
public abstract class MixinAgeableEntity extends PathfinderMob {

    protected MixinAgeableEntity(EntityType<? extends PathfinderMob> type, Level worldIn) {
        super(type, worldIn);
    }

    @Nullable
    @Override
    public Entity changeDimension(ServerLevel server) {
        if (!ForgeHooks.onTravelToDimension(this, server.dimension())) {
            return null;
        }
        return super.changeDimension(server);
    }
}
