package de.z0rdak.yawp.mixin;

import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.FlyingMob;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.level.Level;
import net.minecraftforge.common.ForgeHooks;
import org.spongepowered.asm.mixin.Mixin;

import javax.annotation.Nullable;

@Mixin(FlyingMob.class)
public abstract class MixinFlyingEntity extends Mob {

    protected MixinFlyingEntity(EntityType<? extends Mob> type, Level worldIn) {
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
