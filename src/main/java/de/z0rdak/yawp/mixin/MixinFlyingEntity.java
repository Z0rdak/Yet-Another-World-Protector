package de.z0rdak.yawp.mixin;

import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityType;
import net.minecraft.entity.FlyingEntity;
import net.minecraft.entity.MobEntity;
import net.minecraft.world.World;
import net.minecraft.world.server.ServerWorld;
import net.minecraftforge.common.ForgeHooks;
import org.spongepowered.asm.mixin.Mixin;

import javax.annotation.Nullable;

@Mixin(FlyingEntity.class)
public abstract class MixinFlyingEntity extends MobEntity {

    protected MixinFlyingEntity(EntityType<? extends MobEntity> type, World worldIn) {
        super(type, worldIn);
    }

    @Nullable
    @Override
    public Entity changeDimension(ServerWorld server) {
        if (!server.isClientSide) {
            if (!ForgeHooks.onTravelToDimension(this, server.dimension())) {
                return null;
            }
            return super.changeDimension(server);
        }
        return super.changeDimension(server);
    }
}
