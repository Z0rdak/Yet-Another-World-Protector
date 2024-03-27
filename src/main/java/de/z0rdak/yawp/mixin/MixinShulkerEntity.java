package de.z0rdak.yawp.mixin;

import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityType;
import net.minecraft.entity.monster.IMob;
import net.minecraft.entity.monster.ShulkerEntity;
import net.minecraft.entity.passive.GolemEntity;
import net.minecraft.world.World;
import net.minecraft.world.server.ServerWorld;
import net.minecraftforge.common.ForgeHooks;
import org.spongepowered.asm.mixin.Mixin;

import javax.annotation.Nullable;

@Mixin(ShulkerEntity.class)
public abstract class MixinShulkerEntity  extends GolemEntity implements IMob {

    protected MixinShulkerEntity(EntityType<? extends GolemEntity> type, World worldIn) {
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
