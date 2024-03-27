package de.z0rdak.yawp.mixin;

import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityType;
import net.minecraft.entity.item.ItemEntity;
import net.minecraft.world.World;
import net.minecraft.world.server.ServerWorld;
import net.minecraftforge.common.ForgeHooks;
import org.spongepowered.asm.mixin.Mixin;

import javax.annotation.Nullable;

@Mixin(ItemEntity.class)
public abstract class MixinItemEntity extends Entity {

    public MixinItemEntity(EntityType<?> entityTypeIn, World worldIn) {
        super(entityTypeIn, worldIn);
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
