package de.z0rdak.yawp.mixin;

import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityType;
import net.minecraft.entity.item.minecart.AbstractMinecartEntity;
import net.minecraft.world.World;
import net.minecraft.world.server.ServerWorld;
import net.minecraftforge.common.ForgeHooks;
import net.minecraftforge.common.extensions.IForgeEntityMinecart;
import org.spongepowered.asm.mixin.Mixin;

import javax.annotation.Nullable;

@Mixin(AbstractMinecartEntity.class)
public abstract class MixinAbstractMinecartEntity extends Entity implements IForgeEntityMinecart {

    public MixinAbstractMinecartEntity(EntityType<?> entityTypeIn, World worldIn) {
        super(entityTypeIn, worldIn);
    }

    @Nullable
    @Override
    public Entity changeDimension(ServerWorld server) {
        if (!ForgeHooks.onTravelToDimension(this, server.dimension())) {
            return null;
        }
        return super.changeDimension(server);
    }
}
