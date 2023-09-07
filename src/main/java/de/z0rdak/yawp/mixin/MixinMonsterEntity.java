package de.z0rdak.yawp.mixin;

import net.minecraft.entity.CreatureEntity;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityType;
import net.minecraft.entity.monster.MonsterEntity;
import net.minecraft.world.World;
import net.minecraft.world.server.ServerWorld;
import net.minecraftforge.common.ForgeHooks;
import org.spongepowered.asm.mixin.Mixin;

import javax.annotation.Nullable;

@Mixin(MonsterEntity.class)
public abstract class MixinMonsterEntity extends CreatureEntity {

    protected MixinMonsterEntity(EntityType<? extends CreatureEntity> type, World worldIn) {
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
