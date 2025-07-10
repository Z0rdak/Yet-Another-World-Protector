package de.z0rdak.yawp.api.visualization;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.area.BlockDisplayProperties;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.commands.data.EntityDataAccessor;
import net.minecraft.world.entity.Entity;

import java.util.HashMap;
import java.util.Map;

public class RegionVisualization {

    private final Map<BlockPos, Entity> displayEntities;
    private final BlockDisplayProperties properties;

    public RegionVisualization(BlockDisplayProperties properties) {
        this.displayEntities = new HashMap<>();
        this.properties = properties;
    }

    public void trackBlockDisplay(BlockPos pos, Entity entity) {
        this.displayEntities.put(pos, entity);
    }

    public void discardBlockDisplay(BlockPos pos) {
        Entity entityToRemove = this.displayEntities.remove(pos);
        entityToRemove.remove(Entity.RemovalReason.DISCARDED);
    }

    public void discardEntities(){
        this.displayEntities.forEach((pos,entity) -> {
            entity.remove(Entity.RemovalReason.DISCARDED);
        });
        this.displayEntities.clear();
    }

    public void updateBlock(final ResourceLocation block) {
        this.properties.setBlockRl(block);
        this.displayEntities.forEach((pos, entity) -> {
            EntityDataAccessor entityDataAccessor = new EntityDataAccessor(entity);
            CompoundTag entityTag = entityDataAccessor.getData();
            entityTag.putString("id", block.toString());
            try {
                entityDataAccessor.setData(entityTag);
            } catch (CommandSyntaxException e) {
                Constants.LOGGER.error("Should not happend - what did you do?!", e);
                throw new RuntimeException(e);
            }
        });
    }

    public void setGlow(final boolean glow) {
        this.properties.setHasGlow(glow);
        this.displayEntities.forEach((pos, entity) -> {
            EntityDataAccessor entityDataAccessor = new EntityDataAccessor(entity);
            CompoundTag entityTag = entityDataAccessor.getData();
            entityTag.putBoolean("Glowing", glow);
            try {
                entityDataAccessor.setData(entityTag);
            } catch (CommandSyntaxException e) {
                Constants.LOGGER.error("Should not happend - what did you do?!", e);
                throw new RuntimeException(e);
            }
        });
    }

    public void setLightLevel(final int lightLevel) {
        this.properties.setLightLevel(lightLevel);
        this.displayEntities.forEach((pos, entity) -> {
            EntityDataAccessor entityDataAccessor = new EntityDataAccessor(entity);
            CompoundTag entityTag = entityDataAccessor.getData();
            var brightnessTag = new CompoundTag();
            brightnessTag.putInt("sky", lightLevel);
            brightnessTag.putInt("block", lightLevel);
            entityTag.put("brightness", brightnessTag);
            try {
                entityDataAccessor.setData(entityTag);
            } catch (CommandSyntaxException e) {
                Constants.LOGGER.error("Should not happend - what did you do?!", e);
                throw new RuntimeException(e);
            }
        });
    }

}
