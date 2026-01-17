package de.z0rdak.yawp.api.visualization;

import de.z0rdak.yawp.core.area.BlockDisplayProperties;
import net.minecraft.core.BlockPos;
import net.minecraft.resources.Identifier;
import net.minecraft.world.entity.Entity;

import java.util.HashMap;
import java.util.Map;

import static de.z0rdak.yawp.api.visualization.VisualizationUtil.*;

public class RegionVisualization {

    private final Map<BlockPos, Entity> displayEntities;
    private BlockDisplayProperties properties;

    public BlockDisplayProperties getProperties() {
        return properties;
    }

    public RegionVisualization(BlockDisplayProperties properties) {
        this.displayEntities = new HashMap<>();
        this.properties = properties;
    }

    public boolean doesTrackEntityAt(BlockPos pos) {
        return displayEntities.containsKey(pos);
    }

    public void trackBlockDisplay(BlockPos pos, Entity entity) {
        this.displayEntities.put(pos, entity);
    }

    public void updateDisplay(BlockDisplayProperties displayProperties, boolean refresh) {
        this.properties = displayProperties;
        if (refresh) {
            this.refresh(displayProperties);
        }
    }

    private void refresh(BlockDisplayProperties properties) {
        this.displayEntities.forEach((pos, entity) -> {
            updateDisplayProperties(entity, properties);
        });
    }

    public boolean hasEntitiesTracked(){
        return !this.displayEntities.isEmpty();
    }

    public void discardEntities(){
        this.displayEntities.forEach((pos,entity) -> {
            entity.remove(Entity.RemovalReason.DISCARDED);
        });
        this.displayEntities.clear();
    }

    public void updateBlock(final Identifier block) {
        this.properties.setBlockRl(block);
        this.displayEntities.forEach((pos, entity) -> {
            updateDisplayBlock(entity, block);
        });
    }

    public void updateGlow(final boolean glow) {
        this.properties.setHasGlow(glow);
        this.displayEntities.forEach((pos, entity) -> {
            updateDisplayGlow(entity, glow);
        });
    }

    public void updateLightLevel(final int lightLevel) {
        this.properties.setLightLevel(lightLevel);
        this.displayEntities.forEach((pos, entity) -> {
            updateDisplayLightLevel(entity, lightLevel);
        });
    }

}
