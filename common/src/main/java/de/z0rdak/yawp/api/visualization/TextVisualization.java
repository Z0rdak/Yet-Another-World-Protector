package de.z0rdak.yawp.api.visualization;

import de.z0rdak.yawp.core.area.TextDisplayProperties;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.Entity;
import org.jetbrains.annotations.Nullable;

public class TextVisualization {

    private BlockPos pos;

    @Nullable
    private Entity entity;
    private final TextDisplayProperties properties;

    public TextVisualization(BlockPos pos, TextDisplayProperties textDisplayProperties) {
        this.pos = pos;
        this.properties = textDisplayProperties;
    }

    public void track(Entity textEntity) {
        this.entity = textEntity;
    }

    public void discard() {
        if (this.entity != null) {
            this.entity.remove(Entity.RemovalReason.DISCARDED);
            this.entity = null;
        }
    }
}
