package de.z0rdak.yawp.api.visualization;

import de.z0rdak.yawp.core.area.BlockDisplayProperties;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.phys.Vec3;
import org.jetbrains.annotations.Nullable;

public class BlockVisualization {

    private BlockPos pos;
    @Nullable
    private Entity entity;
    private final BlockDisplayProperties properties;

    public BlockVisualization(BlockPos pos, BlockDisplayProperties properties) {
        this.properties = properties;
        this.pos = pos;
    }

    public void track(Entity blockEntity) {
        this.entity = blockEntity;
    }

    public void discard() {
        if (this.entity != null) {
            this.entity.remove(Entity.RemovalReason.DISCARDED);
            this.entity = null;
        }
    }

    public boolean doesTrack() {
        return this.entity != null;
    }

    public void move(BlockPos pos) {
        if (this.entity != null) {
            this.entity.moveOrInterpolateTo(new Vec3(pos),0, 0);
        }
    }
}
