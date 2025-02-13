package de.z0rdak.yawp.api.core.area;

import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.area.IMarkableArea;
import de.z0rdak.yawp.util.AreaUtil;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.levelgen.structure.BoundingBox;

public class CuboidBuilder implements AreaBuilder {

    private BlockPos pos1;
    private BlockPos pos2;

    public CuboidBuilder() {
    }

    public CuboidBuilder area(BlockPos corner1, BlockPos corner2) {
        this.pos1 = AreaUtil.getLowerPos(corner1, corner2);
        this.pos2 = AreaUtil.getHigherPos(corner1, corner2);
        return this;
    }

    public CuboidBuilder atBlock(BlockPos pos) {
        return this.area(pos, pos);
    }

    public CuboidBuilder around(BlockPos center, int offsetX, int offsetY, int offsetZ) {
        this.pos1 = center.offset(-offsetX, -offsetY, -offsetZ);
        this.pos2 = center.offset(offsetX, offsetY, offsetZ);
        return this;
    }

    public CuboidBuilder startingAt(BlockPos corner, int widthX, int heightY, int depthZ) {
        this.pos1 = corner;
        this.pos2 = corner.offset(widthX, heightY, depthZ);
        return this;
    }

    public CuboidBuilder area(BoundingBox box) {
        BlockPos pos1 = new BlockPos(box.minX(), box.minY(), box.minZ());
        BlockPos pos2 = new BlockPos(box.maxX(), box.maxY(), box.maxZ());
        return this.area(pos1, pos2);
    }

    @Override
    public CuboidArea build() {
        return new CuboidArea(pos1, pos2);
    }

}