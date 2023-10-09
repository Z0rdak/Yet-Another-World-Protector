package de.z0rdak.yawp.core.area;

import de.z0rdak.yawp.util.AreaUtil;
import de.z0rdak.yawp.util.constants.AreaNBT;
import net.minecraft.nbt.NbtCompound;
import net.minecraft.nbt.NbtHelper;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Box;

import java.util.Arrays;
import java.util.List;

/**
 * Represents and wraps a simple AxisAlignedBB.
 * This area is marked by two positions and thus spans a cuboid shape
 */
public class CuboidArea extends AbstractArea {

    private Box area;
    private BlockPos p1;
    private BlockPos p2;

    public CuboidArea(Box area) {
        super(AreaType.CUBOID);
        this.area = area;
    }

    public CuboidArea(BlockPos p1, BlockPos p2) {
        this(new Box(p1, p2));
        this.p1 = p1;
        this.p2 = p2;
    }

    public CuboidArea(List<BlockPos> blocks) {
        this(blocks.get(0), blocks.get(1));
    }

    public CuboidArea(NbtCompound nbt) {
        super(nbt);
        this.deserializeNBT(nbt);
    }

    @Override
    public boolean contains(BlockPos pos) {
        // INFO: this.area.contains(x,y,z); does not work, because the max checks are exclusive by default.
        // TODO: Maybe replace with net.minecraft.util.math.MutableBoundingBox::intersectsWith which has inclusive checks
        return pos.getX() >= area.minX && pos.getX() <= area.maxX
                && pos.getY() >= this.area.minY && pos.getY() <= this.area.maxY
                && pos.getZ() >= this.area.minZ && pos.getZ() <= this.area.maxZ;
    }

    public boolean contains(CuboidArea inner) {
        return this.area.minX <= inner.area.minX && this.area.maxX >= inner.area.maxX
                && this.area.minY <= inner.area.minY && this.area.maxY >= inner.area.maxY
                && this.area.minZ <= inner.area.minZ && this.area.maxZ >= inner.area.maxZ;
    }

    public boolean intersects(CuboidArea other) {
        return this.area.intersects(other.area);
    }

    public Box getArea() {
        return area;
    }


    public int getXsize() {
        return (int) Math.max(this.area.getLengthX(), 1);
    }

    public int getZsize() {
        return (int) Math.max(this.area.getLengthZ(), 1);
    }

    public int getYsize() {
        return (int) Math.max(this.area.getLengthY(), 1);
    }

    public BlockPos getAreaP1() {
        return this.p1;
    }

    public BlockPos getAreaP2() {
        return this.p2;
    }

    @Override
    public NbtCompound serializeNBT() {
        NbtCompound nbt = super.serializeNBT();
        nbt.put(AreaNBT.P1, NbtHelper.fromBlockPos(this.p1));
        nbt.put(AreaNBT.P2, NbtHelper.fromBlockPos(this.p2));
        return nbt;
    }

    @Override
    public void deserializeNBT(NbtCompound nbt) {
        super.deserializeNBT(nbt);
        this.p1 = NbtHelper.toBlockPos(nbt.getCompound(AreaNBT.P1));
        this.p2 = NbtHelper.toBlockPos(nbt.getCompound(AreaNBT.P2));
        this.area = new Box(p1, p2);
    }

    @Override
    public String toString() {
        String strBuilder = getAreaType().areaType + " " + AreaUtil.blockPosStr(this.p1) + " <-> " + AreaUtil.blockPosStr(this.p2) +
                "\n" + "Size: " + "X=" + this.area.getLengthX() + ", Y=" + this.area.getLengthY() + ", Z=" + this.area.getLengthZ() +
                "\n" + "Blocks: " + AreaUtil.blockPosStr(this.p1) + ", " + AreaUtil.blockPosStr(this.p2);
        return strBuilder;
    }

    @Override
    public List<BlockPos> getMarkedBlocks() {
        return Arrays.asList(this.p1, this.p2);
    }
}
