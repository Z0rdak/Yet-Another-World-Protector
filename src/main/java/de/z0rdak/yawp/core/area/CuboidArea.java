package de.z0rdak.yawp.core.area;

import de.z0rdak.yawp.util.AreaUtil;
import de.z0rdak.yawp.util.constants.AreaNBT;
import net.minecraft.nbt.NbtCompound;
import net.minecraft.nbt.NbtHelper;
import net.minecraft.util.math.BlockBox;
import net.minecraft.util.math.BlockPos;

import java.util.Arrays;
import java.util.List;

/**
 * Represents and wraps a simple AxisAlignedBB.
 * This area is marked by two positions and thus spans a cuboid shape
 */
public class CuboidArea extends AbstractArea {

    private BlockBox area;

    public CuboidArea(BlockBox area) {
        super(AreaType.CUBOID);
        this.area = area;
    }

    public CuboidArea(BlockPos p1, BlockPos p2) {
        this(new BlockBox(p1.getX(), p1.getY(), p1.getZ(), p2.getX(), p2.getY(), p2.getZ()));
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
        return pos.getX() >= area.getMinX() && pos.getX() <= area.getMaxX()
                && pos.getY() >= this.area.getMinY() && pos.getY() <= this.area.getMaxY()
                && pos.getZ() >= this.area.getMinZ() && pos.getZ() <= this.area.getMaxZ();
    }

    public boolean contains(CuboidArea inner) {
        return this.area.getMinX() <= inner.area.getMinX() && this.area.getMaxX() >= inner.area.getMaxX()
                && this.area.getMinY() <= inner.area.getMinY() && this.area.getMaxY() >= inner.area.getMaxY()
                && this.area.getMinZ() <= inner.area.getMinZ() && this.area.getMaxZ() >= inner.area.getMaxZ();
    }

    public boolean intersects(CuboidArea other) {
        return this.area.intersects(other.area);
    }

    public BlockBox getArea() {
        return area;
    }

    public BlockPos getAreaP1() {
        return new BlockPos(this.area.getMinX(), this.area.getMinY(), this.area.getMinZ());
    }

    public BlockPos getAreaP2() {
        return new BlockPos(this.area.getMaxX(), this.area.getMaxY(), this.area.getMaxZ());
    }

    @Override
    public NbtCompound serializeNBT() {
        NbtCompound nbt = super.serializeNBT();
        nbt.put(AreaNBT.P1, NbtHelper.fromBlockPos(this.getAreaP1()));
        nbt.put(AreaNBT.P2, NbtHelper.fromBlockPos(this.getAreaP2()));
        return nbt;
    }

    @Override
    public void deserializeNBT(NbtCompound nbt) {
        super.deserializeNBT(nbt);
        BlockPos p1 = NbtHelper.toBlockPos(nbt.getCompound(AreaNBT.P1));
        BlockPos p2 = NbtHelper.toBlockPos(nbt.getCompound(AreaNBT.P2));
        this.area = new BlockBox(p1.getX(), p1.getY(), p1.getZ(), p2.getX(), p2.getY(), p2.getZ());
    }

    @Override
    public String toString() {
        String strBuilder = getAreaType().areaType + " " + AreaUtil.blockPosStr(this.getAreaP1()) + " <-> " + AreaUtil.blockPosStr(this.getAreaP2()) +
                "\n" + "Size: " + "X=" + this.area.getBlockCountX() + ", Y=" + this.area.getBlockCountY() + ", Z=" + this.area.getBlockCountZ() +
                "\n" + "Blocks: " + AreaUtil.blockPosStr(this.getAreaP1()) + ", " + AreaUtil.blockPosStr(this.getAreaP2());
        return strBuilder;
    }

    @Override
    public List<BlockPos> getMarkedBlocks() {
        return Arrays.asList(this.getAreaP1(), this.getAreaP2());
    }
}
