package de.z0rdak.yawp.core.area;

import de.z0rdak.yawp.util.AreaUtil;
import de.z0rdak.yawp.util.constants.AreaNBT;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.nbt.NBTUtil;
import net.minecraft.util.math.AxisAlignedBB;
import net.minecraft.util.math.BlockPos;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Represents and wraps a simple AxisAlignedBB.<br>
 * This area is marked by two diagonal opposite positions and thus span a cuboid shape.
 */
public class CuboidArea extends AbstractArea {

    private AxisAlignedBB area;

    public CuboidArea(AxisAlignedBB area) {
        super(AreaType.CUBOID);
        this.area = area;
    }

    public CuboidArea(BlockPos p1, BlockPos p2) {
        this(new AxisAlignedBB(p1, p2));
    }

    public CuboidArea(List<BlockPos> blocks) {
        this(blocks.get(0), blocks.get(1));
    }

    public CuboidArea(CompoundNBT nbt) {
        super(nbt);
        this.deserializeNBT(nbt);
    }

    @Override
    public boolean contains(BlockPos pos) {
        // INFO: this.area.contains(x,y,z); does not work, because the max checks are exclusive by default.
        // INFO: Maybe replace with net.minecraft.util.math.MutableBoundingBox::intersectsWith which has inclusive checks
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

    public AxisAlignedBB getArea() {
        return area;
    }

    public BlockPos getAreaP1() {
        return new BlockPos(this.area.minX, this.area.minY, this.area.minZ);
    }

    public BlockPos getAreaP2() {
        return new BlockPos(this.area.maxX, this.area.maxY, this.area.maxZ);
    }

    @Override
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = super.serializeNBT();
        nbt.put(AreaNBT.P1, NBTUtil.writeBlockPos(this.getAreaP1()));
        nbt.put(AreaNBT.P2, NBTUtil.writeBlockPos(this.getAreaP2()));
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        super.deserializeNBT(nbt);
        BlockPos p1 = NBTUtil.readBlockPos(nbt.getCompound(AreaNBT.P1));
        BlockPos p2 = NBTUtil.readBlockPos(nbt.getCompound(AreaNBT.P2));
        this.area = new AxisAlignedBB(p1, p2);
    }

    @Override
    public String toString() {
        StringBuilder strBuilder = new StringBuilder();
        strBuilder.append(getAreaType().areaType).append(" ").append(AreaUtil.blockPosStr(this.getAreaP1())).append(" <-> ").append(AreaUtil.blockPosStr(this.getAreaP2()))
                .append("\n").append("Size: ").append("X=").append(this.area.getXsize()).append(", Y=").append( this.area.getYsize()).append(", Z=").append( this.area.getZsize())
                .append("\n").append("Blocks: ").append(AreaUtil.blockPosStr(this.getAreaP1())).append(", ").append(AreaUtil.blockPosStr(this.getAreaP2()));
        return strBuilder.toString();
    }

    @Override
    public List<BlockPos> getMarkedBlocks() {
        return Arrays.asList(this.getAreaP1(), this.getAreaP2());
    }
}
