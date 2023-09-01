package de.z0rdak.yawp.core.area;

import de.z0rdak.yawp.util.AreaUtil;
import de.z0rdak.yawp.util.constants.AreaNBT;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.nbt.NBTUtil;
import net.minecraft.util.math.AxisAlignedBB;
import net.minecraft.util.math.BlockPos;

import java.util.Arrays;
import java.util.List;

/**
 * Represents and wraps a simple AxisAlignedBB.
 * This area is marked by two positions and thus spans a cuboid shape
 */
public class CuboidArea extends AbstractArea {

    private AxisAlignedBB area;
    private BlockPos p1;
    private BlockPos p2;

    public CuboidArea(AxisAlignedBB area) {
        super(AreaType.CUBOID);
        this.area = area;
    }

    public CuboidArea(BlockPos p1, BlockPos p2) {
        this(new AxisAlignedBB(p1, p2));
        this.p1 = p1;
        this.p2 = p2;
    }

    public CuboidArea(List<BlockPos> blocks){
        this(blocks.get(0), blocks.get(1));
    }

    public CuboidArea(CompoundNBT nbt) {
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

    // TODO: Get the lower pos and retain it as lower pos
    public static CuboidArea expand(CuboidArea area, int min, int max) {
        BlockPos p1 = area.getAreaP1();
        BlockPos p2 = area.getAreaP2();
        return new CuboidArea(new BlockPos(p1.getX(), min, p1.getZ()),
                new BlockPos(p2.getX(), max, p2.getZ()));
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

    public int getXsize(){
        return (int) Math.max(this.area.getXsize(), 1);
    }

    public int getZsize(){
        return (int) Math.max(this.area.getZsize(), 1);
    }

    public int getYsize(){
        return (int) Math.max(this.area.getYsize(), 1);
    }

    public BlockPos getAreaP1() {
        return this.p1;
    }

    public BlockPos getAreaP2() {
        return this.p2;
    }

    @Override
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = super.serializeNBT();
        nbt.put(AreaNBT.P1, NBTUtil.writeBlockPos(this.p1));
        nbt.put(AreaNBT.P2, NBTUtil.writeBlockPos(this.p2));
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        super.deserializeNBT(nbt);
        this.p1 = NBTUtil.readBlockPos(nbt.getCompound(AreaNBT.P1));
        this.p2 = NBTUtil.readBlockPos(nbt.getCompound(AreaNBT.P2));
        this.area = new AxisAlignedBB(this.p1, this.p2);
    }

    @Override
    public String toString() {
        String strBuilder = getAreaType().areaType + " " + AreaUtil.blockPosStr(this.p1) + " <-> " + AreaUtil.blockPosStr(this.p2) +
                "\n" + "Size: " + "X=" + this.area.getXsize() + ", Y=" + this.area.getYsize() + ", Z=" + this.area.getZsize() +
                "\n" + "Blocks: " + AreaUtil.blockPosStr(this.p1) + ", " + AreaUtil.blockPosStr(this.p2);
        return strBuilder;
    }

    @Override
    public List<BlockPos> getMarkedBlocks() {
        return Arrays.asList(this.p1, this.p2);
    }
}
