package de.z0rdak.regionshield.common.core.area;

import net.minecraft.nbt.CompoundNBT;
import net.minecraft.nbt.NBTUtil;
import net.minecraft.util.math.AxisAlignedBB;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.vector.Vector3d;

/**
 * Represents and wraps a simple AxisAlignedBB.
 * This area is marked by two positions and thus spans a cuboid shape
 */
public class CuboidArea extends AbstractArea {

    private AxisAlignedBB area;

    public CuboidArea(AxisAlignedBB area) {
        this();
        this.area = area;
    }

    private CuboidArea(){
        super(AreaType.CUBOID);
    }

    public CuboidArea(CompoundNBT nbt) {
        this();
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

    public AxisAlignedBB getArea() {
        return area;
    }

    public BlockPos getAreaP1(){
        return new BlockPos(this.area.minX, this.area.minY, this.area.minZ);
    }

    public BlockPos getAreaP2(){
        return new BlockPos(this.area.maxX, this.area.maxY, this.area.maxZ);
    }

    // TODO: compound p1, p2
    @Override
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = new CompoundNBT();
        nbt.put("p1", NBTUtil.writeBlockPos(this.getAreaP1()));
        nbt.put("p2", NBTUtil.writeBlockPos(this.getAreaP2()));
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        BlockPos p1 = NBTUtil.readBlockPos(nbt.getCompound("p1"));
        BlockPos p2 = NBTUtil.readBlockPos(nbt.getCompound("p2"));
        this.area = new AxisAlignedBB(p1, p2);
    }

    @Override
    public String toString() {
        return "Cuboid " + AreaUtil.toString(this.getAreaP1()) + " -> " + AreaUtil.toString(this.getAreaP1());
    }

    @Override
    public Vector3d getCenter() {
        return this.area.getCenter();
    }
}
