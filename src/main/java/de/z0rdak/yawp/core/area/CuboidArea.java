package de.z0rdak.yawp.core.area;

import de.z0rdak.yawp.util.AreaUtil;
import de.z0rdak.yawp.util.constants.AreaNBT;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtUtils;
import net.minecraft.world.phys.AABB;
import net.minecraft.world.phys.Vec3;

import java.util.Arrays;
import java.util.List;

/**
 * Represents and wraps a simple AxisAlignedBB.
 * This area is marked by two positions and thus spans a cuboid shape
 */
public class CuboidArea extends AbstractArea {

    private AABB area;
    private BlockPos p1;
    private BlockPos p2;

    public CuboidArea(AABB area) {
        super(AreaType.CUBOID);
        this.area = area;
    }

    public CuboidArea(BlockPos p1, BlockPos p2) {
        this(new AABB(new Vec3(p1.getX(), p1.getY(), p1.getZ()), new Vec3(p2.getX(), p2.getY(), p2.getZ())));
        this.p1 = p1;
        this.p2 = p2;
    }

    public CuboidArea(List<BlockPos> blocks){
        this(blocks.get(0), blocks.get(1));
    }

    public CuboidArea(CompoundTag nbt) {
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

    public AABB getArea() {
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
    public CompoundTag serializeNBT() {
        CompoundTag nbt = super.serializeNBT();
        nbt.put(AreaNBT.P1, NbtUtils.writeBlockPos(this.p1));
        nbt.put(AreaNBT.P2, NbtUtils.writeBlockPos(this.p2));
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        super.deserializeNBT(nbt);
        this.p1 = NbtUtils.readBlockPos(nbt.getCompound(AreaNBT.P1));
        this.p2 = NbtUtils.readBlockPos(nbt.getCompound(AreaNBT.P2));
        this.area = new AABB(new Vec3(this.p1.getX(), this.p1.getY(), this.p1.getZ()), new Vec3(this.p2.getX(), this.p2.getY(), this.p2.getZ()));
    }

    @Override
    public String toString() {
        String strBuilder = getAreaType().areaType + " " + AreaUtil.blockPosStr(this.p1) + " <-> " + AreaUtil.blockPosStr(this.p2) +
                "\n" + "Size: " + "X=" + this.getXsize() + ", Y=" + this.getYsize() + ", Z=" + this.getZsize() +
                "\n" + "Blocks: " + AreaUtil.blockPosStr(this.p1) + ", " + AreaUtil.blockPosStr(this.p2);
        return strBuilder;
    }

    @Override
    public List<BlockPos> getMarkedBlocks() {
        return Arrays.asList(this.p1, this.p2);
    }
}
