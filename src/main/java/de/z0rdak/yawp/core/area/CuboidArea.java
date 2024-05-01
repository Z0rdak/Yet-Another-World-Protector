package de.z0rdak.yawp.core.area;

import de.z0rdak.yawp.util.AreaUtil;
import de.z0rdak.yawp.util.constants.AreaNBT;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.core.Direction;
import net.minecraft.nbt.NbtUtils;
import net.minecraft.world.phys.AABB;

import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static de.z0rdak.yawp.util.AreaUtil.blocksBetweenOnAxis;

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
        this(new AABB(p1, p2));
        this.p1 = AreaUtil.getLowerPos(p1, p2);
        this.p2 = AreaUtil.getHigherPos(p1, p2);
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
        this.area = new AABB(this.p1, this.p2);
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

    /**
     * TODO: WIP - this only returns the frame
     *                  Z+
     *        p7-----p8
     *       /|      /|
     *  Y+  p5------p6|
     *      | |     | |
     *      | p3----|-p4
     *      |/      |/
     *      p1------p2  X+
     *
     * @return outer frame as set of BlockPos of cuboid area
     */
    @Override
    public Set<BlockPos> getHull() {
        BlockPos p1 = new BlockPos(this.area.minX, this.area.minY, this.area.minZ);
        BlockPos p2 = new BlockPos(this.area.maxX, this.area.minY, this.area.minZ);
        BlockPos p3 = new BlockPos(this.area.minX, this.area.minY, this.area.maxZ);
        BlockPos p4 = new BlockPos(this.area.maxX, this.area.minY, this.area.maxZ);
        BlockPos p5 = new BlockPos(this.area.minX, this.area.maxY, this.area.minZ);
        BlockPos p6 = new BlockPos(this.area.maxX, this.area.maxY, this.area.minZ);
        BlockPos p7 = new BlockPos(this.area.minX, this.area.maxY, this.area.maxZ);
        BlockPos p8 = new BlockPos(this.area.maxX, this.area.maxY, this.area.maxZ);
        Set<BlockPos> p12 = blocksBetweenOnAxis(p1, p2, Direction.Axis.X);
        Set<BlockPos> p34 = blocksBetweenOnAxis(p3, p4, Direction.Axis.X);
        Set<BlockPos> p56 = blocksBetweenOnAxis(p5, p6, Direction.Axis.X);
        Set<BlockPos> p78 = blocksBetweenOnAxis(p7, p8, Direction.Axis.X);
        Set<BlockPos> p15 = blocksBetweenOnAxis(p1, p5, Direction.Axis.Y);
        Set<BlockPos> p26 = blocksBetweenOnAxis(p2, p6, Direction.Axis.Y);
        Set<BlockPos> p37 = blocksBetweenOnAxis(p3, p7, Direction.Axis.Y);
        Set<BlockPos> p48 = blocksBetweenOnAxis(p4, p8, Direction.Axis.Y);
        Set<BlockPos> p13 = blocksBetweenOnAxis(p1, p3, Direction.Axis.Z);
        Set<BlockPos> p24 = blocksBetweenOnAxis(p2, p4, Direction.Axis.Z);
        Set<BlockPos> p57 = blocksBetweenOnAxis(p5, p7, Direction.Axis.Z);
        Set<BlockPos> p68 = blocksBetweenOnAxis(p6, p8, Direction.Axis.Z);
        return Stream.of(p12, p34, p56, p78, p15, p26, p37, p48, p13, p24, p57, p68)
                .flatMap(Set::stream)
                .collect(Collectors.toSet());

}
    }
