package de.z0rdak.regionshield.common.core.area;

import net.minecraft.nbt.CompoundNBT;
import net.minecraft.nbt.NBTUtil;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.vector.Vector3d;

import static de.z0rdak.regionshield.common.core.area.AreaUtil.distance;

public class SphereArea extends AbstractArea {

    private BlockPos centerP;
    private int radius;

    private SphereArea() {
        super(AreaType.SPHERE);
    }

    public SphereArea(CompoundNBT nbt) {
        this();
        this.deserializeNBT(nbt);
    }

    public SphereArea(BlockPos centerPos, BlockPos scopePos){
        this();
        this.centerP = centerPos;
        this.radius = (int) distance(centerPos, scopePos);
    }

    public BlockPos getCenterP() {
        return new BlockPos(this.centerP);
    }

    @Override
    public Vector3d getCenter() {
        return new Vector3d(this.centerP.getX(), this.centerP.getY(), this.centerP.getZ());
    }

    public int getRadius() {
        return this.radius;
    }

    public SphereArea(BlockPos middlePos, int radius){
        this(middlePos, new BlockPos(middlePos)
                .offset(0, radius, 0));
    }

    @Override
    public boolean contains(BlockPos pos) {
        return distance(this.centerP, pos) < this.radius + 0.5;
    }

    @Override
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = new CompoundNBT();
        nbt.put("center", NBTUtil.writeBlockPos(this.centerP));
        nbt.putInt("radius", this.radius);
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        this.centerP = NBTUtil.readBlockPos(nbt.getCompound("center"));
        this.radius = nbt.getInt("radius");
    }

    @Override
    public String toString() {
        return "SphereArea " + AreaUtil.toString(this.centerP) + ", r=" + radius;
    }
}
