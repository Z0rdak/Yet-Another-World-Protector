package de.z0rdak.regionshield.common.core.area;

import net.minecraft.nbt.CompoundNBT;
import net.minecraft.nbt.NBTUtil;
import net.minecraft.util.math.BlockPos;

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

    public int getRadius() {
        return this.radius;
    }

    public static double distance(BlockPos a, BlockPos b) {
        return Math.sqrt(Math.pow(b.getX() - a.getX(), 2)
                + Math.pow(b.getY() - a.getY(), 2)
                + Math.pow(b.getZ() - a.getZ(), 2));
    }

    public SphereArea(BlockPos middlePos, int radius){
        this(middlePos, new BlockPos(middlePos)
                .offset(middlePos.getX(), middlePos.getY() + radius, middlePos.getZ()));
    }

    @Override
    public boolean contains(BlockPos pos) {
        // (x−cx)2+(y−cy)2+(z−cz)2<r2
        int cx = centerP.getX();
        int cy = centerP.getY();
        int cz = centerP.getZ();
        double xPow = Math.pow((pos.getX() - cx), 2);
        double yPow = Math.pow((pos.getY() - cy), 2);
        double zPow = Math.pow((pos.getZ() - cz), 2);
        double rPow = Math.pow(this.radius, 2);
        return (xPow + yPow + zPow) < rPow;
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
