package de.z0rdak.yawp.core.area;

import net.minecraft.nbt.CompoundNBT;
import net.minecraft.nbt.NBTUtil;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.vector.Vector3d;

public abstract class CenteredArea extends AbstractArea {

    protected BlockPos center;

    public CenteredArea(AreaType areaType){
        super(areaType);
    }

    public CenteredArea(CompoundNBT nbt){
        super(nbt);
        this.deserializeNBT(nbt);
    }

    public CenteredArea(BlockPos center, AreaType areaType){
        super(areaType);
        this.center = center;
    }

    public Vector3d getCenter() {
        return new Vector3d(this.center.getX(), this.center.getY(), this.center.getZ());
    }

    @Override
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = super.serializeNBT();
        nbt.put("center", NBTUtil.writeBlockPos(this.center));
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        super.deserializeNBT(nbt);
        this.center = NBTUtil.readBlockPos(nbt.getCompound("center"));
    }
}
