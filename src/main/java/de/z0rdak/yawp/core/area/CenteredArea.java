package de.z0rdak.yawp.core.area;

import com.mojang.math.Vector3d;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtUtils;

import java.util.Collections;
import java.util.List;

public abstract class CenteredArea extends AbstractArea {

    protected BlockPos center;

    public CenteredArea(AreaType areaType){
        super(areaType);
    }

    public CenteredArea(CompoundTag nbt){
        super(nbt);
        this.deserializeNBT(nbt);
    }

    public CenteredArea(BlockPos center, AreaType areaType){
        super(areaType);
        this.center = center;
    }

    // FIXME: Use Vec3 instead of Vector3d
    public Vector3d getCenter() {
        return new Vector3d(this.center.getX(), this.center.getY(), this.center.getZ());
    }

    public BlockPos getCenterPos() {
        return this.center;
    }

    @Override
    public boolean contains(BlockPos pos) {
        return false;
    }

    @Override
    public CompoundTag serializeNBT() {
        CompoundTag nbt = super.serializeNBT();
        nbt.put("center", NbtUtils.writeBlockPos(this.center));
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        super.deserializeNBT(nbt);
        this.center = NbtUtils.readBlockPos(nbt.getCompound("center"));
    }

    @Override
    public List<BlockPos> getMarkedBlocks() {
        return Collections.singletonList(this.center);
    }
}
