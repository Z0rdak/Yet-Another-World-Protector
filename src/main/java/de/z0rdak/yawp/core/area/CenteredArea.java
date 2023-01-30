package de.z0rdak.yawp.core.area;

import net.minecraft.nbt.NbtCompound;
import net.minecraft.nbt.NbtHelper;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3i;

import java.util.Collections;
import java.util.List;

public abstract class CenteredArea extends AbstractArea {

    protected BlockPos center;

    public CenteredArea(AreaType areaType) {
        super(areaType);
    }

    public CenteredArea(NbtCompound nbt) {
        super(nbt);
        this.deserializeNBT(nbt);
    }

    public CenteredArea(BlockPos center, AreaType areaType) {
        super(areaType);
        this.center = center;
    }

    public Vec3i getCenter() {
        return new Vec3i(this.center.getX(), this.center.getY(), this.center.getZ());
    }

    @Override
    public boolean contains(BlockPos pos) {
        return false;
    }

    @Override
    public NbtCompound serializeNBT() {
        NbtCompound nbt = super.serializeNBT();
        nbt.put("center", NbtHelper.fromBlockPos(this.center));
        return nbt;
    }

    @Override
    public void deserializeNBT(NbtCompound nbt) {
        super.deserializeNBT(nbt);
        this.center = NbtHelper.toBlockPos(nbt.getCompound("center"));
    }

    @Override
    public List<BlockPos> getMarkedBlocks() {
        return Collections.singletonList(this.center);
    }
}
