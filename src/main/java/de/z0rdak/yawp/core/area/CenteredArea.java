package de.z0rdak.yawp.core.area;

import net.minecraft.core.BlockPos;
import net.minecraft.core.HolderLookup;
import net.minecraft.core.Vec3i;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtUtils;

import java.util.Collections;
import java.util.List;

public abstract class CenteredArea extends AbstractArea {

    protected BlockPos center;

    public CenteredArea(AreaType areaType){
        super(areaType);
    }

    public CenteredArea(CompoundTag nbt) {
        super(nbt);
        this.deserializeNBT(provider, nbt);
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
    public CompoundTag serializeNBT(HolderLookup.Provider provider) {
        CompoundTag nbt = super.serializeNBT(provider);
        nbt.put("center", NbtUtils.writeBlockPos(this.center));
        return nbt;
    }

    @Override
    public void deserializeNBT(HolderLookup.Provider provider, CompoundTag nbt) {
        super.deserializeNBT(provider, nbt);
        this.center = NbtUtils.readBlockPos(nbt.getCompound("center"));
    }

    @Override
    public List<BlockPos> getMarkedBlocks() {
        return Collections.singletonList(this.center);
    }
}
