package de.z0rdak.yawp.core.area;

import de.z0rdak.yawp.constants.serialization.RegionNbtKeys;
import de.z0rdak.yawp.util.NbtCompatHelper;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Vec3i;

import java.util.Set;

public abstract class CenteredArea extends MarkedArea {

    protected BlockPos center;

    public CenteredArea(AreaType areaType) {
        super(areaType);
    }

    public CenteredArea(BlockPos center, AreaType areaType) {
        super(areaType);
        this.center = center;
    }

    public Vec3i getCenter() {
        return new Vec3i(this.center.getX(), this.center.getY(), this.center.getZ());
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
        this.center = NbtCompatHelper.toBlockPos(nbt, "center").orElseThrow();
    }

    @Override
    public Set<BlockPos> markedBlocks() {
        return Set.of(this.center);
    }
}
