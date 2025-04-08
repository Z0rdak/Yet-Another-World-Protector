package de.z0rdak.yawp.core.area;

import de.z0rdak.yawp.util.NbtCompatHelper;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Vec3i;
import net.minecraft.nbt.*;

import java.util.Collections;
import java.util.List;

public abstract class CenteredArea extends AbstractArea {

    protected BlockPos center;

    public CenteredArea(AreaType areaType) {
        super(areaType);
    }

    public CenteredArea(CompoundTag nbt) {
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
        IntArrayTag blockPosInts = new IntArrayTag(new int[]{this.center.getX(), this.center.getY(), this.getCenter().getZ()});
        nbt.put("center", blockPosInts);
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        super.deserializeNBT(nbt);
        this.center = NbtCompatHelper.asBlockPos(nbt, "center").orElseThrow();
    }

    @Override
    public List<BlockPos> markedBlocks() {
        return Collections.singletonList(this.center);
    }
}
