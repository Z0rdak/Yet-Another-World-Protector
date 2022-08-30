package de.z0rdak.yawp.core.area;

import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;

import java.util.ArrayList;
import java.util.List;

public class PrismArea extends AbstractArea {
    // TODO: implementation
    public List<BlockPos> blockNodes;

    public PrismArea(CompoundTag nbt) {
        this();
        this.deserializeNBT(nbt);
    }
    public PrismArea() {
        super(AreaType.PRISM);
        this.blockNodes = new ArrayList<>();
    }

    public PrismArea(List<BlockPos> blockNodes){
        this();
        this.blockNodes = blockNodes;
    }

    @Override
    public boolean contains(BlockPos pos) {
        return false;
    }

    @Override
    public CompoundTag serializeNBT() {
        return null;
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {

    }
}
