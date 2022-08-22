package de.z0rdak.yawp.core.area;

import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.math.BlockPos;

import java.util.ArrayList;
import java.util.List;

public class PrismArea extends AbstractArea {
    // TODO: implementation
    public List<BlockPos> blockNodes;

    public PrismArea(CompoundNBT nbt) {
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
    public CompoundNBT serializeNBT() {
        return null;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {

    }
}
