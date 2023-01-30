package de.z0rdak.yawp.core.area;


import net.minecraft.nbt.NbtCompound;
import net.minecraft.util.math.BlockPos;
import org.apache.commons.lang3.NotImplementedException;

import java.util.ArrayList;
import java.util.List;

public class PrismArea extends AbstractArea {

    public List<BlockPos> blockNodes;

    public PrismArea(NbtCompound nbt) {
        super(nbt);
        this.deserializeNBT(nbt);
    }

    public PrismArea() {
        super(AreaType.PRISM);
        this.blockNodes = new ArrayList<>();
    }

    public PrismArea(List<BlockPos> blockNodes) {
        this();
        this.blockNodes = blockNodes;
    }

    // TODO: implementation
    @Override
    public boolean contains(BlockPos pos) {
        throw new NotImplementedException("Missing contains implementation in PrismArea");
    }

    // TODO: implementation
    @Override
    public NbtCompound serializeNBT() {
        NbtCompound nbt = super.serializeNBT();
        throw new NotImplementedException("Missing serializeNBT implementation in PrismArea");
    }

    // TODO: implementation
    @Override
    public void deserializeNBT(NbtCompound nbt) {
        super.deserializeNBT(nbt);
        throw new NotImplementedException("Missing deserializeNBT implementation in PrismArea");
    }

    @Override
    public List<BlockPos> getMarkedBlocks() {
        return this.blockNodes;
    }
}