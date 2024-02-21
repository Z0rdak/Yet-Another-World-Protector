package de.z0rdak.yawp.core.area;

import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.math.BlockPos;
import org.apache.commons.lang3.NotImplementedException;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

public class PrismArea extends AbstractArea {

    public List<BlockPos> blockNodes;

    public PrismArea(CompoundNBT nbt) {
        super(nbt);
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
        throw new NotImplementedException("Missing contains implementation in PrismArea");
    }

    @Override
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = super.serializeNBT();
        throw new NotImplementedException("Missing serializeNBT implementation in PrismArea");
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        super.deserializeNBT(nbt);
        throw new NotImplementedException("Missing deserializeNBT implementation in PrismArea");
    }

    @Override
    public List<BlockPos> getMarkedBlocks() {
        return this.blockNodes;
    }

    @Override
    public Set<BlockPos> getHull() {
        throw new NotImplementedException("ChunkArea.getHull() not implemented yet");
    }
}
