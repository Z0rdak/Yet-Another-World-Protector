package de.z0rdak.yawp.core.area;

import net.minecraft.core.BlockPos;
import org.apache.commons.lang3.NotImplementedException;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class PrismArea extends MarkedArea {

    public List<BlockPos> blockNodes;

    public PrismArea() {
        super(AreaType.PRISM);
        this.blockNodes = new ArrayList<>();
    }

    public PrismArea(List<BlockPos> blockNodes) {
        this();
        this.blockNodes = blockNodes;
    }

    @Override
    public boolean contains(BlockPos pos) {
        throw new NotImplementedException("Missing contains implementation in PrismArea");
    }

    @Override
    public Set<BlockPos> markedBlocks() {
        return new HashSet<>(this.blockNodes);
    }

    @Override
    public Set<BlockPos> getHull() {
        throw new NotImplementedException("Prism.getHull() not implemented yet");
    }

    @Override
    public Set<BlockPos> getFrame() {
        return Set.of();
    }

    @Override
    public Set<BlockPos> getMinimalOutline() {
        return Set.of();
    }


    @Override
    public boolean containsOther(IMarkableArea other) {
        throw new NotImplementedException("Not yet implemented");
    }

    @Override
    public boolean intersects(IMarkableArea other) {
        throw new NotImplementedException("Not yet implemented");
    }

    @Override
    public MarkedAreaType<?> getType() {
        return null;
    }
}