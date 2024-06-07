package de.z0rdak.yawp.core.area;

import net.minecraft.util.math.BlockPos;
import net.minecraft.nbt.NbtCompound;
import net.minecraft.nbt.NbtHelper;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.ChunkPos;
import org.apache.commons.lang3.NotImplementedException;

import java.util.Collections;
import java.util.List;
import java.util.Set;

import static de.z0rdak.yawp.util.constants.AreaNBT.POS;

public class ChunkArea extends AbstractArea {

    private ChunkPos chunk;

    protected ChunkArea(ChunkPos pos) {
        super(AreaType.CHUNK);
        this.chunk = pos;
    }

    protected ChunkArea(int x, int z) {
        this(new ChunkPos(x, z));
    }

    protected ChunkArea(NbtCompound nbt) {
        super(nbt);
        this.deserializeNBT(nbt);
    }

    @Override
    public NbtCompound serializeNBT() {
        NbtCompound nbt = super.serializeNBT();
        nbt.put(POS, NbtHelper.fromBlockPos(this.chunk.getStartPos()));
        return nbt;
    }

    @Override
    public void deserializeNBT(NbtCompound nbt) {
        super.deserializeNBT(nbt);
        this.chunk = new ChunkPos(NbtHelper.toBlockPos(nbt.getCompound(POS)));
    }

    @Override
    public boolean contains(BlockPos pos) {
        return this.chunk.equals(new ChunkPos(pos));
    }

    @Override
    public List<BlockPos> getMarkedBlocks() {
        return Collections.singletonList(this.chunk.getStartPos());
    }

    @Override
    public Set<BlockPos> getHull() {
        throw new NotImplementedException("ChunkArea.getHull() not implemented yet");
    }


    @Override
    public boolean containsOther(IMarkableArea other) {
        throw new NotImplementedException("Not yet implemented");
    }

    @Override
    public boolean intersects(IMarkableArea other) {
        throw new NotImplementedException("Not yet implemented");
    }

}
