package de.z0rdak.yawp.core.area;

import net.minecraft.nbt.CompoundNBT;
import net.minecraft.nbt.NBTUtil;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.ChunkPos;

import java.util.Collections;
import java.util.List;

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

    protected ChunkArea(CompoundNBT nbt) {
        super(nbt);
        this.deserializeNBT(nbt);
    }

    @Override
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = super.serializeNBT();
        nbt.put(POS, NBTUtil.writeBlockPos(this.chunk.getWorldPosition()));
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        super.deserializeNBT(nbt);
        this.chunk = new ChunkPos(NBTUtil.readBlockPos(nbt.getCompound(POS)));
    }

    @Override
    public boolean contains(BlockPos pos) {
        return this.chunk.equals(new ChunkPos(pos));
    }

    @Override
    public List<BlockPos> getMarkedBlocks() {
        return Collections.singletonList(this.chunk.getWorldPosition());
    }

}
