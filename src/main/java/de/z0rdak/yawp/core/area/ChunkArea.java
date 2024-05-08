package de.z0rdak.yawp.core.area;

import net.minecraft.core.BlockPos;
import net.minecraft.core.HolderLookup;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtUtils;
import net.minecraft.world.level.ChunkPos;

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

    protected ChunkArea(CompoundTag nbt) {
        super(nbt);
        this.deserializeNBT(provider, nbt);
    }

    @Override
    public CompoundTag serializeNBT(HolderLookup.Provider provider) {
        CompoundTag nbt = super.serializeNBT(provider);
        nbt.put(POS, NbtUtils.writeBlockPos(this.chunk.getWorldPosition()));
        return nbt;
    }

    @Override
    public void deserializeNBT(HolderLookup.Provider provider, CompoundTag nbt) {
        super.deserializeNBT(provider, nbt);
        this.chunk = new ChunkPos(NbtUtils.readBlockPos(nbt.getCompound(POS)));
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
