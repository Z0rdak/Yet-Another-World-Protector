package de.z0rdak.yawp.core.area;

import de.z0rdak.yawp.util.constants.AreaNBT;
import net.minecraft.nbt.NbtCompound;
import net.minecraft.nbt.NbtElement;
import net.minecraft.nbt.NbtHelper;
import net.minecraft.nbt.NbtList;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.ChunkPos;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class MultiChunkArea extends AbstractArea {

    private List<ChunkPos> chunks;

    protected MultiChunkArea() {
        super(AreaType.MULTI_CHUNK);
        chunks = new ArrayList<>();
    }

    protected MultiChunkArea(NbtCompound nbt) {
        super(nbt);
        this.deserializeNBT(nbt);
    }

    @Override
    public NbtCompound serializeNBT() {
        NbtCompound nbt = super.serializeNBT();
        NbtList pointList = new NbtList();
        this.chunks.stream()
                .map(ChunkPos::getStartPos)
                .forEach((point) -> {
                    NbtElement pointNbt = NbtHelper.fromBlockPos(point);
                    pointList.add(pointNbt);
                });
        nbt.put(AreaNBT.BLOCKS, pointList);
        return nbt;
    }

    @Override
    public void deserializeNBT(NbtCompound nbt) {
        super.deserializeNBT(nbt);
        this.chunks.clear();
        NbtList posList = nbt.getList(AreaNBT.BLOCKS, NbtElement.COMPOUND_TYPE);
//FIXME        for (int i = 0; i < posList.size(); i++) {
//FIXME            BlockPos pos = NbtHelper.toBlockPos(posList.getCompound(i));
//FIXME            this.chunks.add(new ChunkPos(pos));
//FIXME        }
    }

    @Override
    public boolean contains(BlockPos pos) {
        ChunkPos chunkPosOfBlock = new ChunkPos(pos);
        return chunks.stream().anyMatch(chunkPos -> chunkPos.equals(chunkPosOfBlock));
    }

    @Override
    public List<BlockPos> getMarkedBlocks() {
        return this.chunks.stream().map(ChunkPos::getStartPos).collect(Collectors.toList());
    }
}
