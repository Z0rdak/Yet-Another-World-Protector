package de.z0rdak.yawp.core.area;

import de.z0rdak.yawp.util.constants.AreaNBT;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.nbt.ListNBT;
import net.minecraft.nbt.NBTUtil;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.ChunkPos;
import net.minecraftforge.common.util.Constants;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class MultiChunkArea extends AbstractArea {

    private List<ChunkPos> chunks;

    protected MultiChunkArea() {
        super(AreaType.MULTI_CHUNK);
        chunks = new ArrayList<>();
    }

    protected MultiChunkArea(CompoundNBT nbt) {
        super(nbt);
        this.deserializeNBT(nbt);
    }

    @Override
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = super.serializeNBT();
        ListNBT pointList = new ListNBT();
        this.chunks.stream()
                .map(ChunkPos::getWorldPosition)
                .forEach((point) -> {
                    CompoundNBT pointNbt = NBTUtil.writeBlockPos(point);
                    pointList.add(pointNbt);
                });
        nbt.put(AreaNBT.BLOCKS, pointList);
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        super.deserializeNBT(nbt);
        this.chunks.clear();
        ListNBT posList = nbt.getList(AreaNBT.BLOCKS, Constants.NBT.TAG_COMPOUND);
        for (int i = 0; i < posList.size(); i++) {
            BlockPos pos = NBTUtil.readBlockPos(posList.getCompound(i));
            this.chunks.add(new ChunkPos(pos));
        }
    }

    @Override
    public boolean contains(BlockPos pos) {
        ChunkPos chunkPosOfBlock = new ChunkPos(pos);
        return chunks.stream().anyMatch(chunkPos -> chunkPos.equals(chunkPosOfBlock));
    }

    @Override
    public List<BlockPos> getMarkedBlocks() {
        return this.chunks.stream().map(ChunkPos::getWorldPosition).collect(Collectors.toList());
    }
}
