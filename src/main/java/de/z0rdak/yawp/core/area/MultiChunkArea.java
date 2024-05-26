package de.z0rdak.yawp.core.area;

import de.z0rdak.yawp.util.constants.AreaNBT;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.NbtUtils;
import net.minecraft.nbt.Tag;
import net.minecraft.world.level.ChunkPos;
import org.apache.commons.lang3.NotImplementedException;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public class MultiChunkArea extends AbstractArea {

    private List<ChunkPos> chunks;

    protected MultiChunkArea() {
        super(AreaType.MULTI_CHUNK);
        chunks = new ArrayList<>();
    }

    protected MultiChunkArea(CompoundTag nbt) {
        super(nbt);
        this.deserializeNBT(nbt);
    }

    @Override
    public CompoundTag serializeNBT() {
        CompoundTag nbt = super.serializeNBT();
        ListTag pointList = new ListTag();
        this.chunks.stream()
                .map(ChunkPos::getWorldPosition)
                .forEach((point) -> {
                    CompoundTag pointNbt = NbtUtils.writeBlockPos(point);
                    pointList.add(pointNbt);
                });
        nbt.put(AreaNBT.BLOCKS, pointList);
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        super.deserializeNBT(nbt);
        this.chunks.clear();
        ListTag posList = nbt.getList(AreaNBT.BLOCKS, Tag.TAG_COMPOUND);
        for (int i = 0; i < posList.size(); i++) {
            BlockPos pos = NbtUtils.readBlockPos(posList.getCompound(i));
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
