package de.z0rdak.yawp.core.area;

import de.z0rdak.yawp.util.constants.AreaNBT;
import net.minecraft.nbt.NbtCompound;
import net.minecraft.nbt.NbtElement;
import net.minecraft.nbt.NbtHelper;
import net.minecraft.nbt.NbtList;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.ChunkSectionPos;

import java.util.List;
import java.util.stream.Collectors;

public class MultiSectionArea extends AbstractArea {

    List<ChunkSectionPos> sections;

    protected MultiSectionArea() {
        super(AreaType.MULTI_SECTION);
    }

    protected MultiSectionArea(List<ChunkSectionPos> sections) {
        this();
        this.sections = sections;
    }

    protected MultiSectionArea(NbtCompound nbt) {
        super(nbt);
        this.deserializeNBT(nbt);
    }

    @Override
    public NbtCompound serializeNBT() {
        NbtCompound nbt = super.serializeNBT();
        NbtList pointList = new NbtList();
        this.sections.stream()
                .map(ChunkSectionPos::getMinPos)
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
        this.sections.clear();
        NbtList posList = nbt.getList(AreaNBT.BLOCKS, NbtElement.COMPOUND_TYPE);
        for (int i = 0; i < posList.size(); i++) {
            BlockPos pos = NbtHelper.toBlockPos(posList.getCompound(i));
            this.sections.add(ChunkSectionPos.from(pos));
        }
    }

    @Override
    public boolean contains(BlockPos pos) {
        ChunkSectionPos sectionForPos = ChunkSectionPos.from(pos);
        return sections.stream().anyMatch(s -> s.equals(sectionForPos));
    }

    @Override
    public List<BlockPos> getMarkedBlocks() {
        return this.sections.stream().map(ChunkSectionPos::getMinPos).collect(Collectors.toList());
    }

}
