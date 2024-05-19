package de.z0rdak.yawp.core.area;

import de.z0rdak.yawp.util.constants.AreaNBT;
import net.minecraft.core.BlockPos;
import net.minecraft.core.SectionPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.NbtUtils;
import net.minecraft.nbt.Tag;
import org.apache.commons.lang3.NotImplementedException;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public class MultiSectionArea extends AbstractArea {

    List<SectionPos> sections;

    protected MultiSectionArea() {
        super(AreaType.MULTI_SECTION);
    }

    protected MultiSectionArea(List<SectionPos> sections) {
        this();
        this.sections = sections;
    }

    protected MultiSectionArea(CompoundTag nbt) {
        super(nbt);
        this.deserializeNBT(nbt);
    }

    @Override
    public CompoundTag serializeNBT() {
        CompoundTag nbt = super.serializeNBT();
        ListTag pointList = new ListTag();
        this.sections.stream()
                .map(SectionPos::origin)
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
        this.sections.clear();
        ListTag posList = nbt.getList(AreaNBT.BLOCKS, Tag.TAG_COMPOUND);
        for (int i = 0; i < posList.size(); i++) {
            BlockPos pos = NbtUtils.readBlockPos(posList.getCompound(i));
            this.sections.add(SectionPos.of(pos));
        }
    }

    @Override
    public boolean contains(BlockPos pos) {
        SectionPos sectionForPos = SectionPos.of(pos);
        return sections.stream().anyMatch(s -> s.equals(sectionForPos));
    }

    @Override
    public List<BlockPos> getMarkedBlocks() {
        return this.sections.stream().map(SectionPos::origin).collect(Collectors.toList());
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
