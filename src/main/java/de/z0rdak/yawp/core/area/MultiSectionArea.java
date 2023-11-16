package de.z0rdak.yawp.core.area;

import de.z0rdak.yawp.util.constants.AreaNBT;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.nbt.ListNBT;
import net.minecraft.nbt.NBTUtil;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.SectionPos;
import net.minecraftforge.common.util.Constants;
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

    protected MultiSectionArea(CompoundNBT nbt) {
        super(nbt);
        this.deserializeNBT(nbt);
    }

    @Override
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = super.serializeNBT();
        ListNBT pointList = new ListNBT();
        this.sections.stream()
                .map(SectionPos::origin)
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
        this.sections.clear();
        ListNBT posList = nbt.getList(AreaNBT.BLOCKS, Constants.NBT.TAG_COMPOUND);
        for (int i = 0; i < posList.size(); i++) {
            BlockPos pos = NBTUtil.readBlockPos(posList.getCompound(i));
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

}
