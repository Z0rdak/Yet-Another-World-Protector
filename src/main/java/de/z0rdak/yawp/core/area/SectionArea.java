package de.z0rdak.yawp.core.area;

import net.minecraft.core.BlockPos;
import net.minecraft.core.SectionPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtUtils;
import org.apache.commons.lang3.NotImplementedException;

import java.util.Collections;
import java.util.List;
import java.util.Set;

import static de.z0rdak.yawp.util.constants.AreaNBT.POS;

public class SectionArea extends AbstractArea {

    private SectionPos section;

    protected SectionArea(SectionPos pos) {
        super(AreaType.SECTION);
        this.section = pos;
    }

    protected SectionArea(int x, int y, int z) {
        this(SectionPos.of(x, y, z));
    }

    protected SectionArea(CompoundTag nbt) {
        super(nbt);
        this.deserializeNBT(nbt);
    }

    @Override
    public CompoundTag serializeNBT() {
        CompoundTag nbt = super.serializeNBT();
        nbt.put(POS, NbtUtils.writeBlockPos(this.section.origin()));
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        super.deserializeNBT(nbt);
        this.section = SectionPos.of(NbtUtils.readBlockPos(nbt.getCompound(POS)));
    }

    @Override
    public boolean contains(BlockPos pos) {
        return SectionPos.of(pos).equals(this.section);
    }

    @Override
    public List<BlockPos> getMarkedBlocks() {
        return Collections.singletonList(this.section.origin());
    }

    @Override
    public Set<BlockPos> getHull() {
        throw new NotImplementedException("ChunkArea.getHull() not implemented yet");
    }

}
