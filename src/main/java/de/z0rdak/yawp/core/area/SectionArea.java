package de.z0rdak.yawp.core.area;

import net.minecraft.nbt.NbtHelper;
import net.minecraft.util.math.BlockPos;
import net.minecraft.nbt.NbtCompound;
import net.minecraft.nbt.NbtHelper;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.ChunkSectionPos;
import org.apache.commons.lang3.NotImplementedException;

import java.util.Collections;
import java.util.List;
import java.util.Set;

import static de.z0rdak.yawp.util.constants.AreaNBT.POS;

public class SectionArea extends AbstractArea {

    private ChunkSectionPos section;

    protected SectionArea(ChunkSectionPos pos) {
        super(AreaType.SECTION);
        this.section = pos;
    }

    protected SectionArea(int x, int y, int z) {
        this(ChunkSectionPos.from(x, y, z));
    }

    protected SectionArea(NbtCompound nbt) {
        super(nbt);
        this.deserializeNBT(nbt);
    }

    @Override
    public NbtCompound serializeNBT() {
        NbtCompound nbt = super.serializeNBT();
        nbt.put(POS, NbtHelper.fromBlockPos(this.section.getMinPos()));
        return nbt;
    }

    @Override
    public void deserializeNBT(NbtCompound nbt) {
        super.deserializeNBT(nbt);
        this.section = ChunkSectionPos.from(NbtHelper.toBlockPos(nbt.getCompound(POS)));
    }

    @Override
    public boolean contains(BlockPos pos) {
        return ChunkSectionPos.from(pos).equals(this.section);
    }

    @Override
    public List<BlockPos> getMarkedBlocks() {
        return Collections.singletonList(this.section.getMinPos());
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
