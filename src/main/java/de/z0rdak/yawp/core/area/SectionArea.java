package de.z0rdak.yawp.core.area;

import net.minecraft.core.BlockPos;
import net.minecraft.core.HolderLookup;
import net.minecraft.core.SectionPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtUtils;
import org.apache.commons.lang3.NotImplementedException;

import java.util.Collections;
import java.util.List;
import java.util.Optional;
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

    protected SectionArea(HolderLookup.Provider provider, CompoundTag nbt) {
        super(provider, nbt);
        this.deserializeNBT(provider, nbt);
    }

    @Override
    public CompoundTag serializeNBT(HolderLookup.Provider provider) {
        CompoundTag nbt = super.serializeNBT(provider);
        nbt.put(POS, NbtUtils.writeBlockPos(this.section.origin()));
        return nbt;
    }

    @Override
    public void deserializeNBT(HolderLookup.Provider provider, CompoundTag nbt) {
        super.deserializeNBT(provider, nbt);
        Optional<BlockPos> pos = NbtUtils.readBlockPos(nbt, POS);
        pos.ifPresent(blockPos -> this.section = SectionPos.of(blockPos));
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


    @Override
    public boolean containsOther(IMarkableArea other) {
        throw new NotImplementedException("Not yet implemented");
    }

    @Override
    public boolean intersects(IMarkableArea other) {
        throw new NotImplementedException("Not yet implemented");
    }

}
