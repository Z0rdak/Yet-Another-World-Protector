package de.z0rdak.yawp.core.area;

import net.minecraft.nbt.NbtCompound;
import net.minecraft.nbt.NbtHelper;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.ChunkSectionPos;

import java.util.Collections;
import java.util.List;

import de.z0rdak.yawp.util.NbtCompatHelper;

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
        this.section = ChunkSectionPos.from(NbtCompatHelper.toBlockPos(nbt, POS).orElseThrow());
    }

    @Override
    public boolean contains(BlockPos pos) {
        return ChunkSectionPos.from(pos).equals(this.section);
    }

    @Override
    public List<BlockPos> getMarkedBlocks() {
        return Collections.singletonList(this.section.getMinPos());
    }

}
