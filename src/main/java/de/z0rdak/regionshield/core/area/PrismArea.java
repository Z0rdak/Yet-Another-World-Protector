package de.z0rdak.regionshield.core.area;

import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.math.BlockPos;

public class PrismArea extends AbstractArea {
    // TODO: implementation
    private PrismArea() {
        super(AreaType.PRISM);
    }

    @Override
    public boolean contains(BlockPos pos) {
        return false;
    }

    @Override
    public CompoundNBT serializeNBT() {
        return null;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {

    }
}
