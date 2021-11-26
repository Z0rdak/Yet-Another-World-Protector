package de.z0rdak.regionshield.common.core.area;

import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.vector.Vector3d;

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

    // todo: type for center focused areas?
    @Override
    public Vector3d getCenter() {
        return null;
    }
}
