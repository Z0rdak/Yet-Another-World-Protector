package de.z0rdak.yawp.core.flag;

import net.minecraft.nbt.NbtCompound;

/**
 * A simple boolean state flag.
 */
public class BooleanFlag extends AbstractFlag {

    public BooleanFlag(NbtCompound nbt) {
        super(nbt);
        this.deserializeNBT(nbt);
    }

    public BooleanFlag(RegionFlag flag, FlagState state, boolean override) {
        super(flag.name, flag.type, override, state);
    }

    public BooleanFlag(RegionFlag flag) {
        super(flag.name, flag.type, false, FlagState.DENIED);
    }

    @Override
    public NbtCompound serializeNBT() {
        NbtCompound nbt = super.serializeNBT();
        return nbt;
    }

    @Override
    public void deserializeNBT(NbtCompound nbt) {
        super.deserializeNBT(nbt);
    }
}
