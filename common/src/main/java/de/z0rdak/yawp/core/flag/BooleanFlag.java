package de.z0rdak.yawp.core.flag;

import net.minecraft.nbt.CompoundTag;

/**
 * A simple boolean state flag.
 */
public class BooleanFlag extends AbstractFlag {

    public BooleanFlag(CompoundTag nbt) {
        super(nbt);
        this.deserializeNBT(nbt);
    }

    public BooleanFlag(RegionFlag flag, FlagState state, boolean override) {
        super(flag.name, flag.type, override, state);
    }

    public BooleanFlag(RegionFlag flag, FlagState state, FlagMessage msg, boolean override) {
        this(flag, state, override);
        this.msg = msg;
    }

    public BooleanFlag(RegionFlag flag) {
        super(flag.name, flag.type, false, FlagState.DENIED);
    }

    @Override
    public CompoundTag serializeNBT() {
        CompoundTag nbt = super.serializeNBT();
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        super.deserializeNBT(nbt);
    }
}
