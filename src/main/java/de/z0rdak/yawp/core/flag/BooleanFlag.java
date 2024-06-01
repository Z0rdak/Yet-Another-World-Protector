package de.z0rdak.yawp.core.flag;

import net.minecraft.core.HolderLookup;
import net.minecraft.nbt.CompoundTag;

/**
 * A simple boolean state flag.
 */
public class BooleanFlag extends AbstractFlag {

    public BooleanFlag(HolderLookup.Provider provider, CompoundTag nbt) {
        super(provider, nbt);
        this.deserializeNBT(provider, nbt);
    }

    public BooleanFlag(RegionFlag flag, FlagState state, boolean override) {
        super(flag.name, flag.type, override, state);
    }

    public BooleanFlag(RegionFlag flag) {
        super(flag.name, flag.type, false, FlagState.DENIED);
    }

    public BooleanFlag(BooleanFlag flag) {
        super(flag.name, flag.type, flag.doesOverride, flag.state);
        this.msg = new FlagMessage(flag.msg);
    }

    @Override
    public CompoundTag serializeNBT(HolderLookup.Provider provider) {
        CompoundTag nbt = super.serializeNBT(provider);
        return nbt;
    }

    @Override
    public void deserializeNBT(HolderLookup.Provider provider, CompoundTag nbt) {
        super.deserializeNBT(provider, nbt);
    }
}
