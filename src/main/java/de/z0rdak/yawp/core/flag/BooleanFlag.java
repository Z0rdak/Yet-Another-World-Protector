package de.z0rdak.yawp.core.flag;

import net.minecraft.nbt.CompoundTag;

import static de.z0rdak.yawp.core.flag.FlagType.BOOLEAN_FLAG;

/**
 * A simple boolean state flag.
 */
public class BooleanFlag extends AbstractFlag {

    public BooleanFlag(CompoundTag nbt) {
        super(nbt);
        this.deserializeNBT(provider, nbt);
    }

    public BooleanFlag(String flag, boolean isAllowed) {
        super(flag, BOOLEAN_FLAG, isAllowed);
    }

    public BooleanFlag(RegionFlag flag) {
        super(flag.name, flag.type, false, true);
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

    @Override
    public boolean isAllowed(Object... args) {
        return isActive() && (this.isInverted());
    }
}
