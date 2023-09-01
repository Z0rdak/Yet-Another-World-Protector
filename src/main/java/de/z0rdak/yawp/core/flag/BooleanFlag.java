package de.z0rdak.yawp.core.flag;

import net.minecraft.nbt.CompoundTag;

import static de.z0rdak.yawp.core.flag.FlagType.BOOLEAN_FLAG;

/**
 * A simple boolean state flag.
 */
public class BooleanFlag extends AbstractFlag {

    public BooleanFlag(CompoundTag nbt) {
        super(nbt);
        this.deserializeNBT(nbt);
    }

    public BooleanFlag(String flag, boolean override) {
        super(flag, BOOLEAN_FLAG, override);
    }

    public BooleanFlag(RegionFlag flag) {
        super(flag.name, flag.type, false, true);
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

    @Override
    public boolean isAllowed(Object... args) {
        return isActive() && (this.doesOverride());
    }
}
