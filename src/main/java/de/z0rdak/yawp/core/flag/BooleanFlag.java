package de.z0rdak.yawp.core.flag;

import net.minecraft.nbt.NbtCompound;

import static de.z0rdak.yawp.core.flag.FlagType.BOOLEAN_FLAG;

/**
 * A simple boolean state flag.
 */
public class BooleanFlag extends AbstractFlag {

    public BooleanFlag(NbtCompound nbt) {
        super(nbt);
        this.deserializeNBT(nbt);
    }

    public BooleanFlag(String flag, boolean isAllowed) {
        super(flag, BOOLEAN_FLAG, isAllowed);
    }

    public BooleanFlag(RegionFlag flag) {
        super(flag.name, flag.type, false, true);
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

    @Override
    public boolean isAllowed(Object... args) {
        return isActive() && (this.isInverted());
    }
}
