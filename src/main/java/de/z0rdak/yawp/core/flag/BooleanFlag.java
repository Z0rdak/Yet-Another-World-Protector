package de.z0rdak.yawp.core.flag;

import net.minecraft.nbt.CompoundNBT;

import static de.z0rdak.yawp.core.flag.FlagType.BOOLEAN_FLAG;

/**
 * A simple boolean state flag.
 * The inverted flag would suffice and invert the implicit presence of the flag
 */
public class BooleanFlag extends AbstractFlag {

    public BooleanFlag(CompoundNBT nbt){
        super(nbt);
        this.deserializeNBT(nbt);
    }

    public BooleanFlag(String flag, boolean override) {
        super(flag, BOOLEAN_FLAG, override);
    }

    public BooleanFlag(RegionFlag flag) {
        super(flag.name, flag.type, false, FlagState.DENIED);
    }

    @Override
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = super.serializeNBT();
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        super.deserializeNBT(nbt);
    }
}
