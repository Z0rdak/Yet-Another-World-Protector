package de.z0rdak.yawp.core.flag;

import net.minecraft.nbt.CompoundNBT;

import static de.z0rdak.yawp.core.flag.FlagType.BOOLEAN_FLAG;
import static de.z0rdak.yawp.util.constants.RegionNBT.FLAG_VALUE;

/**
 * A simple boolean state flag.
 * The inverted flag would suffice and invert the implicit presence of the flag
 */
public class BooleanFlag extends AbstractFlag {

    public BooleanFlag(CompoundNBT nbt){
        super(nbt);
        this.deserializeNBT(nbt);
    }

    public BooleanFlag(String flag, boolean isAllowed) {
        super(flag, BOOLEAN_FLAG, isAllowed);
    }

    public BooleanFlag(RegionFlag flag) {
        super(flag.name, flag.type, false, false);
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

    @Override
    public boolean isAllowed(Object... args) {
        return isActive() && (this.isInverted());
    }
}
