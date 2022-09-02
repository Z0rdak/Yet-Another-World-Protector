package de.z0rdak.yawp.core.flag;

import net.minecraft.nbt.CompoundNBT;

import static de.z0rdak.yawp.core.flag.FlagType.BOOLEAN_FLAG;
import static de.z0rdak.yawp.util.constants.RegionNBT.FLAG_VALUE;

/**
 * A simple boolean state flag.
 */
public class BooleanFlag extends AbstractFlag {

    /**
     * true -> allow
     * false -> deny
     */
    private boolean value;

    public BooleanFlag(CompoundNBT nbt){
        super(nbt);
        this.deserializeNBT(nbt);
    }

    public BooleanFlag(String flag, boolean isAllowed) {
        super(flag, BOOLEAN_FLAG, isAllowed);
        this.value = false;
    }

    public BooleanFlag(String flag, boolean isAllowed, boolean value) {
        super(flag, BOOLEAN_FLAG, isAllowed);
        this.value = value;
    }

    @Override
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = super.serializeNBT();
        nbt.putBoolean(FLAG_VALUE, value);
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        super.deserializeNBT(nbt);
        this.value = nbt.getBoolean(FLAG_VALUE);
    }

    public boolean isDenied() {
        return value;
    }

    public void setValue(boolean value) {
        this.value = value;
    }
}
