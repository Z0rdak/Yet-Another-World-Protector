package de.z0rdak.yawp.core.flag;

import net.minecraft.nbt.CompoundNBT;

/**
 * A simple boolean state flag.
 */
public class ConditionFlag extends AbstractFlag {

    public static final String CONDITION_FLAG = "ConditionFlag";

    public ConditionFlag(CompoundNBT nbt){
        super(nbt);
    }

    public ConditionFlag(String flag, boolean isAllowed) {
        super(flag, CONDITION_FLAG, isAllowed);
    }

    @Override
    public CompoundNBT serializeNBT() {
        return super.serializeNBT();
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        super.deserializeNBT(nbt);
    }
}
