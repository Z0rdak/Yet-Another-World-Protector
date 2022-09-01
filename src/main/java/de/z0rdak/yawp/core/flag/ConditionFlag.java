package de.z0rdak.yawp.core.flag;

import net.minecraft.nbt.CompoundTag;

/**
 * A simple boolean state flag.
 */
public class ConditionFlag extends AbstractFlag {

    public static final String CONDITION_FLAG = "ConditionFlag";

    public ConditionFlag(CompoundTag nbt){
        super(nbt);
    }

    public ConditionFlag(String flag, boolean isAllowed) {
        super(flag, CONDITION_FLAG, isAllowed);
    }

    @Override
    public CompoundTag serializeNBT() {
        return super.serializeNBT();
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        super.deserializeNBT(nbt);
    }
}
