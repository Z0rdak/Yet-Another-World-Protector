package de.z0rdak.yawp.core.flag;

import net.minecraft.nbt.CompoundTag;

/**
 * A simple boolean state flag.
 */
public class ConditionFlag extends AbstractFlag {

    public static final String FLAG_REGISTRY_NAME = "ConditionFlag";

    public ConditionFlag(CompoundTag nbt){
        this("", false);
        this.deserializeNBT(nbt);
    }

    public ConditionFlag(String flag, boolean isAllowed) {
        super(flag, FLAG_REGISTRY_NAME, isAllowed);
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
