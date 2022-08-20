package de.z0rdak.regionshield.core.flag;

import net.minecraft.nbt.CompoundNBT;

/**
 * A simple boolean state flag.
 */
public class ConditionFlag extends AbstractFlag {

    public static final String FLAG_REGISTRY_NAME = "ConditionFlag";

    public ConditionFlag(CompoundNBT nbt){
        this("", false);
        this.deserializeNBT(nbt);
    }

    public ConditionFlag(String flag, boolean isAllowed) {
        super(flag, FLAG_REGISTRY_NAME, isAllowed);
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
