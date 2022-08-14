package de.z0rdak.regionshield.core.flag;

import de.z0rdak.regionshield.util.constants.RegionNBT;
import net.minecraft.nbt.CompoundNBT;
import org.apache.commons.lang3.NotImplementedException;

/**
 * A simple boolean state flag.
 */
public class ConditionFlag extends AbstractFlag {

    public static final String FLAG_REGISTRY_NAME = "ConditionFlag";
    private boolean isAllowed;

    public ConditionFlag(String flag, boolean isAllowed) {
        super(flag, FLAG_REGISTRY_NAME);
        this.isAllowed = isAllowed;
    }

    // TODO:
    @Override
    public String getFlagDescription() {
        throw new NotImplementedException("");
    }

    @Override
    public boolean isAllowed() {
        return this.isAllowed;
    }

    @Override
    public void setAllowed(boolean allowed) {
        this.isAllowed = allowed;
    }

    @Override
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = super.serializeNBT();
        nbt.putBoolean(RegionNBT.IS_ALLOWED, this.isAllowed);
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        super.deserializeNBT(nbt);
        this.isAllowed = nbt.getBoolean(RegionNBT.IS_ALLOWED);
    }
}
