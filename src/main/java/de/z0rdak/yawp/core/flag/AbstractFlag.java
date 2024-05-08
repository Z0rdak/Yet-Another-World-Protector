package de.z0rdak.yawp.core.flag;

import de.z0rdak.yawp.util.constants.RegionNBT;
import net.minecraft.nbt.CompoundTag;

import static de.z0rdak.yawp.util.constants.RegionNBT.*;

public abstract class AbstractFlag implements IFlag {

    protected String flagIdentifier;
    protected FlagType flagType;

    protected boolean isActive;
    protected boolean inverted;

    public AbstractFlag(String flagIdentifier, FlagType flagType) {
        this(flagIdentifier, flagType, false, true);
    }

    public AbstractFlag(String flagIdentifier, FlagType flagType, boolean inverted) {
        this(flagIdentifier, flagType, inverted, true);
    }

    public AbstractFlag(String flagIdentifier, FlagType flagType, boolean inverted, boolean isActive) {
        this.flagIdentifier = flagIdentifier;
        this.flagType = flagType;
        this.isActive = isActive;
        this.inverted = inverted;
    }

    public AbstractFlag(CompoundTag nbt) {
        this.deserializeNBT(nbt);
    }

    @Override
    public FlagType getFlagType() {
        return this.flagType;
    }

    @Override
    public String getFlagIdentifier() {
        return this.flagIdentifier;
    }

    @Override
    public boolean isActive() {
        return this.isActive;
    }

    @Override
    public void setIsActive(boolean active) {
        this.isActive = active;
    }

    @Override
    public boolean isInverted() {
        return this.inverted;
    }

    @Override
    public void setInverted(boolean inverted) {
        this.inverted = inverted;
    }

    @Override
    public CompoundTag serializeNBT() {
        CompoundTag nbt = new CompoundTag();
        nbt.putString(FLAG_NAME, this.flagIdentifier);
        nbt.putBoolean(FLAG_ACTIVE, this.isActive);
        nbt.putBoolean(IS_INVERTED, this.inverted);
        nbt.putString(RegionNBT.FLAG_TYPE, this.flagType.flagType);
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        this.flagIdentifier = nbt.getString(FLAG_NAME);
        this.isActive = nbt.getBoolean(FLAG_ACTIVE);
        this.inverted = nbt.getBoolean(IS_INVERTED);
        this.flagType = FlagType.of(nbt.getString(RegionNBT.FLAG_TYPE));
    }

    @Override
    public int compareTo(IFlag o) {
        int nameComparsionRes = this.flagIdentifier.compareTo(o.getFlagIdentifier());
        int activeComparsionRes = this.isActive && !o.isActive() ? 1 : !this.isActive && o.isActive() ? -1 : 0;
        return nameComparsionRes + activeComparsionRes;
    }
}
