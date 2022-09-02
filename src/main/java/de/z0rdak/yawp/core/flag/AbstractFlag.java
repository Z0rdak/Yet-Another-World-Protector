package de.z0rdak.yawp.core.flag;

import de.z0rdak.yawp.util.constants.RegionNBT;
import net.minecraft.nbt.CompoundNBT;

import static de.z0rdak.yawp.util.constants.RegionNBT.*;

public abstract class AbstractFlag implements IFlag {

    protected String flagIdentifier;
    protected FlagType flagType;

    protected boolean isActive;
    protected boolean inverted;

    public AbstractFlag(String flagIdentifier, FlagType flagType){
        this(flagIdentifier, flagType, false, true);
    }

    public AbstractFlag(String flagIdentifier, FlagType flagType, boolean inverted){
        this(flagIdentifier, flagType, inverted, true);
    }

    public AbstractFlag(String flagIdentifier, FlagType flagType, boolean inverted, boolean isActive){
        this.flagIdentifier = flagIdentifier;
        this.flagType = flagType;
        this.isActive = isActive;
        this.inverted = inverted;
    }

    public AbstractFlag(CompoundNBT nbt){
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
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = new CompoundNBT();
        nbt.putString(FLAG_NAME, this.flagIdentifier);
        nbt.putBoolean(FLAG_ACTIVE, this.isActive);
        nbt.putBoolean(IS_INVERTED, this.inverted);
        nbt.putString(RegionNBT.FLAG_TYPE, this.flagType.flagType);
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        this.flagIdentifier = nbt.getString(FLAG_NAME);
        this.isActive = nbt.getBoolean(FLAG_ACTIVE);
        this.inverted = nbt.getBoolean(IS_INVERTED);
        this.flagType = FlagType.of(nbt.getString(RegionNBT.FLAG_TYPE));
    }
}
