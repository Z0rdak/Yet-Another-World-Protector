package de.z0rdak.yawp.core.flag;

import net.minecraft.nbt.CompoundNBT;

import static de.z0rdak.yawp.util.constants.RegionNBT.*;

public abstract class AbstractFlag implements IFlag {

    protected String flagIdentifier;
    protected String flagType;

    protected boolean isActive;
    protected boolean isAllowed;

    public AbstractFlag(String flagIdentifier, String flagType){
        this(flagIdentifier, flagType, false, true);
    }

    public AbstractFlag(String flagIdentifier, String flagType, boolean isAllowed){
        this(flagIdentifier, flagType, isAllowed, true);
    }

    public AbstractFlag(String flagIdentifier, String flagType, boolean isAllowed, boolean isActive){
        this.flagIdentifier = flagIdentifier;
        this.flagType = flagType;
        this.isActive = isActive;
        this.isAllowed = isAllowed;
    }

    public AbstractFlag(CompoundNBT nbt){
        this.deserializeNBT(nbt);
    }

    @Override
    public String getFlagType() {
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
    public boolean isAllowed() {
        return this.isAllowed;
    }

    @Override
    public void setAllowed(boolean allowed) {
        this.isAllowed = allowed;
    }

    @Override
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = new CompoundNBT();
        nbt.putString(FLAG_NAME, this.flagIdentifier);
        nbt.putBoolean(FLAG_ACTIVE, this.isActive);
        nbt.putBoolean(IS_ALLOWED, this.isAllowed);
        nbt.putString(FLAG_REGISTRY_NAME, this.flagType);
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        this.flagIdentifier = nbt.getString(FLAG_NAME);
        this.isActive = nbt.getBoolean(FLAG_ACTIVE);
        this.isAllowed = nbt.getBoolean(IS_ALLOWED);
        this.flagType = nbt.getString(FLAG_REGISTRY_NAME);
    }
}
