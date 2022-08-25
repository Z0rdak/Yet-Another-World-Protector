package de.z0rdak.yawp.core.flag;

import net.minecraft.nbt.CompoundNBT;
import net.minecraftforge.registries.ForgeRegistryEntry;
import org.apache.commons.lang3.NotImplementedException;

import static de.z0rdak.yawp.util.constants.RegionNBT.*;

public abstract class AbstractFlag extends ForgeRegistryEntry<AbstractFlag> implements IFlag {

    private String flagName;
    private String flagType;
    private boolean isActive;
    private boolean isAllowed;

    public AbstractFlag(String flagIdentifier, String flagType, boolean isAllowed){
        this.flagName = flagIdentifier;
        this.flagType = flagType;
        this.isActive = true;
        this.isAllowed = isAllowed;
    }

    public AbstractFlag(String modName, String flagName, String flagType){
        this.flagName = modName + ":" + flagName;
        this.flagType = flagType;
        this.isActive = true;
    }

    @Override
    public String getFlagType() {
        return this.flagType;
    }

    @Override
    public String getFlagName() {
        return this.flagName;
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

    // TODO:
    @Override
    public String getFlagDescription() {
        throw new NotImplementedException("");
    }

    @Override
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = new CompoundNBT();
        nbt.putString(FLAG_NAME, this.flagName);
        nbt.putBoolean(FLAG_ACTIVE, this.isActive);
        nbt.putBoolean(IS_ALLOWED, this.isAllowed);
        nbt.putString(FLAG_REGISTRY_NAME, this.flagType);
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        this.flagName = nbt.getString(FLAG_NAME);
        this.isActive = nbt.getBoolean(FLAG_ACTIVE);
        this.isAllowed = nbt.getBoolean(IS_ALLOWED);
        this.flagType = nbt.getString(FLAG_REGISTRY_NAME);
    }
}