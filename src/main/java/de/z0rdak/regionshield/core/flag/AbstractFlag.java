package de.z0rdak.regionshield.core.flag;

import net.minecraft.nbt.CompoundNBT;
import net.minecraftforge.common.util.INBTSerializable;
import net.minecraftforge.registries.ForgeRegistryEntry;

import static de.z0rdak.regionshield.util.constants.RegionNBT.*;

public abstract class AbstractFlag extends ForgeRegistryEntry<AbstractFlag> implements IFlag, INBTSerializable<CompoundNBT> {

    private String flagName;
    private String flagType;
    private boolean isActive;

    public AbstractFlag(String flagIdentifier, String flagType){
        this.flagName = flagIdentifier;
        this.flagType = flagType;
        this.isActive = true;
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
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = new CompoundNBT();
        nbt.putString(FLAG_NAME, this.flagName);
        nbt.putBoolean(FLAG_ACTIVE, this.isActive);
        nbt.putString(FLAG_REGISTRY_NAME, this.flagType);
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        this.flagName = nbt.getString(FLAG_NAME);
        this.isActive = nbt.getBoolean(FLAG_ACTIVE);
        this.flagType = nbt.getString(FLAG_REGISTRY_NAME);
    }
}
