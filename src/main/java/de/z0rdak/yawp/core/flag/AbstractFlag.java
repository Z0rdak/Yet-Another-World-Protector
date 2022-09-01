package de.z0rdak.yawp.core.flag;

import net.minecraft.nbt.CompoundTag;
import net.minecraftforge.registries.ForgeRegistryEntry;
import org.apache.commons.lang3.NotImplementedException;

import static de.z0rdak.yawp.util.constants.RegionNBT.*;

public abstract class AbstractFlag extends ForgeRegistryEntry<AbstractFlag> implements IFlag {

    private String flagIdentifier;
    private String flagType;
    private boolean isActive;
    private boolean isAllowed;

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

    public AbstractFlag(CompoundTag nbt){
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

    // TODO:
    @Override
    public String getFlagDescription() {
        throw new NotImplementedException("");
    }

    @Override
    public CompoundTag serializeNBT() {
        CompoundTag nbt = new CompoundTag();
        nbt.putString(FLAG_NAME, this.flagIdentifier);
        nbt.putBoolean(FLAG_ACTIVE, this.isActive);
        nbt.putBoolean(IS_ALLOWED, this.isAllowed);
        nbt.putString(FLAG_REGISTRY_NAME, this.flagType);
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        this.flagIdentifier = nbt.getString(FLAG_NAME);
        this.isActive = nbt.getBoolean(FLAG_ACTIVE);
        this.isAllowed = nbt.getBoolean(IS_ALLOWED);
        this.flagType = nbt.getString(FLAG_REGISTRY_NAME);
    }
}
