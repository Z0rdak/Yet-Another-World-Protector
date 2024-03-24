package de.z0rdak.yawp.core.flag;

import net.minecraft.nbt.CompoundTag;

import static de.z0rdak.yawp.util.constants.RegionNBT.*;

public abstract class AbstractFlag implements IFlag {

    protected String name;
    protected FlagType type;
    protected boolean isActive;
    protected boolean doesOverride;
    protected FlagMessage msg;

    public AbstractFlag(String name, FlagType type, boolean override) {
        this(name, type, override, true);
    }

    public AbstractFlag(String name, FlagType type, boolean override, boolean isActive) {
        this.name = name;
        this.type = type;
        this.isActive = isActive;
        this.doesOverride = override;
        this.msg = FlagMessage.DEFAULT_FLAG_MSG;
    }

    public AbstractFlag(String name, FlagType type) {
        this(name, type, false, true);
    }

    public AbstractFlag(String name, FlagType type, boolean override, boolean isActive, String msg) {
        this(name, type, override, isActive);
        this.msg = new FlagMessage(msg);
    }

    public AbstractFlag(CompoundTag nbt) {
        this.deserializeNBT(nbt);
    }

    @Override
    public FlagType getType() {
        return this.type;
    }

    @Override
    public String getName() {
        return this.name;
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
    public boolean doesOverride() {
        return this.doesOverride;
    }

    @Override
    public void setOverride(boolean override) {
        this.doesOverride = override;
    }

    @Override
    public FlagMessage getFlagMsg() {
        return this.msg;
    }

    @Override
    public void setFlagMsg(FlagMessage msg) {
        this.msg = msg;
    }

    @Override
    public CompoundTag serializeNBT() {
        CompoundTag nbt = new CompoundTag();
        nbt.putString(FLAG_NAME, this.name);
        nbt.putBoolean(FLAG_ACTIVE, this.isActive);
        nbt.putBoolean(OVERRIDE, this.doesOverride);
        nbt.putString(FLAG_TYPE, this.type.flagType);
        nbt.put(FLAG_MSG, this.msg.serializeNBT());
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        this.name = nbt.getString(FLAG_NAME);
        this.isActive = nbt.getBoolean(FLAG_ACTIVE);
        this.doesOverride = nbt.getBoolean(OVERRIDE);
        this.type = FlagType.of(nbt.getString(FLAG_TYPE));
        this.msg = new FlagMessage(nbt.getCompound(FLAG_MSG));
    }

    @Override
    public int compareTo(IFlag o) {
        int nameComparsionRes = this.name.compareTo(o.getName());
        int activeComparsionRes = this.isActive && !o.isActive() ? 1 : !this.isActive && o.isActive() ? -1 : 0;
        return nameComparsionRes + activeComparsionRes;
    }
}
