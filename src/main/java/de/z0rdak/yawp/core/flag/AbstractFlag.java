package de.z0rdak.yawp.core.flag;

import net.minecraft.nbt.CompoundNBT;

import static de.z0rdak.yawp.util.constants.RegionNBT.*;

public abstract class AbstractFlag implements IFlag {

    protected String name;

    protected FlagType type;

    protected boolean isActive;
    protected boolean inverted;

    protected FlagMessage msg;

    public AbstractFlag(String name, FlagType type, boolean inverted) {
        this(name, type, inverted, true);
    }

    public AbstractFlag(String name, FlagType type, boolean inverted, boolean isActive) {
        this.name = name;
        this.type = type;
        this.isActive = isActive;
        this.inverted = inverted;
        this.msg = FlagMessage.DEFAULT_FLAG_MSG;
    }

    public AbstractFlag(String name, FlagType type) {
        this(name, type, false, true);
    }

    public AbstractFlag(String name, FlagType type, boolean inverted, boolean isActive, String msg) {
        this(name, type, inverted, isActive);
        this.msg = new FlagMessage(msg);
    }

    public AbstractFlag(CompoundNBT nbt) {
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
    public boolean isInverted() {
        return this.inverted;
    }

    @Override
    public void setInverted(boolean inverted) {
        this.inverted = inverted;
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
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = new CompoundNBT();
        nbt.putString(FLAG_NAME, this.name);
        nbt.putBoolean(FLAG_ACTIVE, this.isActive);
        nbt.putBoolean(IS_INVERTED, this.inverted);
        nbt.putString(FLAG_TYPE, this.type.flagType);
        nbt.put(FLAG_MSG, this.msg.serializeNBT());
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        this.name = nbt.getString(FLAG_NAME);
        this.isActive = nbt.getBoolean(FLAG_ACTIVE);
        this.inverted = nbt.getBoolean(IS_INVERTED);
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
