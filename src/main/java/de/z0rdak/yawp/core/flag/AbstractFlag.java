package de.z0rdak.yawp.core.flag;

import net.minecraft.nbt.CompoundNBT;

import static de.z0rdak.yawp.util.constants.RegionNBT.*;

public abstract class AbstractFlag implements IFlag {

    protected String name;
    protected FlagType type;
    protected FlagState state;
    protected boolean doesOverride;
    protected FlagMessage msg;

    public AbstractFlag(String name, FlagType type, boolean override) {
        this(name, type, override, FlagState.DENIED);
    }

    public AbstractFlag(String name, FlagType type, boolean override, FlagState state) {
        this.name = name;
        this.type = type;
        this.state = state;
        this.doesOverride = override;
        this.msg = FlagMessage.DEFAULT_FLAG_MSG;
    }

    public AbstractFlag(String name, FlagType type) {
        this(name, type, false, FlagState.DENIED);
    }

    public AbstractFlag(String name, FlagType type, boolean override, FlagState state, String msg) {
        this(name, type, override, state);
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
        return this.state == FlagState.ALLOWED || this.state == FlagState.DENIED;
    }

    @Override
    public FlagState getState() {
        return this.state;
    }

    @Override
    public void setState(FlagState state) {
        this.state = state;
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
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = new CompoundNBT();
        nbt.putString(FLAG_NAME, this.name);
        nbt.putInt(FLAG_STATE, this.state.ordinal());
        nbt.putBoolean(OVERRIDE, this.doesOverride);
        nbt.putString(FLAG_TYPE, this.type.flagType);
        nbt.put(FLAG_MSG, this.msg.serializeNBT());
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        this.name = nbt.getString(FLAG_NAME);
        this.state = FlagState.values()[nbt.getInt(FLAG_STATE)];
        this.doesOverride = nbt.getBoolean(OVERRIDE);
        this.type = FlagType.of(nbt.getString(FLAG_TYPE));
        this.msg = new FlagMessage(nbt.getCompound(FLAG_MSG));
    }

    @Override
    public int compareTo(IFlag o) {
        int nameComparisonRes = this.name.compareTo(o.getName());
        int stateResult = this.state.compareTo(o.getState());
        return nameComparisonRes + stateResult;
    }
}
