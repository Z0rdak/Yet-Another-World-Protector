package de.z0rdak.yawp.core.flag;

import net.minecraft.nbt.CompoundTag;

import static de.z0rdak.yawp.constants.serialization.RegionNbtKeys.*;

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
    public CompoundTag serializeNBT() {
        CompoundTag nbt = new CompoundTag();
        nbt.putString(FLAG_NAME, this.name);
        nbt.putString(FLAG_STATE, this.state.name);
        nbt.putBoolean(OVERRIDE, this.doesOverride);
        nbt.putString(FLAG_TYPE, this.type.flagType);
        nbt.put(FLAG_MSG, this.msg.serializeNBT());
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        this.name = nbt.getString(FLAG_NAME);
        // Note: this is here for compatibility for the jump from 0.0.3.0-beta1 to 0.0.4.0-beta1
        // The state was not saved in the nbt before, there was a boolean flag instead
        if (nbt.contains(FLAG_STATE)) {
            this.state = FlagState.from(nbt.getString(FLAG_STATE));
        } else {
            if (nbt.contains(FLAG_ACTIVE)) {
                boolean active = nbt.getBoolean(FLAG_ACTIVE);
                if (active) {
                    this.state = FlagState.DENIED;
                } else {
                    this.state = FlagState.DISABLED;
                }
            } else {
                this.state = FlagState.DISABLED;
            }
        }
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
