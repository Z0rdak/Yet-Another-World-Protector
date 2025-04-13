package de.z0rdak.yawp.core.flag;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import net.minecraft.nbt.CompoundTag;

import static de.z0rdak.yawp.constants.serialization.RegionNbtKeys.*;

public abstract class Flag implements IFlag {

    public static Codec<IFlag> CODEC = RecordCodecBuilder.create(
            instance -> instance.group(
                    Codec.STRING.fieldOf("name")
                            .forGetter(IFlag::getName),
                    Codec.STRING.fieldOf("type")
                            .forGetter(f -> f.getType().flagType),
                    Codec.STRING.fieldOf("state")
                            .forGetter(f -> f.getState().name),
                    Codec.BOOL.fieldOf("override")
                            .forGetter(IFlag::doesOverride),
                    FlagMessage.CODEC.fieldOf("msg")
                            .forGetter(IFlag::getFlagMsg)
                    ).apply(instance, (name, type, state, override, flagMessage) -> {
                        var flagType = FlagType.of(type);
                        switch (flagType) {
                            case BOOLEAN_FLAG -> {
                                return new BooleanFlag(RegionFlag.fromId(name), FlagState.from(state), override, flagMessage);
                            }
                            default -> throw new IllegalStateException("Unexpected value: " + flagType);
                        }
                    }
            ));

    protected String name;
    protected FlagType type;
    protected FlagState state;
    protected boolean doesOverride;
    protected FlagMessage msg;

    public Flag(String name, FlagType type, boolean override) {
        this(name, type, override, FlagState.DENIED);
    }

    public Flag(String name, FlagType type, boolean override, FlagState state) {
        this.name = name;
        this.type = type;
        this.state = state;
        this.doesOverride = override;
        this.msg = FlagMessage.DEFAULT_FLAG_MSG;
    }

    public Flag(String name, FlagType type) {
        this(name, type, false, FlagState.DENIED);
    }

    public Flag(String name, FlagType type, boolean override, FlagState state, String msg) {
        this(name, type, override, state);
        this.msg = new FlagMessage(msg);
    }

    public Flag(String name, FlagType type, boolean override, FlagState state, FlagMessage msg) {
        this(name, type, override, state);
        this.msg = msg;
    }

    public Flag(CompoundTag nbt) {
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
        this.name = nbt.getString(FLAG_NAME).orElseThrow();
        // Note: this is here for compatibility for the jump from 0.0.3.0-beta1 to 0.0.4.0-beta1
        // The state was not saved in the nbt before, there was a boolean flag instead
        if (nbt.contains(FLAG_STATE)) {
            this.state = FlagState.from(nbt.getString(FLAG_STATE).orElseThrow());
        } else {
            if (nbt.contains(FLAG_ACTIVE)) {
                boolean active = nbt.getBoolean(FLAG_ACTIVE).orElseThrow();
                if (active) {
                    this.state = FlagState.DENIED;
                } else {
                    this.state = FlagState.DISABLED;
                }
            } else {
                this.state = FlagState.DISABLED;
            }
        }
        this.doesOverride = nbt.getBoolean(OVERRIDE).orElseThrow();
        this.type = FlagType.of(nbt.getString(FLAG_TYPE).orElseThrow());
        this.msg = new FlagMessage(nbt.getCompound(FLAG_MSG).orElseThrow());
    }

    @Override
    public int compareTo(IFlag o) {
        int nameComparisonRes = this.name.compareTo(o.getName());
        int stateResult = this.state.compareTo(o.getState());
        return nameComparisonRes + stateResult;
    }
}
