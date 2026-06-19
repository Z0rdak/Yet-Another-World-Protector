package de.z0rdak.yawp.core.flag;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import de.z0rdak.yawp.api.FlagRegister;

public abstract class FlagValue implements IFlag {

    public static Codec<IFlag> CODEC = RecordCodecBuilder.create(
            instance -> instance.group(
                    Codec.STRING.fieldOf("name")
                            .forGetter(IFlag::getName),
                    Codec.STRING.fieldOf("state")
                            .forGetter(f -> f.getState().name),
                    Codec.BOOL.fieldOf("override")
                            .forGetter(IFlag::doesOverride),
                    FlagMessage.CODEC.fieldOf("msg")
                            .forGetter(IFlag::getFlagMsg)
                    ).apply(instance, (name, state, override, flagMessage) ->
                    new BooleanFlag(FlagRegister.byId(name), FlagState.from(state), override, flagMessage)
            ));

    protected String name;
    protected FlagState state;
    protected boolean doesOverride;
    protected FlagMessage msg;

    public FlagValue(String name, boolean override) {
        this(name, override, FlagState.DENIED);
    }

    public FlagValue(String name, boolean override, FlagState state) {
        this.name = name;
        this.state = state;
        this.doesOverride = override;
        this.msg = FlagMessage.DEFAULT_FLAG_MSG;
    }

    public FlagValue(String name) {
        this(name,false, FlagState.DENIED);
    }

    public FlagValue(String name, boolean override, FlagState state, String msg) {
        this(name, override, state);
        this.msg = new FlagMessage(msg);
    }

    public FlagValue(String name, boolean override, FlagState state, FlagMessage msg) {
        this(name, override, state);
        this.msg = msg;
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
    public int compareTo(IFlag o) {
        int nameComparisonRes = this.name.compareTo(o.getName());
        int stateResult = this.state.compareTo(o.getState());
        return nameComparisonRes + stateResult;
    }
}
