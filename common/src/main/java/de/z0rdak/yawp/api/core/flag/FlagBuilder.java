package de.z0rdak.yawp.api.core.flag;

import de.z0rdak.yawp.core.flag.*;
import org.jetbrains.annotations.NotNull;

import static de.z0rdak.yawp.core.flag.FlagMessage.DEFAULT_FLAG_MSG;
import static de.z0rdak.yawp.core.flag.RegionFlag.fromId;

public class FlagBuilder {

    private final RegionFlag flag;
    private FlagState state;
    private boolean override;
    private FlagMessage msg;

    public FlagBuilder(RegionFlag flag) {
        this.flag = flag;
        this.state = FlagState.DENIED;
        this.override = false;
        this.msg = DEFAULT_FLAG_MSG;
    }

    public FlagBuilder(String flagName) throws IllegalArgumentException {
        this(fromId(flagName));
    }

    public IFlag build() {
        return new BooleanFlag(this.flag, this.state, this.msg, this.override);
    }

    public FlagBuilder deactivated() {
        return this.withState(FlagState.DISABLED);
    }

    private FlagBuilder withState(FlagState state) {
        this.state = state;
        return this;
    }

    public FlagBuilder denied() {
        return this.withState(FlagState.DENIED);
    }

    public FlagBuilder allowed() {
        return this.withState(FlagState.ALLOWED);
    }

    public FlagBuilder withMsg(FlagMessage msg) {
        this.msg = msg;
        return this;
    }

    public FlagBuilder withMsg(@NotNull String msg, boolean mute) {
        this.msg = new FlagMessage(msg, mute);
        return this;
    }

    public FlagBuilder withMsg(@NotNull String msg) {
        return this.withMsg(msg, false);
    }

    public FlagBuilder override(boolean override) {
        this.override = override;
        return this;
    }

    public FlagBuilder override() {
        return this.override(true);
    }
}
