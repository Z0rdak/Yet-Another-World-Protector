package de.z0rdak.yawp.core.flag;

/**
 * A simple boolean state flag.
 */
public class BooleanFlag extends FlagValue {

    public BooleanFlag(RegionFlag flag, FlagState state, boolean override) {
        super(flag.name, flag.type, override, state);
    }

    public BooleanFlag(RegionFlag flag, FlagState state, FlagMessage msg, boolean override) {
        this(flag, state, override);
        this.msg = msg;
    }

    public BooleanFlag(RegionFlag regionFlag, FlagState state, Boolean override, FlagMessage flagMessage) {
        this(regionFlag, state, flagMessage, override);
    }

    public BooleanFlag(RegionFlag flag) {
        super(flag.name, flag.type, false, FlagState.DENIED);
    }
}
