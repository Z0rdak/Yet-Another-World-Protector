package de.z0rdak.yawp.core.flag;

import de.z0rdak.yawp.api.Flag;

/**
 * A simple boolean state flag.
 */
public class BooleanFlag extends FlagValue {

    public BooleanFlag(Flag flag, FlagState state, boolean override) {
        super(flag.name(), override, state);
    }

    public BooleanFlag(Flag flag, FlagState state, FlagMessage msg, boolean override) {
        this(flag, state, override);
        this.msg = msg;
    }

    public BooleanFlag(Flag regionFlag, FlagState state, boolean override, FlagMessage flagMessage) {
        this(regionFlag, state, flagMessage, override);
    }

    public BooleanFlag(Flag flag) {
        super(flag.name(), false, FlagState.DENIED);
    }
}
