package de.z0rdak.yawp.core.flag;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public enum FlagState {
    /**
     * The flag is disabled, duh!
     */
    DISABLED("Disabled"),
    /**
     * The flag is allowed and implicitly active.
     */
    ALLOWED("Allowed"),
    /**
     * The flag is denied and implicitly active.
     */
    DENIED("Denied"),
    /**
     * The flag is undefined and thus not present in a flag container. <br>
     * This state is used to indicate that a flag is not set during flag checks. <br>
     * It is not accessible for users to set this state.
     *
     * @see RegionFlags
     */
    UNDEFINED("Undefined");

    public final String name;

    FlagState(String name) {
        this.name = name;
    }

    public static FlagState from(boolean value) {
        return value ? ALLOWED : DENIED;
    }

    /**
     * Converts a string to a flag state. <br>
     * Allowed values are: ALLOWED, DENIED, DISABLED, UNDEFINED
     *
     * @param value the string to convert
     * @return the corresponding flag state
     * @throws IllegalArgumentException if the value is invalid
     */
    public static FlagState from(String value) throws IllegalArgumentException {
        switch (value.toLowerCase()) {
            case "allowed":
                return ALLOWED;
            case "denied":
                return DENIED;
            case "disabled":
                return DISABLED;
            case "undefined":
                return UNDEFINED;
            default:
                throw new IllegalArgumentException("Unknown flag state: " + value);
        }
    }

    /**
     * List of valid flag states for CLI to set the flag state.
     * Undefined is excluded because it should not be accessible for users.
     *
     * @return list of valid flag states
     */
    public static List<String> ValidFlagStates() {
        // exclude UNDEFINED
        return Arrays.stream(FlagState.values()).filter(state -> state != UNDEFINED).map(fs -> fs.name).collect(Collectors.toList());
    }

    public static boolean validLoggingStates(String flagState) {
        return Stream.of(ALLOWED, DENIED).anyMatch(state -> state.name.equalsIgnoreCase(flagState));
    }

    public static FlagState invert(FlagState state) {
        switch (state) {
            case ALLOWED:
                return DENIED;
            case DENIED:
                return ALLOWED;
            default:
                return state;
        }
    }
}
