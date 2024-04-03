package de.z0rdak.yawp.core.flag;

public enum FlagState {
    DISABLED,
    /**
     * The flag is allowed and implicitly active.
     */
    ALLOWED,
    /**
     * The flag is denied and implicitly active.
     */
    DENIED,
    UNDEFINED;

    public static FlagState from(boolean value) {
        return value ? ALLOWED : DENIED;
    }

    /**
     * Converts a string to a flag state. <br></br>
     * Allowed values are: ALLOWED, DENIED, DISABLED, UNDEFINED
     *
     * @param value the string to convert
     * @return the corresponding flag state
     * @throws IllegalArgumentException if the value is invalid
     */
    public static FlagState from(String value) throws IllegalArgumentException {
        switch (value) {
            case "ALLOWED":
                return ALLOWED;
            case "DENIED":
                return DENIED;
            case "DISABLED":
                return DISABLED;
            case "UNDEFINED":
                return UNDEFINED;
            default:
                throw new IllegalArgumentException("Unknown flag state: " + value);
        }
    }
}
