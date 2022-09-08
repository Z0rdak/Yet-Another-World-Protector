package de.z0rdak.yawp.core.flag;

public enum FlagType {

    BOOLEAN_FLAG("BooleanFlag"),
    LIST_FLAG("ListFlag"),
    INT_FLAG("IntFlag");

    public final String flagType;

    FlagType(String flagType) {
        this.flagType = flagType;
    }

    public static FlagType of(String name) {
        switch (name) {
            case "BooleanFlag":
                return BOOLEAN_FLAG;
            case "ListFlag":
                return LIST_FLAG;
            case "IntFlag":
                return INT_FLAG;
            default:
                return null;
        }
    }
}
