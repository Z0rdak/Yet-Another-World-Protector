package de.z0rdak.yawp.core.flag;

import java.util.Arrays;
import java.util.Set;
import java.util.stream.Collectors;

public enum FlagType {

    BOOLEAN_FLAG("BooleanFlag"),
    @Deprecated
    LIST_FLAG("ListFlag"),
    @Deprecated
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

    public static Set<String> getFlagTypes() {
        return Arrays.stream(FlagType.values())
                .map(type -> type.flagType)
                .collect(Collectors.toSet());
    }
}
