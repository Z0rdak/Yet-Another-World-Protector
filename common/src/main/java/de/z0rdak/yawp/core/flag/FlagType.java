package de.z0rdak.yawp.core.flag;

import java.util.Arrays;
import java.util.Set;
import java.util.stream.Collectors;

public enum FlagType {

    BOOLEAN_FLAG("BooleanFlag");

    public final String flagType;

    FlagType(String flagType) {
        this.flagType = flagType;
    }

    public static FlagType of(String name) {
        switch (name) {
            case "BooleanFlag":
                return BOOLEAN_FLAG;
            default:
                throw new IllegalArgumentException("Unknown FlagType: " + name);
        }
    }

    public static Set<String> getFlagTypes() {
        return Arrays.stream(FlagType.values())
                .map(type -> type.flagType)
                .collect(Collectors.toSet());
    }
}
