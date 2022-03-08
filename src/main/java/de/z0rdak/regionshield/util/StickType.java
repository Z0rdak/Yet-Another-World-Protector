package de.z0rdak.regionshield.util;

public enum StickType {

    UNKNOWN("Unknown"),
    MARKER("RegionMarker"),
    FLAG_STICK("FlagStick"),
    REGION_STICK("RegionStick");

    public final String stickName;

    StickType(String name) {
        this.stickName = name;
    }

    public static StickType of(String name) {
        switch (name) {
            case "RegionMarker":
                return MARKER;
            case "FlagStick":
                return FLAG_STICK;
            case "RegionStick":
                return REGION_STICK;
            default:
                return UNKNOWN;
        }
    }
}
