package de.z0rdak.yawp.util;

public enum StickType {

    UNKNOWN("Unknown"),
    MARKER("RegionMarker");

    public final String stickName;

    StickType(String name) {
        this.stickName = name;
    }

    public static StickType of(String name) {
        if (name.equals("RegionMarker")) {
            return MARKER;
        }
        return UNKNOWN;
    }
}
