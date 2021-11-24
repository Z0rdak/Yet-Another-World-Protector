package de.z0rdak.regionshield.common.util;

public final class StickUtil {

    private StickUtil() {
    }

    public static final String STICK_TYPE = "stick_type";

    public enum StickType {

        MARKER("RegionMarker"),
        FLAG_STICK("FlagStick"),
        REGION_STICK("RegionStick");

        public final String stickName;

        StickType(String name) {
            this.stickName = name;
        }

        public static StickType toStickType(String name) {
            switch (name) {
                case "RegionMarker":
                    return StickType.MARKER;
                case "FlagStick":
                    return StickType.FLAG_STICK;
                case "RegionStick":
                    return StickType.REGION_STICK;
                default:
                    return null;
            }
        }
    }
}
