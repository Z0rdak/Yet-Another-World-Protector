package de.z0rdak.yawp.core.region;

/**
 * Enum to identify region type. Mainly used for region deserialization.
 */
public enum RegionType {

    GLOBAL("global"),
    DIMENSION("dimension"),
    LOCAL("local");

    public final String type;

    RegionType(String type){
        this.type = type;
    }

    public static RegionType of(String name) {
        switch (name) {
            case "local":
                return LOCAL;
            case "dimension":
                return DIMENSION;
            case "global":
                return GLOBAL;
            default:
                return null;
        }
    }
}
