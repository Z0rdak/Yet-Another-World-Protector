package de.z0rdak.yawp.core.region;

/**
 * Enum to identify region type. Mainly used for region deserialization.
 */
public enum RegionType {

    /**
     * Used for a global region, spanning over all dimensional regions. NOT yet used.
     */
    GLOBAL("global"),
    /**
     * Used for dimension regions. Instances of DimensionalRegion.
     */
    DIMENSION("dimension"),
    /**
     * Used to for local, markable regions. Instances of IMarkableRegion.
     */
    LOCAL("local"),
    /**
     * Used to mark a template region. NOT yet used.
     */
    TEMPLATE("template");

    public final String type;

    RegionType(String type) {
        this.type = type;
    }

    public static RegionType of(String name) {
        switch (name) {
            case "local":
            case "region":
                return LOCAL;
            case "dimension":
            case "dim":
                return DIMENSION;
            case "global":
                return GLOBAL;
            default:
                return null;
        }
    }
}
