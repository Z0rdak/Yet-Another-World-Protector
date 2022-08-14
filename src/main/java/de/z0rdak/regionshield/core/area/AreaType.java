package de.z0rdak.regionshield.core.area;

// TODO: registry or datapack for area with their area types
public enum AreaType {

    /**
     * UNKNOWN has to be the last value
     */
    CUBOID("Cuboid", 2),
    CYLINDER("Cylinder", 3),
    SPHERE("Sphere", 2),
    // TODO: config values for polygon and prism
    POLYGON_3D("Polygon", 3, 20),
    PRISM("Prism", 3, 10),
    UNKNOWN("Unknown", -1);

    public final String areaType;
    public final int neededBlocks;
    public final int maxBlocks;

    AreaType(String name, int neededBlocks, int maxBlocks) {
        this.areaType = name;
        this.neededBlocks = neededBlocks;
        this.maxBlocks = maxBlocks;
    }

    AreaType(String name, int neededBlocks) {
        this.areaType = name;
        this.neededBlocks = neededBlocks;
        this.maxBlocks = neededBlocks;
    }

    public static AreaType of(String name) {
        switch (name) {
            case "Cuboid":
                return CUBOID;
            case "Cylinder":
                return CYLINDER;
            case "Sphere":
                return SPHERE;
            case "Polygon":
                return POLYGON_3D;
            case "Prism":
                return PRISM;
            default:
                return UNKNOWN;
        }
    }
}
