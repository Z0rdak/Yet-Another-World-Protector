package de.z0rdak.yawp.core.area;

import java.util.Arrays;
import java.util.Collection;
import java.util.stream.Collectors;

public enum AreaType {

    CUBOID("Cuboid", 2),
    CYLINDER("Cylinder", 3),
    SPHERE("Sphere", 2),
    POLYGON_3D("Polygon", 3, 20),
    PRISM("Prism", 3, 10);

    public final String areaType;
    public final int neededBlocks;
    public final int maxBlocks;

    AreaType(String name, int neededBlocks, int maxBlocks) {
        this.areaType = name;
        this.neededBlocks = neededBlocks;
        this.maxBlocks = maxBlocks;
    }

    AreaType(String name) {
        this.areaType = name;
        this.neededBlocks = 0;
        this.maxBlocks = 0;
    }

    AreaType(String name, int neededBlocks) {
        this.areaType = name;
        this.neededBlocks = neededBlocks;
        this.maxBlocks = neededBlocks;
    }

    public static Collection<String> getTypes() {
        return Arrays.stream(AreaType.values())
                .map(AreaType::toString)
                .collect(Collectors.toSet());
    }

    public static boolean isValidAreaType(String type) {
        return AreaType.of(type) != null;
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
                return null;
        }
    }

    @Override
    public String toString() {
        return areaType;
    }
}
