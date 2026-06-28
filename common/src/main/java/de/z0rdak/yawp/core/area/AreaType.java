package de.z0rdak.yawp.core.area;

import java.util.Arrays;
import java.util.Collection;
import java.util.Locale;
import java.util.stream.Collectors;

public enum AreaType {

    CUBOID("Cuboid", 2),
    SPHERE("Sphere", 2);

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

    public static Collection<String> getTypes() {
        return Arrays.stream(AreaType.values())
                .map(AreaType::toString)
                .collect(Collectors.toSet());
    }

    public static boolean isValidAreaType(String type) {
        return AreaType.of(type) != null;
    }

    public static AreaType of(String name) {
        switch (name.toLowerCase(Locale.ROOT)) {
            case "cuboid":
                return CUBOID;
            case "sphere":
                return SPHERE;
            default:
                throw new IllegalArgumentException("Unknown area type: " + name);
        }
    }

    @Override
    public String toString() {
        return areaType.toLowerCase(Locale.ROOT);
    }
}
