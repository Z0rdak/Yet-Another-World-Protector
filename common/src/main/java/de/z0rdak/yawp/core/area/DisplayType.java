package de.z0rdak.yawp.core.area;

import java.util.Arrays;
import java.util.Set;
import java.util.stream.Collectors;

public enum DisplayType {
    FRAME("Frame"),
    HULL("Hull"),
    MINIMAL("Minimal"),
    MARKED("Marked");

    public final String name;

    DisplayType(String name) {
        this.name = name;
    }

    public static Set<String> entries(){
        return Arrays.stream(values())
                .map(DisplayType::name)
                .collect(Collectors.toSet());
    }

    public static DisplayType of(String displayType) {
        return switch (displayType.toLowerCase()) {
            case "frame" -> FRAME;
            case "hull" -> HULL;
            case "minimal" -> MINIMAL;
            case "marked" -> MARKED;
            default -> throw new IllegalStateException("Unexpected value: " + displayType.toLowerCase());
        };
    }

    @Override
    public String toString() {
        return name;
    }
}
