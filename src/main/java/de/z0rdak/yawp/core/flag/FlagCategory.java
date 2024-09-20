package de.z0rdak.yawp.core.flag;

import java.util.Arrays;
import java.util.List;

public enum FlagCategory {
    PLAYER("player"),
    BLOCK("block"),
    ENTITY("entity"),
    ITEM("item"),
    ENVIRONMENT("environment"),
    PROTECTION("protection");

    public final String name;

    FlagCategory(String name) {
        this.name = name;
    }

    public static FlagCategory from(String category) throws IllegalArgumentException {
        List<FlagCategory> categories = Arrays.stream(values())
                .filter(flag -> flag.name.equalsIgnoreCase(category))
                .toList();
        if (categories.isEmpty()) {
            throw new IllegalArgumentException("Invalid flag category supplied");
        }
        return categories.get(0);
    }
}