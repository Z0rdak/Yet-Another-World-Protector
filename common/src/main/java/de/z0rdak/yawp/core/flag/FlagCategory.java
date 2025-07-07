package de.z0rdak.yawp.core.flag;

import java.util.Arrays;
import java.util.List;

public enum FlagCategory {
    /**
     * Flags in this category involve preventing a certain event for the player.
     * E.g. gaining XP/levels without reporting it to the player
     */
    PLAYER("player"),
    BENEFICIAL("beneficial"),
    /**
     * Flags in this category involve reporting back the prevented action to the player.
     * E.g. break-blocks
     */
    PLAYER_PREVENTION("prevention"),
    /**
     * Flags related to actions directly targeting blocks
     */
    BLOCK("block"),
    /**
     * Flags related to actions directly targeting entities
     */
    ENTITY("entity"),
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