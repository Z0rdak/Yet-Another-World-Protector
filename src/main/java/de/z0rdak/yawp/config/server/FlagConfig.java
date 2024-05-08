package de.z0rdak.yawp.config.server;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import net.minecraftforge.common.ForgeConfigSpec;
import net.neoforged.neoforge.common.ModConfigSpec;

import java.util.*;
import java.util.stream.Collectors;

public class FlagConfig {

    public static final ModConfigSpec CONFIG_SPEC;
    public static final String CONFIG_NAME = YetAnotherWorldProtector.MODID + "-flags.toml";
    private static final ModConfigSpec.ConfigValue<List<? extends String>> BREAK_FLAG_ENTITIES;
    private static final ModConfigSpec.ConfigValue<List<? extends String>> BREAK_FLAG_ENTITY_TAGS;

    static {
        final ModConfigSpec.Builder BUILDER = new ModConfigSpec.Builder();

        BUILDER.push("YetAnotherWorldProtector mod flag configuration").build();

        BREAK_FLAG_ENTITIES = BUILDER
                .comment("Entities included/protected by the break block flag. Includes entities like armor stands and pictures by default")
                .defineListAllowEmpty(List.of("break_flag_entities"), FlagConfig::defaultEntityBreakFlagEntries, FlagConfig::isValidEntityEntry);

        BREAK_FLAG_ENTITY_TAGS = BUILDER
                .comment("Entity tags included/protected by the break block flag.")
                .defineListAllowEmpty(List.of("break_flag_entity_tags"), ArrayList::new, FlagConfig::isValidTagEntry);

        BUILDER.pop();
        CONFIG_SPEC = BUILDER.build();

    }

    private static List<String> defaultEntityBreakFlagEntries() {
        return Arrays.asList("minecraft:armor_stand", "minecraft:painting", "minecraft:item_frame", "minecraft:glow_item_frame", "minecraft:leash_knot");
    }

    public static Set<String> getBreakFlagEntities() {
        return FlagConfig.BREAK_FLAG_ENTITIES.get().stream()
                .filter(Objects::nonNull)
                .map(String::toString).collect(Collectors.toSet());
    }

    public static Set<String> getBreakFlagEntityTags() {
        return FlagConfig.BREAK_FLAG_ENTITY_TAGS.get().stream()
                .filter(Objects::nonNull)
                .map(String::toString)
                .collect(Collectors.toSet());
    }

    private static boolean isValidEntityEntry(Object entity) {
        if (entity instanceof String str) {
            boolean isNotEmptyAndContainsColon = !str.isEmpty() && !str.isBlank() && str.contains(":");
            if (!isNotEmptyAndContainsColon) {
                YetAnotherWorldProtector.LOGGER.warn("Invalid block tile resource key supplied for 'break_flag_entities': " + entity);
            }
            return isNotEmptyAndContainsColon;
        }
        return false;
    }

    private static boolean isValidTagEntry(Object entity) {
        if (entity instanceof String str) {
            boolean isNotEmptyAndContainsColon = !str.isEmpty() && !str.isBlank() && str.contains(":");
            if (!isNotEmptyAndContainsColon) {
                YetAnotherWorldProtector.LOGGER.warn("Invalid block tile resource key supplied for 'break_flag_entity_tags': " + entity);
            }
            return isNotEmptyAndContainsColon;
        }
        return false;
    }
}
