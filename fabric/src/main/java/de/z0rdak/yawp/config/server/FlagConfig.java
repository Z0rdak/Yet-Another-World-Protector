package de.z0rdak.yawp.config.server;

import net.minecraftforge.common.ForgeConfigSpec;

import java.util.*;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.config.ConfigRegistry.CONFIG_LOGGER;
import static de.z0rdak.yawp.constants.Constants.MOD_ID;

public class FlagConfig {

    public static final ForgeConfigSpec CONFIG_SPEC;
    public static final String CONFIG_NAME = MOD_ID + "-flags.toml";

    private static final ForgeConfigSpec.ConfigValue<Boolean> REMOVE_ENTITIES_FOR_SPAWNING_FLAGS;
    private static final ForgeConfigSpec.ConfigValue<List<? extends String>> COVERED_BLOCK_ENTITIES;
    private static final ForgeConfigSpec.ConfigValue<List<? extends String>> COVERED_BLOCK_ENTITY_TAGS;

    static {
        final ForgeConfigSpec.Builder BUILDER = new ForgeConfigSpec.Builder();

        BUILDER.push("YetAnotherWorldProtector mod flag configuration").build();

        COVERED_BLOCK_ENTITIES = BUILDER
                .comment("Entities included/protected by the break-block and place-blocks flags. Includes entities like armor stands and pictures by default")
                .defineListAllowEmpty(List.of("covered_block_entities"), FlagConfig::defaultCoveredBlockEntityEntries, FlagConfig::isValidEntityEntry);

        COVERED_BLOCK_ENTITY_TAGS = BUILDER
                .comment("Entity tags included/protected by the break-block and place-blocks flags.")
                .defineListAllowEmpty(List.of("covered_block_entity_tags"), ArrayList::new, FlagConfig::isValidTagEntry);

        REMOVE_ENTITIES_FOR_SPAWNING_FLAGS = BUILDER
                .comment("Toggle to remove entities when adding spawning-* flags.\nEntities with the PersistenceRequired tag will not be removed.\n true -> remove entities related to this flag\n false -> don't remove entities")
                .define(Collections.singletonList("remove_entities_for_spawning_flags"), true);


        BUILDER.pop();
        CONFIG_SPEC = BUILDER.build();

    }

    private static List<String> defaultCoveredBlockEntityEntries() {
        return Arrays.asList("minecraft:armor_stand", "minecraft:painting", "minecraft:item_frame", "minecraft:glow_item_frame", "minecraft:leash_knot");
    }

    public static Set<String> getCoveredBlockEntities() {
        return FlagConfig.COVERED_BLOCK_ENTITIES.get().stream()
                .filter(Objects::nonNull)
                .map(String::toString).collect(Collectors.toSet());
    }

    public static Set<String> getCoveredBlockEntityTags() {
        return FlagConfig.COVERED_BLOCK_ENTITY_TAGS.get().stream()
                .filter(Objects::nonNull)
                .map(String::toString)
                .collect(Collectors.toSet());
    }

    public static boolean removeEntitiesEnabled() {
        return REMOVE_ENTITIES_FOR_SPAWNING_FLAGS.get();
    }

    private static boolean isValidEntityEntry(Object entity) {
        if (entity instanceof String str) {
            boolean isNotEmptyAndContainsColon = !str.isEmpty() && !str.isBlank() && str.contains(":");
            if (!isNotEmptyAndContainsColon) {
                CONFIG_LOGGER.warn("Invalid block tile resource key supplied for 'break_flag_entities': {}", entity);
            }
            return isNotEmptyAndContainsColon;
        }
        return false;
    }

    private static boolean isValidTagEntry(Object entity) {
        if (entity instanceof String str) {
            boolean isNotEmptyAndContainsColon = !str.isEmpty() && !str.isBlank() && str.contains(":");
            if (!isNotEmptyAndContainsColon) {
                CONFIG_LOGGER.warn("Invalid block tile resource key supplied for 'break_flag_entity_tags': {}", entity);
            }
            return isNotEmptyAndContainsColon;
        }
        return false;
    }
}