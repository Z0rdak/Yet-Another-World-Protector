package de.z0rdak.yawp.config.server;

import de.z0rdak.yawp.api.FlagRegister;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.flag.FlagFrequency;
import de.z0rdak.yawp.core.flag.RegionFlag;
import net.neoforged.neoforge.common.ModConfigSpec;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.*;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.constants.Constants.MOD_ID;

public class FlagConfig {

    public static final ModConfigSpec CONFIG_SPEC;
    public static final String CONFIG_NAME = Constants.MOD_ID + "-flags.toml";
    public static final Logger FLAG_CONFIG_LOGGER = LogManager.getLogger(MOD_ID.toUpperCase() + "-Flag-Config");

    private static final ModConfigSpec.ConfigValue<Boolean> REMOVE_ENTITIES_FOR_SPAWNING_FLAGS;
    private static final ModConfigSpec.ConfigValue<List<? extends String>> COVERED_BLOCK_ENTITIES;
    private static final ModConfigSpec.ConfigValue<List<? extends String>> COVERED_BLOCK_ENTITY_TAGS;
    private static final ModConfigSpec.ConfigValue<List<? extends String>> DISABLED_FLAGS;

    static {
        final ModConfigSpec.Builder BUILDER = new ModConfigSpec.Builder();

        BUILDER.push("YetAnotherWorldProtector mod flag configuration").build();

        COVERED_BLOCK_ENTITIES = BUILDER
                .comment("Entities included/protected by the break-block and place-blocks flags. Includes entities like armor stands and pictures by default")
                .defineListAllowEmpty(List.of("covered_block_entities"), FlagConfig::defaultCoveredBlockEntityEntries, null, FlagConfig::isValidEntityEntry);

        COVERED_BLOCK_ENTITY_TAGS = BUILDER
                .comment("Entity tags included/protected by the break-block and place-blocks flags.")
                .defineListAllowEmpty(List.of("covered_block_entity_tags"), ArrayList::new, null, FlagConfig::isValidTagEntry);

        REMOVE_ENTITIES_FOR_SPAWNING_FLAGS = BUILDER
                .comment("Toggle to remove entities when adding spawning-* flags.\nEntities with the PersistenceRequired tag will not be removed.\n true -> remove entities related to this flag\n false -> don't remove entities")
                .define(Collections.singletonList("remove_entities_for_spawning_flags"), true);

        DISABLED_FLAGS = BUILDER
                .comment("Flags which are disabled to reduce performance impact. This is currently limited to the following flags: `fluid_flow`, `water_flow` and `lava_flow`")
                .defineListAllowEmpty("disabled_flags", ArrayList::new, null, FlagConfig::isValidFlagEntry);

        BUILDER.pop();

        CONFIG_SPEC = BUILDER.build();
    }

    private static List<String> defaultCoveredBlockEntityEntries() {
        return Arrays.asList("minecraft:armor_stand", "minecraft:painting", "minecraft:item_frame", "minecraft:glow_item_frame", "minecraft:leash_knot");
    }

    public static Set<String> getCoveredBlockEntities() {
        return COVERED_BLOCK_ENTITIES.get().stream()
                .filter(Objects::nonNull)
                .map(String::toString).collect(Collectors.toSet());
    }

    public static Set<String> getCoveredBlockEntityTags() {
        return COVERED_BLOCK_ENTITY_TAGS.get().stream()
                .filter(Objects::nonNull)
                .map(String::toString).collect(Collectors.toSet());
    }

    /**
     *
     * @param frequency this is currently only for VERY_HIGH, but can be expanded later when the config is more fine grained
     */
    public static Set<String> getDisabledFrequencyFlags(FlagFrequency frequency) {
        if (frequency == FlagFrequency.VERY_HIGH) {
            return DISABLED_FLAGS.get().stream()
                    .filter(Objects::nonNull)
                    .map(String::toString).collect(Collectors.toSet());
        }
        return Set.of();
    }

    public static boolean isDisabledByConfig(String flag) {
        return getDisabledFrequencyFlags(FlagFrequency.VERY_HIGH).contains(flag);
    }


    public static boolean removeEntitiesEnabled() {
        return REMOVE_ENTITIES_FOR_SPAWNING_FLAGS.get();
    }

    private static boolean isValidEntityEntry(Object entity) {
        if (entity instanceof String str) {
            boolean isNotEmptyAndContainsColon = !str.isEmpty() && !str.isBlank() && str.contains(":");
            if (!isNotEmptyAndContainsColon) {
                FLAG_CONFIG_LOGGER.warn("Invalid block tile resource key supplied for 'break_flag_entities': {}", entity);
                return false;
            }
            return true;
        }
        return false;
    }

    private static boolean isValidFlagEntry(Object entity) {
        if (entity instanceof String str) {
            boolean isNotEmptyAndContainsColon = !str.isEmpty() && !str.isBlank() && str.contains(":");
            if (!isNotEmptyAndContainsColon) {
                FLAG_CONFIG_LOGGER.warn("Invalid flag supplied for 'disabled_flags': {}", entity);
                return false;
            }
            return str.equals(RegionFlag.FLUID_FLOW.name())
                    || str.equals(RegionFlag.WATER_FLOW.name())
                    || str.equals(RegionFlag.LAVA_FLOW.name());
        }
        return false;
    }

    private static boolean isValidTagEntry(Object entity) {
        if (entity instanceof String str) {
            boolean isNotEmptyAndContainsColon = !str.isEmpty() && !str.isBlank() && str.contains(":");
            if (!isNotEmptyAndContainsColon) {
                FLAG_CONFIG_LOGGER.warn("Invalid block tile resource key supplied for 'break_flag_entity_tags': {}", entity);
            }
            return isNotEmptyAndContainsColon;
        }
        return false;
    }
}
