package de.z0rdak.yawp.config.server;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import net.minecraftforge.common.ForgeConfigSpec;

import java.util.Arrays;
import java.util.List;

public class FlagConfig {

    public static final ForgeConfigSpec CONFIG_SPEC;
    public static final String CONFIG_NAME = YetAnotherWorldProtector.MODID + "-flags.toml";
    public static final ForgeConfigSpec.ConfigValue<List<? extends String>> BREAK_FLAG_ENTITIES;
    public static final ForgeConfigSpec.ConfigValue<List<? extends String>> BREAK_FLAG_ENTITY_TAGS;

    static {
        final ForgeConfigSpec.Builder BUILDER = new ForgeConfigSpec.Builder();

        BUILDER.push("YetAnotherWorldProtector mod flag configuration").build();

        BREAK_FLAG_ENTITIES = BUILDER
                .comment("Entities included/protected by the break block flag. Includes entities like armor stands and pictures by default")
                .defineListAllowEmpty(List.of("break_flag_entities"), FlagConfig::defaultEntityBreakFlagEntries, FlagConfig::isValidEntityEntry);

        BREAK_FLAG_ENTITY_TAGS = BUILDER
                .comment("Entity tags included/protected by the break block flag.")
                .defineListAllowEmpty(List.of("break_flag_entity_tags"), () -> List.of(""), FlagConfig::isValidTagEntry);

        BUILDER.pop();
        CONFIG_SPEC = BUILDER.build();

    }

    private static List<String> defaultEntityBreakFlagEntries() {
        return Arrays.asList("minecraft:armor_stand", "minecraft:painting", "minecraft:item_frame", "minecraft:glow_item_frame", "minecraft:leash_knot");
    }

    private static boolean isValidEntityEntry(Object entity) {
        return true;
    }

    private static boolean isValidTagEntry(Object entity) {
        return true;
    }
}
