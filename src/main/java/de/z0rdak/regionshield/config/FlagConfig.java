package de.z0rdak.regionshield.config;

import net.minecraftforge.common.ForgeConfigSpec;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class FlagConfig {

    public static final ForgeConfigSpec CONFIG_SPEC;

    public static final ForgeConfigSpec.ConfigValue<List<? extends String>>  BREAK_FLAG_ENTITIES;
    public static final ForgeConfigSpec.ConfigValue<List<? extends String>>  BREAK_FLAG_ENTITY_TAGS;

    static {
        final ForgeConfigSpec.Builder BUILDER = new ForgeConfigSpec.Builder();

        BUILDER.push("RegionShield mod flag configuration").build();

        BREAK_FLAG_ENTITIES = BUILDER
                .comment("Entities included/protected by the break block flag. Includes entities like armor stands and pictures by default")
                .defineListAllowEmpty(Arrays.asList("break_flag_entities"), () -> Arrays.asList("minecraft:armor_stand", "minecraft:painting", "minecraft:item_frame", "minecraft:glow_item_frame"), s -> true);

        BREAK_FLAG_ENTITY_TAGS = BUILDER
                .comment("Entity tags included/protected by the break block flag.")
                .defineListAllowEmpty(Arrays.asList("break_flag_entity_tags"), () -> Arrays.asList(), s -> true);

        BUILDER.pop();
        CONFIG_SPEC = BUILDER.build();

    }
}
