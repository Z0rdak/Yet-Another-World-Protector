package de.z0rdak.regionshield.config;

import net.minecraftforge.common.ForgeConfigSpec;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.Predicate;

public class ServerRegionConfigBuilder {

    public static final ForgeConfigSpec CONFIG_SPEC;

    public static final ForgeConfigSpec.ConfigValue<Integer> REGION_COLLISION_CHECK_INTERVAL;
    //public static final ForgeConfigSpec.ConfigValue<Boolean> DISABLE_FLAG_REGISTER;
    //public static final ForgeConfigSpec.ConfigValue<Boolean> DISABLE_REGION_COLLISION_CHECK;
    public static final ForgeConfigSpec.ConfigValue<Boolean> REGION_DEFAULT_FLAG_TYPE;
    public static final ForgeConfigSpec.ConfigValue<Integer> DEFAULT_REGION_PRIORITY;
    public static final ForgeConfigSpec.ConfigValue<Integer> REGION_DEFAULT_PRIORITY_INC;
    public static final ForgeConfigSpec.ConfigValue<List<? extends String>> REGION_DEFAULT_FLAGS;

    static {
        final ForgeConfigSpec.Builder BUILDER = new ForgeConfigSpec.Builder();

        BUILDER.push("RegionShield mod server configuration").build();

        REGION_COLLISION_CHECK_INTERVAL = BUILDER.comment("Default OP level to use commands.")
                .defineInRange("command_op_level", 4, 1, 4);

        DEFAULT_REGION_PRIORITY = BUILDER.comment("Default region priority for newly created regions.")
                .defineInRange("default_region_priority", 2, 0, Integer.MAX_VALUE);

        REGION_DEFAULT_PRIORITY_INC = BUILDER.comment("Default region priority increment/decrement.")
                .defineInRange("default_region_priority_inc", 5, 1, 100);

        // fixme
        REGION_DEFAULT_FLAGS = BUILDER.comment("Default flags for new regions.")
                .defineList("default_flags", new ArrayList<>(Arrays.asList("break", "place")), /* TODO */s -> true);

        REGION_DEFAULT_FLAG_TYPE = BUILDER.comment("Default flag tyoe for new flags.\n true -> Whitelist\n false -> Blacklist")
                .define("default_flag_type", false);

        BUILDER.pop();
        CONFIG_SPEC = BUILDER.build();
    }
}
