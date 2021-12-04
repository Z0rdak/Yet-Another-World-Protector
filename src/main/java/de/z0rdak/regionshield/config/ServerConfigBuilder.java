package de.z0rdak.regionshield.config;

import net.minecraftforge.common.ForgeConfigSpec;
import net.minecraftforge.fml.ModList;

public class ServerConfigBuilder {

    public static final ForgeConfigSpec CONFIG_SPEC;

    public static final ForgeConfigSpec.ConfigValue<Integer> OP_COMMAND_PERMISSION_LEVEL;
    public static final ForgeConfigSpec.ConfigValue<Integer> DEFAULT_REGION_PRIORITY;
    public static final ForgeConfigSpec.ConfigValue<Integer> DEFAULT_REGION_PRIORITY_INC;

    public final static String BASE_CMD = "rs";

    static {
        final ForgeConfigSpec.Builder BUILDER = new ForgeConfigSpec.Builder();

        BUILDER.push("RegionShield mod server configuration").build();

        OP_COMMAND_PERMISSION_LEVEL = BUILDER.comment("Default OP level to use commands.")
                .defineInRange("command_op_level", 4, 1, 4);

        DEFAULT_REGION_PRIORITY = BUILDER.comment("Default region priority for newly created regions.")
                .defineInRange("default_region_priority", 2, 0, Integer.MAX_VALUE);

        // TODO: should be a not synced client config
        DEFAULT_REGION_PRIORITY_INC = BUILDER.comment("Default region priority increment for usage in commands.")
                .defineInRange("default_region_priority_inc", 1, 1, 100);

        BUILDER.pop();
        CONFIG_SPEC = BUILDER.build();

    }

    private static boolean isInModList(String modid) {
        return ModList.get().isLoaded(modid);
    }

    public static void setup(){

    }

}
