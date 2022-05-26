package de.z0rdak.regionshield.config;

import de.z0rdak.regionshield.RegionShield;
import net.minecraftforge.common.ForgeConfigSpec;
import net.minecraftforge.fml.ModList;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

public class ServerConfigBuilder {

    public static final ForgeConfigSpec CONFIG_SPEC;

    public static final ForgeConfigSpec.ConfigValue<Integer> RS_CMD_OP_LEVEL;
    public static final ForgeConfigSpec.ConfigValue<Integer> DEFAULT_REGION_PRIORITY;
    public static final ForgeConfigSpec.ConfigValue<Integer> DEFAULT_REGION_PRIORITY_INC;
    public static final ForgeConfigSpec.ConfigValue<List<? extends String>> PLAYERS_WITH_PERMISSION;


    public final static String BASE_CMD = "rs";

    static {
        final ForgeConfigSpec.Builder BUILDER = new ForgeConfigSpec.Builder();

        BUILDER.push("RegionShield mod server configuration").build();

        RS_CMD_OP_LEVEL = BUILDER.comment("Default OP level to use mod commands.")
                .defineInRange("command_op_level", 4, 1, 4);

        DEFAULT_REGION_PRIORITY = BUILDER.comment("Default region priority for newly created regions.")
                .defineInRange("default_region_priority", 2, 0, Integer.MAX_VALUE);

        // TODO: should be a not synced client config
        DEFAULT_REGION_PRIORITY_INC = BUILDER.comment("Default region priority increment for usage in commands.")
                .defineInRange("default_region_priority_inc", 1, 1, 100);

        // TODO: Sort config options
        PLAYERS_WITH_PERMISSION = BUILDER.comment("Player UUIDs with permission to use RS commands")
                .defineListAllowEmpty(Collections.singletonList("players_with_permission"), ArrayList::new, (uuid) -> {
                    if (uuid instanceof String) {
                        try {
                            UUID uuidObj = UUID.fromString((String) uuid);
                            RegionShield.LOGGER.info("Player with UUID '" + uuidObj + "' read from config");
                            return true;
                        } catch (IllegalArgumentException e) {
                            RegionShield.LOGGER.error("Invalid UUID '" + uuid + "' in config");
                            return false;
                        }
                    }
                    return false;
                });

        BUILDER.pop();
        CONFIG_SPEC = BUILDER.build();

    }

    private static boolean isInModList(String modid) {
        return ModList.get().isLoaded(modid);
    }

    public static void setup() {

    }

}
