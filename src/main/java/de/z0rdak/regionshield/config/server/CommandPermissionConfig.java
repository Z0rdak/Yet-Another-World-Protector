package de.z0rdak.regionshield.config.server;

import de.z0rdak.regionshield.RegionShield;
import net.minecraftforge.common.ForgeConfigSpec;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

public class CommandPermissionConfig {

    public static final ForgeConfigSpec CONFIG_SPEC;
    public static final ForgeConfigSpec.ConfigValue<Integer> REQUIRED_OP_LEVEL;
    public static final ForgeConfigSpec.ConfigValue<List<? extends String>> PLAYERS_WITH_PERMISSION;
    public static final ForgeConfigSpec.ConfigValue<Boolean> COMMAND_BLOCK_EXECUTION;


    static {
        final ForgeConfigSpec.Builder BUILDER = new ForgeConfigSpec.Builder();

        BUILDER.push("RegionShield mod server configuration").build();

        COMMAND_BLOCK_EXECUTION = BUILDER.comment("Permission for command blocks to execute mod commands")
                .define("command_block_execution", true);

        REQUIRED_OP_LEVEL = BUILDER.comment("Minimum OP level to use mod commands.\n")
                .defineInRange("command_op_level", 4, 1, 4);

        PLAYERS_WITH_PERMISSION = BUILDER.comment("Player UUIDs with permission to use mod commands")
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
}
