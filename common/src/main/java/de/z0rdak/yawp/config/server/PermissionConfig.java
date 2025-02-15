package de.z0rdak.yawp.config.server;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.commands.CommandSourceType;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.player.Player;
import net.neoforged.neoforge.common.ModConfigSpec;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.*;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.constants.Constants.MOD_ID;

public class PermissionConfig {

    public static final ModConfigSpec CONFIG_SPEC;
    public static final String CONFIG_NAME = Constants.MOD_ID + "-common.toml";
    public static final Logger PERMISSION_CONFIG_LOGGER = LogManager.getLogger(MOD_ID.toUpperCase() + "-Permission-Config");

    private static final ModConfigSpec.ConfigValue<Boolean> ENABLE_REGION_TP;
    private static final ModConfigSpec.ConfigValue<Boolean> ALLOW_READ_ONLY_CMDS;
    private static final ModConfigSpec.ConfigValue<Boolean> DISABLE_CMD_FOR_NON_OP;
    private static final ModConfigSpec.ConfigValue<Boolean> OP_BYPASS_FLAGS;
    private static final ModConfigSpec.ConfigValue<Boolean> ENABLE_HIERARCHY_OWNERSHIP;
    private static final ModConfigSpec.ConfigValue<Integer> REQUIRED_OP_LEVEL;
    private static final ModConfigSpec.ConfigValue<List<? extends String>> PLAYERS_WITH_PERMISSION;
    private static final ModConfigSpec.ConfigValue<Boolean> COMMAND_BLOCK_EXECUTION;
    private static final ModConfigSpec.ConfigValue<Boolean> ENABLE_MARKER_CREATION;

    static {
        final ModConfigSpec.Builder BUILDER = new ModConfigSpec.Builder();

        BUILDER.push("YetAnotherWorldProtector mod server configuration").build();

        COMMAND_BLOCK_EXECUTION = BUILDER.comment("Permission for command blocks to execute mod commands")
                .define("command_block_execution", true);
        
        REQUIRED_OP_LEVEL = BUILDER.comment("Minimum OP level to use mod commands.\n 0 -> everyone can use the commands.\n 1-4 -> OP with specific level can use the commands.\n 5 -> no operator can use the commands.\n Defaults to 5.")
                .defineInRange("command_op_level", 4, 0, 5);

        ALLOW_READ_ONLY_CMDS = BUILDER.comment("Defines whether info commands for regions can be used by every player.")
                .define("allow_info_cmds", true);

        DISABLE_CMD_FOR_NON_OP = BUILDER.comment("Defines whether mod commands are disabled for non-OP players.")
                .define("disable_cmd_for_non_op", false);

        OP_BYPASS_FLAGS = BUILDER.comment("Defines whether OPs/permitted players are allowed to bypass flags set in regions.")
                .define("op_bypass_flags", true);

        ENABLE_HIERARCHY_OWNERSHIP = BUILDER.comment("Defines whether owners of parent regions have implicit ownership rights for child regions as well")
                .define("hierarchy_ownership", true);

        ENABLE_REGION_TP = BUILDER.comment("Defines whether teleport in and out of a region is allowed by everyone. Mostly useful when using something like Waystones inside of regions.")
                .define("allow_region_tp", false);

        ENABLE_MARKER_CREATION = BUILDER.comment("Enable creation of RegionMarker by renaming a stick in an Anvil.")
                .define("enable_marker_creation", true);
        
        PLAYERS_WITH_PERMISSION = BUILDER.comment("Player UUIDs with permission to use mod commands.\n Make sure to put the UUIDs in parentheses, just like a normal string.\n Example: players_with_permission = [\"614c9eac-11c9-3ca6-b697-938355fa8235\", \"b9f5e998-520a-3fa2-8208-0c20f22aa20f\"]")
                .defineListAllowEmpty(Collections.singletonList("players_with_permission"), ArrayList::new, PermissionConfig::validateUuid);
        BUILDER.pop();
        CONFIG_SPEC = BUILDER.build();
    }

    private static boolean containsBadLength(List<String> tokens, int size) {
        return tokens.stream().anyMatch(t -> t.length() != size);
    }

    public static Set<String> UUIDsWithPermission() {
        return PLAYERS_WITH_PERMISSION.get()
                .stream()
                .filter(Objects::nonNull)
                .map(s -> (String) s)
                .collect(Collectors.toSet());
    }

    public static boolean isReadOnlyAllowed() {
        return ALLOW_READ_ONLY_CMDS.get();
    }

    public static boolean isCmdEnabledForNonOp() {
        return !DISABLE_CMD_FOR_NON_OP.get();
    }

    public static boolean allowRegionTp() {
        return ENABLE_REGION_TP.get();
    }
    
    public static boolean isMarkerCreationEnabled() {
        return ENABLE_MARKER_CREATION.get();
    }

    public static boolean isHierarchyOwnershipEnabled() {
        return ENABLE_HIERARCHY_OWNERSHIP.get();
    }

    public static boolean isCommandBlockExecutionAllowed() {
        return COMMAND_BLOCK_EXECUTION.get();
    }

    public static int getRequiredOpLevel() {
        return REQUIRED_OP_LEVEL.get();
    }

    public static boolean byPassFlagAllowed() {
        return OP_BYPASS_FLAGS.get();
    }

    private static boolean validateUuid(Object uuid) {
        if (uuid instanceof String) {
            try {
                String uuidStr = (String) uuid;
                if (uuidStr.length() != 36) {
                    throw new IllegalArgumentException("Invalid UUID - wrong length");
                }
                List<String> uuidTokens = Arrays.asList(uuidStr.split("-"));
                List<String> shortTokens = uuidTokens.subList(1, 3);
                if (uuidTokens.get(0).length() != 8 || containsBadLength(shortTokens, 4) || uuidTokens.get(4).length() != 12) {
                    throw new IllegalArgumentException("Invalid UUID - wrong token sizes");
                }
                return true;
            } catch (IllegalArgumentException e) {
                PERMISSION_CONFIG_LOGGER.warn("Invalid UUID '{}' in config", uuid);
                return false;
            }
        }
        return false;
    }

    public static boolean hasConfigPermission(Player player) {
        return hasUUIDConfigEntry(player) || hasRequiredOpLevel(player);
    }

    public static boolean hasUUIDConfigEntry(Player player) {
        return PermissionConfig.UUIDsWithPermission().contains(player.getStringUUID());
    }

    public static boolean hasRequiredOpLevel(Player player) {
        return player.hasPermissions(PermissionConfig.getRequiredOpLevel());
    }

    public static boolean hasConfigPermission(CommandSourceStack src, CommandSourceType cmdSrcType) throws CommandSyntaxException {
        switch (cmdSrcType) {
            case PLAYER: {
                ServerPlayer player = src.getPlayerOrException();
                return hasConfigPermission(player);
            }
            case COMMAND_BLOCK:
                return Services.PERMISSION_CONFIG.isCommandBlockExecutionAllowed();
            case SERVER:
                return true;
        }
        return false;
    }
}
