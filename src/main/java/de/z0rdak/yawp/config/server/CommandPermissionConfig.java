package de.z0rdak.yawp.config.server;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.commands.CommandSourceType;
import de.z0rdak.yawp.commands.CommandUtil;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import net.minecraft.command.CommandSource;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.server.management.OpEntry;
import net.minecraftforge.common.ForgeConfigSpec;
import net.minecraftforge.fml.server.ServerLifecycleHooks;

import java.util.*;
import java.util.stream.Collectors;

public class CommandPermissionConfig {

    public static final ForgeConfigSpec CONFIG_SPEC;
    public static final String CONFIG_NAME = YetAnotherWorldProtector.MODID + "-common.toml";

    private static final ForgeConfigSpec.ConfigValue<Boolean> ENABLE_REGION_TP;
    private static final ForgeConfigSpec.ConfigValue<Boolean> ALLOW_READ_ONLY_CMDS;
    private static final ForgeConfigSpec.ConfigValue<Boolean> DISABLE_CMD_FOR_NON_OP;
    private static final ForgeConfigSpec.ConfigValue<Boolean> OP_BYPASS_FLAGS;
    private static final ForgeConfigSpec.ConfigValue<Boolean> ENABLE_HIERARCHY_OWNERSHIP;
    private static final ForgeConfigSpec.ConfigValue<Integer> REQUIRED_OP_LEVEL;
    private static final ForgeConfigSpec.ConfigValue<List<? extends String>> PLAYERS_WITH_PERMISSION;
    private static final ForgeConfigSpec.ConfigValue<Boolean> COMMAND_BLOCK_EXECUTION;
    private static final ForgeConfigSpec.ConfigValue<Integer> WP_COMMAND_ALTERNATIVE;
    private static final String[] WP_CMDS = new String[]{"wp", "yawp"};
    public static String BASE_CMD;

    static {
        final ForgeConfigSpec.Builder BUILDER = new ForgeConfigSpec.Builder();

        BUILDER.push("YetAnotherWorldProtector mod server configuration").build();

        COMMAND_BLOCK_EXECUTION = BUILDER.comment("Permission for command blocks to execute mod commands")
                .define("command_block_execution", true);

        WP_COMMAND_ALTERNATIVE = BUILDER.comment("Default command alternative used in quick commands in chat.\nThis is only important if another mod uses the /wp command (like Journey Map). Defaults to 0.\n" +
                        " 0 -> /wp\n 1 -> /yawp")
                .defineInRange("wp_root_command", 0, 0, 1);

        REQUIRED_OP_LEVEL = BUILDER.comment("Minimum OP level to use mod commands.\n 0 -> everyone can use the commands.\n 1-4 -> OP with specific level can use the commands.\n 5 -> no operator can use the commands.\n Defaults to 5.")
                .defineInRange("command_op_level", 5, 0, 5);

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

        PLAYERS_WITH_PERMISSION = BUILDER.comment("Player UUIDs with permission to use mod commands.\n Make sure to put the UUIDs in parentheses, just like a normal string.\n Example: players_with_permission = [\"614c9eac-11c9-3ca6-b697-938355fa8235\", \"b9f5e998-520a-3fa2-8208-0c20f22aa20f\"]")
                .defineListAllowEmpty(Collections.singletonList("players_with_permission"), ArrayList::new, CommandPermissionConfig::validateUuid);
        BUILDER.pop();
        CONFIG_SPEC = BUILDER.build();
    }

    private static boolean containsBadLength(List<String> tokens, int size) {
        return tokens.stream().anyMatch(t -> t.length() != size);
    }

    public static boolean isReadOnlyAllowed() {
        return ALLOW_READ_ONLY_CMDS.get();
    }

    public static boolean isCmdEnabledForNonOp() {
        return !DISABLE_CMD_FOR_NON_OP.get();
    }

    public static String getBaseCmdAlt() {
        return WP_CMDS[1];
    }

    public static String getBaseCmd() {
        return WP_CMDS[WP_COMMAND_ALTERNATIVE.get()];
    }

    public static boolean allowRegionTp() {
        return ENABLE_REGION_TP.get();
    }

    public static Set<String> UUIDsWithPermission() {
        return PLAYERS_WITH_PERMISSION.get()
                .stream()
                .filter(Objects::nonNull)
                .map(s -> (String) s)
                .collect(Collectors.toSet());
    }

    public static boolean isHierarchyOwnershipEnabled() {
        return ENABLE_HIERARCHY_OWNERSHIP.get();
    }

    public static boolean isCommandBlockExecutionAllowed() {
        return COMMAND_BLOCK_EXECUTION.get();
    }

    public static boolean hasConfigPermission(PlayerEntity player) {
        return hasUUIDConfigEntry(player) || hasNeededOpLevel(player) || player.hasPermissions(REQUIRED_OP_LEVEL.get());
    }

    public static boolean hasConfigPermAndOpByPassFlags(PlayerEntity player) {
        return hasConfigPermission(player) && OP_BYPASS_FLAGS.get();
    }

    public static boolean hasUUIDConfigEntry(PlayerEntity player) {
        Set<String> playersInConfig = UUIDsWithPermission();
        return playersInConfig.contains(player.getStringUUID());
    }

    public static boolean hasNeededOpLevel(PlayerEntity player) {
        OpEntry opPlayerEntry = ServerLifecycleHooks.getCurrentServer()
                .getPlayerList()
                .getOps()
                .get(player.getGameProfile());
        if (opPlayerEntry != null) {
            return opPlayerEntry.getLevel() >= REQUIRED_OP_LEVEL.get();
        }
        return false;
    }

    public static boolean validateUuid(Object uuid) {
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
                YetAnotherWorldProtector.LOGGER.warn("Invalid UUID '" + uuid + "' in config");
                return false;
            }
        }
        return false;
    }

    public static boolean hasRegionPermission(IProtectedRegion region, PlayerEntity player) {
        return hasRegionPermission(region, player, CommandUtil.OWNER);
    }

    public static boolean hasRegionPermission(IProtectedRegion region, PlayerEntity player, String permissionGroup) {
        return isHierarchyOwnershipEnabled()
                ? hasRegionHierarchyPermission(region, player, permissionGroup)
                : region.isInGroup(player, permissionGroup);
    }

    private static boolean hasRegionHierarchyPermission(IProtectedRegion region, PlayerEntity player, String permissionGroup) {
        return hasRegionHierarchyPermission(region, player, permissionGroup, false);
    }

    private static boolean hasRegionHierarchyPermission(IProtectedRegion region, PlayerEntity player, String permissionGroup, boolean hasPermission) {
        if (region.getParent().equals(region)) {
            return hasPermission || region.isInGroup(player, permissionGroup);
        }
        hasPermission = hasPermission || region.isInGroup(player, permissionGroup);
        return hasRegionHierarchyPermission(region.getParent(), player, permissionGroup, hasPermission);
    }

    public static boolean hasConfigPermission(CommandSource src, CommandSourceType cmdSrcType) throws CommandSyntaxException {
        switch (cmdSrcType) {
            case PLAYER: {
                ServerPlayerEntity player = src.getPlayerOrException();
                return hasConfigPermission(player);
            }
            case SERVER:
                return true;
            case COMMAND_BLOCK:
                return isCommandBlockExecutionAllowed();
            default:
                return false;
        }
    }

    public static boolean hasCmdPermission(CommandSource src) {
        CommandSourceType cmdSrcType = CommandSourceType.of(src);
        try {
            return hasConfigPermission(src, cmdSrcType);
        } catch (CommandSyntaxException e) {
            return false;
        }
    }

    public static boolean isAllowedForNonOp(CommandSource src) {
        CommandSourceType cmdSrcType = CommandSourceType.of(src);
        try {
            return hasConfigPermission(src, cmdSrcType) || CommandPermissionConfig.isCmdEnabledForNonOp();
        } catch (CommandSyntaxException e) {
            return false;
        }
    }
}
