package de.z0rdak.yawp.config.server;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.core.BlockPos;
import net.minecraft.server.players.ServerOpListEntry;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.entity.CommandBlockEntity;
import net.minecraftforge.common.ForgeConfigSpec;
import net.minecraftforge.server.ServerLifecycleHooks;

import java.util.*;
import java.util.stream.Collectors;

public class CommandPermissionConfig {

    public static final ForgeConfigSpec CONFIG_SPEC;

    public static final String CONFIG_NAME = YetAnotherWorldProtector.MODID + "-common.toml";

    // TODO: Dedicated permission to teleport to region
    public static final ForgeConfigSpec.ConfigValue<Boolean> ALLOW_READ_ONLY_CMDS;
    public static final ForgeConfigSpec.ConfigValue<Integer> REQUIRED_OP_LEVEL;
    private static final ForgeConfigSpec.ConfigValue<List<? extends String>> PLAYERS_WITH_PERMISSION;
    public static final ForgeConfigSpec.ConfigValue<Boolean> COMMAND_BLOCK_EXECUTION;
    public static final ForgeConfigSpec.ConfigValue<Integer> WP_COMMAND_ALTERNATIVE;
    public static final String[] WP_CMDS = new String[]{"wp", "yawp"};
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
                .defineInRange("command_op_level", 4, 0, 5);

        ALLOW_READ_ONLY_CMDS = BUILDER.comment("Defines whether info commands for regions can be used by every player.")
                .define("allow_info_cmds", true);

        PLAYERS_WITH_PERMISSION = BUILDER.comment("Player UUIDs with permission to use mod commands.\n Make sure to put the UUIDs in parentheses, just like a normal string.\n Example: players_with_permission = [\"614c9eac-11c9-3ca6-b697-938355fa8235\", \"b9f5e998-520a-3fa2-8208-0c20f22aa20f\"]")
                .defineListAllowEmpty(Collections.singletonList("players_with_permission"), ArrayList::new, (uuid) -> {
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
                });
        BUILDER.pop();
        CONFIG_SPEC = BUILDER.build();
    }

    private static boolean containsBadLength(List<String> tokens, int size) {
        return tokens.stream().anyMatch(t -> t.length() != size);
    }

    public static boolean AllowInfoCmds() {
        return ALLOW_READ_ONLY_CMDS.get();
    }

    public static Set<String> UUIDsWithPermission() {
        return PLAYERS_WITH_PERMISSION.get()
                .stream()
                .filter(Objects::nonNull)
                .map(s -> (String) s)
                .collect(Collectors.toSet());
    }


    // FIXME: What about CommandBlockMinecarts?
    public static boolean hasPermission(CommandSourceStack source) {
        try {
            return hasPlayerPermission(source.getPlayerOrException());
        } catch (CommandSyntaxException e) {
            boolean isServerConsole = source.getTextName().equals("Server");
            if (isServerConsole) {
                return true;
            } else {
                return COMMAND_BLOCK_EXECUTION.get();
            }
        }
    }

    public static boolean hasPlayerPermission(Player player) {
        return hasUUIDConfigEntry(player) || hasNeededOpLevel(player) ||
                player.hasPermissions(REQUIRED_OP_LEVEL.get());
    }

    public static boolean hasUUIDConfigEntry(Player player) {
        Set<String> playersInConfig = UUIDsWithPermission();
        return playersInConfig.contains(player.getStringUUID());
    }

    public static boolean hasNeededOpLevel(Player player) {
        ServerOpListEntry opPlayerEntry = ServerLifecycleHooks.getCurrentServer()
                .getPlayerList()
                .getOps()
                .get(player.getGameProfile());
        if (opPlayerEntry != null) {
            return opPlayerEntry.getLevel() >= REQUIRED_OP_LEVEL.get();
        }
        return false;
    }
}
