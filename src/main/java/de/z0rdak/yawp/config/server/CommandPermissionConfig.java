package de.z0rdak.yawp.config.server;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.server.players.ServerOpListEntry;
import net.minecraft.world.entity.player.Player;
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

    public static String BASE_CMD;
    public static String BASE_CMD_ALT = "yawp";

    static {
        final ForgeConfigSpec.Builder BUILDER = new ForgeConfigSpec.Builder();

        BUILDER.push("YetAnotherWorldProtector mod server configuration").build();

        COMMAND_BLOCK_EXECUTION = BUILDER.comment("Permission for command blocks to execute mod commands")
                .define("command_block_execution", true);

        REQUIRED_OP_LEVEL = BUILDER.comment("Minimum OP level to use mod commands.\n")
                .defineInRange("command_op_level", 4, 0, 4);

        ALLOW_READ_ONLY_CMDS = BUILDER.comment("Defines whether info commands for regions can be used by every player.")
                .define("allow_info_cmds", true);

        PLAYERS_WITH_PERMISSION = BUILDER.comment("Player UUIDs with permission to use mod commands")
                .defineListAllowEmpty(Collections.singletonList("players_with_permission"), ArrayList::new, (uuid) -> {
                    if (uuid instanceof String) {
                        try {
                            String uuidStr = (String) uuid;
                            if (uuidStr.length() != 36) {
                               throw new IllegalArgumentException("Invalid UUID - wrong length");
                            }
                            List<String> uuidTokens = Arrays.asList(uuidStr.split("-"));
                            List<String> shortTokens = uuidTokens.subList(1,3);
                            if (uuidTokens.get(0).length() != 8 || containsBadLength(shortTokens, 4) || uuidTokens.get(4).length() != 12) {
                                throw new IllegalArgumentException("Invalid UUID - wrong token sizes");
                            }
                            UUID uuidObj = UUID.fromString(uuidStr);
                            YetAnotherWorldProtector.LOGGER.info("Player with UUID '" + uuidObj + "' read from config");
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

    public static boolean AllowInfoCmds(){
        return ALLOW_READ_ONLY_CMDS.get();
    }

    public static Set<String> UUIDsWithPermission(){
        return PLAYERS_WITH_PERMISSION.get()
                .stream()
                .filter(Objects::nonNull)
                .map(s -> (String)s)
                .collect(Collectors.toSet());
    }


    public static boolean hasPermission(CommandSourceStack source) {
        try {
            return hasPlayerPermission(source.getPlayerOrException());
        } catch (CommandSyntaxException e) {
            // FIXME: How to identify server console
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
