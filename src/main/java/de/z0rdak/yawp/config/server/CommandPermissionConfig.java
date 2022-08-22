package de.z0rdak.yawp.config.server;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import net.minecraft.command.CommandSource;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.server.management.OpEntry;
import net.minecraftforge.common.ForgeConfigSpec;
import net.minecraftforge.fml.server.ServerLifecycleHooks;

import java.util.*;
import java.util.stream.Collectors;

public class CommandPermissionConfig {

    public static final ForgeConfigSpec CONFIG_SPEC;
    public static final ForgeConfigSpec.ConfigValue<Integer> REQUIRED_OP_LEVEL;
    private static final ForgeConfigSpec.ConfigValue<List<? extends String>> PLAYERS_WITH_PERMISSION;
    public static final ForgeConfigSpec.ConfigValue<Boolean> COMMAND_BLOCK_EXECUTION;


    static {
        final ForgeConfigSpec.Builder BUILDER = new ForgeConfigSpec.Builder();

        BUILDER.push("YetAnotherWorldProtector mod server configuration").build();

        COMMAND_BLOCK_EXECUTION = BUILDER.comment("Permission for command blocks to execute mod commands")
                .define("command_block_execution", true);

        REQUIRED_OP_LEVEL = BUILDER.comment("Minimum OP level to use mod commands.\n")
                .defineInRange("command_op_level", 4, 1, 4);

        PLAYERS_WITH_PERMISSION = BUILDER.comment("Player UUIDs with permission to use mod commands")
                .defineListAllowEmpty(Collections.singletonList("players_with_permission"), ArrayList::new, (uuid) -> {
                    if (uuid instanceof String) {
                        try {
                            UUID uuidObj = UUID.fromString((String) uuid);
                            YetAnotherWorldProtector.LOGGER.info("Player with UUID '" + uuidObj + "' read from config");
                            return true;
                        } catch (IllegalArgumentException e) {
                            YetAnotherWorldProtector.LOGGER.error("Invalid UUID '" + uuid + "' in config");
                            return false;
                        }
                    }
                    return false;
                });
        BUILDER.pop();
        CONFIG_SPEC = BUILDER.build();
    }

    public static Set<String> UUIDsWithPermission(){
        return PLAYERS_WITH_PERMISSION.get()
                .stream()
                .filter(Objects::nonNull)
                .map(s -> (String)s)
                .collect(Collectors.toSet());
    }


    public static boolean hasPermission(CommandSource source) {
        try {
            PlayerEntity player = source.getPlayerOrException();
            return hasPermission(player);
        } catch (CommandSyntaxException e) {
            boolean isServerConsole = false;
            // TODO: How to determine server console as source?

            if (isServerConsole) {
                return true;
            } else {
                return COMMAND_BLOCK_EXECUTION.get();
            }
        }
    }

    public static boolean hasPermission(PlayerEntity player) {
        return hasUUIDConfigEntry(player) || hasNeededOpLevel(player) || player.hasPermissions(REQUIRED_OP_LEVEL.get());
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
}
