package de.z0rdak.yawp.commands;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import net.minecraft.entity.Entity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.vehicle.CommandBlockMinecartEntity;
import net.minecraft.server.command.ServerCommandSource;

public enum CommandSourceType {
    PLAYER("player"),
    SERVER("server"),
    COMMAND_BLOCK("command-block"),
    NON_PLAYER("non-player"),
    UNKNOWN("unknown");

    public final String source;

    CommandSourceType(String source) {
        this.source = source;
    }

    public static CommandSourceType of(ServerCommandSource cmdSrc) throws IllegalArgumentException {
        if (cmdSrc == null) {
            throw new IllegalArgumentException("Command source can't be null!");
        }
        try {
            Entity cmdSrcEntity = cmdSrc.getEntityOrThrow();
            if (!(cmdSrcEntity instanceof PlayerEntity)) {
                if (cmdSrcEntity instanceof CommandBlockMinecartEntity) {
                    return COMMAND_BLOCK;
                }
                return NON_PLAYER;
            } else {
                return PLAYER;
            }
        } catch (CommandSyntaxException e) {
            // for server and command blocks this is
            // just an exclusion procedure because it is not possible to access the direct source
            if (cmdSrc.getName().equals("Server")) {
                return SERVER;
            }
            return COMMAND_BLOCK;
        }
    }

    @Override
    public String toString() {
        return source;
    }
}
