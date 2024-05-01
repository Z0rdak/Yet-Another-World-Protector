package de.z0rdak.yawp.commands;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.entity.vehicle.MinecartCommandBlock;

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

    public static CommandSourceType of(CommandSourceStack cmdSrc) throws IllegalArgumentException {
        if (cmdSrc == null) {
            throw new IllegalArgumentException("Command source can't be null!");
        }
        try {
            Entity cmdSrcEntity = cmdSrc.getEntityOrException();
            if (!(cmdSrcEntity instanceof Player)) {
                if (cmdSrcEntity instanceof MinecartCommandBlock) {
                    return COMMAND_BLOCK;
                }
                return NON_PLAYER;
            } else {
                return PLAYER;
            }
        } catch (CommandSyntaxException e) {
            // for server and command blocks this is
            // just an exclusion procedure because it is not possible to access the direct source
            if (cmdSrc.getTextName().equals("Server")) {
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
