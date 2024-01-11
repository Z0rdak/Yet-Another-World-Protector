package de.z0rdak.yawp.commands;

import net.minecraft.command.CommandSource;
import net.minecraft.entity.Entity;
import net.minecraft.entity.player.PlayerEntity;

public enum CommandSourceType {
    PLAYER("player"),
    SERVER("server"),
    COMMAND_BLOCK("command_block"),
    NON_PLAYER("non_player"),
    UNKNOWN("unknown");

    public final String source;

    CommandSourceType(String source) {
        this.source = source;
    }

    public static CommandSourceType of(CommandSource cmdSrc) throws IllegalArgumentException {
        if (cmdSrc == null) {
            throw new IllegalArgumentException("Command source can't be null!");
        }
        Entity cmdSrcEntity = cmdSrc.getEntity();
        if (cmdSrcEntity != null && !(cmdSrcEntity instanceof PlayerEntity)) {
            return NON_PLAYER;
        }
        if (cmdSrcEntity instanceof PlayerEntity) {
            return PLAYER;
        }
        if (cmdSrc.getTextName().equals("Server")) {
            return SERVER;
        }
        return COMMAND_BLOCK;
    }

    @Override
    public String toString() {
        return source;
    }
}
