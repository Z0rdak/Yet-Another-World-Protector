package de.z0rdak.regionshield.util;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.regionshield.commands.CommandConstants;
import net.minecraft.command.CommandSource;
import net.minecraft.command.arguments.EntityArgument;
import net.minecraft.entity.player.ServerPlayerEntity;

public class CommandUtil {

    public static ServerPlayerEntity getPlayerArgument(CommandContext<CommandSource> ctx) throws CommandSyntaxException {
        return EntityArgument.getPlayer(ctx, CommandConstants.PLAYER.toString());
    }

    public static void handleCommandWithoutPlayer(CommandSyntaxException e) {
        e.printStackTrace();
    }


    public static String getRegionNameArgument(CommandContext<CommandSource> ctx) {
        return StringArgumentType.getString(ctx, CommandConstants.REGION.toString());
    }

    public static ServerPlayerEntity getOwnerArgument(CommandContext<CommandSource> ctx) throws CommandSyntaxException {
        return EntityArgument.getPlayer(ctx, CommandConstants.OWNER.toString());
    }

    public static boolean getActivateArgument(CommandContext<CommandSource> ctx) {
        return BoolArgumentType.getBool(ctx, CommandConstants.ACTIVATE.toString());
    }

    public static boolean getEnableArgument(CommandContext<CommandSource> ctx) {
        return BoolArgumentType.getBool(ctx, CommandConstants.ENABLE.toString());
    }

    public static int getPriorityArgument(CommandContext<CommandSource> ctx) {
        return IntegerArgumentType.getInteger(ctx, CommandConstants.PRIORITY.toString());
    }
}
