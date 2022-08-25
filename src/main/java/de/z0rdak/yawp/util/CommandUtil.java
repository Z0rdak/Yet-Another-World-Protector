package de.z0rdak.yawp.util;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.commands.CommandConstants;
import de.z0rdak.yawp.config.server.CommandPermissionConfig;
import net.minecraft.command.CommandSource;
import net.minecraft.command.Commands;
import net.minecraft.command.arguments.DimensionArgument;
import net.minecraft.command.arguments.EntityArgument;
import net.minecraft.command.arguments.TeamArgument;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.scoreboard.ScorePlayerTeam;
import net.minecraft.util.RegistryKey;
import net.minecraft.world.World;

public class CommandUtil {

    public static LiteralArgumentBuilder<CommandSource> literal(CommandConstants constant) {
        return Commands.literal(constant.toString());
    }

    public static RegistryKey<World> getDimensionArgument(CommandContext<CommandSource> ctx) throws CommandSyntaxException {
        return DimensionArgument.getDimension(ctx, CommandConstants.DIMENSION.toString()).dimension();
    }

    public static ServerPlayerEntity getPlayerArgument(CommandContext<CommandSource> ctx) throws CommandSyntaxException {
        return EntityArgument.getPlayer(ctx, CommandConstants.PLAYER.toString());
    }

    public static void handleCommandWithoutPlayer(CommandSyntaxException e) {
        e.printStackTrace();
    }


    public static String getRegionNameArgument(CommandContext<CommandSource> ctx) {
        return StringArgumentType.getString(ctx, CommandConstants.REGION.toString());
    }

    public static String getFlagNameArgument(CommandContext<CommandSource> ctx) {
        return StringArgumentType.getString(ctx, CommandConstants.FLAG.toString());
    }

    public static String getAssociateArgument(CommandContext<CommandSource> ctx) {
        return StringArgumentType.getString(ctx, CommandConstants.AFFILIATION.toString());
    }

    public static ServerPlayerEntity getOwnerArgument(CommandContext<CommandSource> ctx) throws CommandSyntaxException {
        return EntityArgument.getPlayer(ctx, CommandConstants.OWNER.toString());
    }

    public static ScorePlayerTeam getTeamArgument(CommandContext<CommandSource> ctx) throws CommandSyntaxException {
        return TeamArgument.getTeam(ctx, CommandConstants.TEAM.toString());
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

    public static String buildCommandStr(String... cmdTokens){
        String preamble = "/" + CommandPermissionConfig.BASE_CMD;
        String cmdStr = String.join(" ", cmdTokens);
        return preamble + " " + cmdStr;
    }
}
