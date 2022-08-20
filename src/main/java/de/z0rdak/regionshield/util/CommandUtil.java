package de.z0rdak.regionshield.util;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.builder.RequiredArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.regionshield.commands.CommandConstants;
import net.minecraft.command.CommandSource;
import net.minecraft.command.Commands;
import net.minecraft.command.arguments.DimensionArgument;
import net.minecraft.command.arguments.EntityArgument;
import net.minecraft.command.arguments.EntitySelector;
import net.minecraft.command.arguments.TeamArgument;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.scoreboard.ScorePlayerTeam;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.ResourceLocation;
import net.minecraft.world.World;

public class CommandUtil {

    public static RequiredArgumentBuilder<CommandSource, ResourceLocation> dimensionArgument = Commands.argument(CommandConstants.DIMENSION.toString(), DimensionArgument.dimension());
    public static RequiredArgumentBuilder<CommandSource, String> regionNameArgument = Commands.argument(CommandConstants.REGION.toString(), StringArgumentType.string());
    public static RequiredArgumentBuilder<CommandSource, EntitySelector> ownerArgument = Commands.argument(CommandConstants.OWNER.toString(), EntityArgument.player());
    public static RequiredArgumentBuilder<CommandSource, String> teamArgument = Commands.argument(CommandConstants.TEAM.toString(), TeamArgument.team());
    public static RequiredArgumentBuilder<CommandSource, Boolean> activateArgument = Commands.argument(CommandConstants.ACTIVATE.toString(), BoolArgumentType.bool());
    public static RequiredArgumentBuilder<CommandSource, Integer> priorityArgument = Commands.argument(CommandConstants.PRIORITY.toString(), IntegerArgumentType.integer(1, Integer.MAX_VALUE));
    public static RequiredArgumentBuilder<CommandSource, EntitySelector> playerArgument = Commands.argument(CommandConstants.PLAYER.toString(), EntityArgument.player());
    public static RequiredArgumentBuilder<CommandSource, String> flagArgument = Commands.argument(CommandConstants.FLAG.toString(), StringArgumentType.string());

    public static LiteralArgumentBuilder<CommandSource> regionsLiteral = literal(CommandConstants.REGIONS);
    public static LiteralArgumentBuilder<CommandSource> flagLiteral = literal(CommandConstants.FLAG);
    public static LiteralArgumentBuilder<CommandSource> playerLiteral = literal(CommandConstants.PLAYER);
    public static LiteralArgumentBuilder<CommandSource> teamLiteral = literal(CommandConstants.TEAM);
    public static LiteralArgumentBuilder<CommandSource> ownerLiteral = literal(CommandConstants.OWNER);
    public static LiteralArgumentBuilder<CommandSource> memberLiteral = literal(CommandConstants.MEMBER);
    public static LiteralArgumentBuilder<CommandSource> removeLiteral = literal(CommandConstants.REMOVE);
    public static LiteralArgumentBuilder<CommandSource> addLiteral = literal(CommandConstants.ADD);
    public static LiteralArgumentBuilder<CommandSource> addFlagLiteral = literal(CommandConstants.ADD_FLAG);
    public static LiteralArgumentBuilder<CommandSource> addTeamLiteral = literal(CommandConstants.ADD_TEAM);
    public static LiteralArgumentBuilder<CommandSource> addPlayerLiteral = literal(CommandConstants.ADD_PLAYER);
    public static LiteralArgumentBuilder<CommandSource> removeFlagLiteral = literal(CommandConstants.REMOVE_FLAG);
    public static LiteralArgumentBuilder<CommandSource> removePlayerLiteral = literal(CommandConstants.REMOVE_PLAYER);
    public static LiteralArgumentBuilder<CommandSource> removeTeamLiteral = literal(CommandConstants.REMOVE_TEAM);
    public static LiteralArgumentBuilder<CommandSource> updateLiteral = literal(CommandConstants.UPDATE);
    public static LiteralArgumentBuilder<CommandSource> teleportLiteral = literal(CommandConstants.TELEPORT);
    public static LiteralArgumentBuilder<CommandSource> alertLiteral = literal(CommandConstants.ALERT);
    public static LiteralArgumentBuilder<CommandSource> priorityLiteral = literal(CommandConstants.PRIORITY);
    public static LiteralArgumentBuilder<CommandSource> activateLiteral = literal(CommandConstants.ACTIVATE);
    public static LiteralArgumentBuilder<CommandSource> listLiteral = literal(CommandConstants.LIST);
    public static LiteralArgumentBuilder<CommandSource> infoLiteral = literal(CommandConstants.INFO);
    public static LiteralArgumentBuilder<CommandSource> createLiteral = literal(CommandConstants.CREATE);
    public static LiteralArgumentBuilder<CommandSource> regionLiteral = literal(CommandConstants.REGION);
    public static LiteralArgumentBuilder<CommandSource> dimensionLiteral = literal(CommandConstants.DIMENSION);
    public static LiteralArgumentBuilder<CommandSource> helpLiteral = literal(CommandConstants.HELP);
    public static LiteralArgumentBuilder<CommandSource> dimFlagLiteral = literal(CommandConstants.DIM_FLAG);
    public static LiteralArgumentBuilder<CommandSource> baseCmdLiteral = literal(CommandConstants.BASE_CMD);
    public static LiteralArgumentBuilder<CommandSource> expandLiteral = literal(CommandConstants.EXPAND);


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
        String preamble = "/" + CommandConstants.BASE_CMD;
        String cmdStr = String.join(" ", cmdTokens);
        return preamble + " " + cmdStr;
    }
}
