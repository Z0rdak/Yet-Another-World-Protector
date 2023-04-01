package de.z0rdak.yawp.util;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.commands.CommandConstants;
import de.z0rdak.yawp.commands.arguments.AreaArgumentType;
import de.z0rdak.yawp.commands.arguments.DimensionCacheArgumentType;
import de.z0rdak.yawp.commands.arguments.flag.RegionFlagArgumentType;
import de.z0rdak.yawp.commands.arguments.region.RegionArgumentType;
import de.z0rdak.yawp.config.server.CommandPermissionConfig;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.flag.FlagType;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import net.minecraft.command.argument.EntityArgumentType;
import net.minecraft.command.argument.TeamArgumentType;
import net.minecraft.scoreboard.Team;
import net.minecraft.server.command.CommandManager;
import net.minecraft.server.command.ServerCommandSource;
import net.minecraft.server.network.ServerPlayerEntity;

import static de.z0rdak.yawp.commands.CommandConstants.*;

public class CommandUtil {

    public static LiteralArgumentBuilder<ServerCommandSource> literal(CommandConstants constant) {
        return CommandManager.literal(constant.toString());
    }

    public static DimensionRegionCache getDimCacheArgument(CommandContext<ServerCommandSource> ctx) throws CommandSyntaxException {
        return DimensionCacheArgumentType.getDimRegion(ctx, CommandConstants.DIM.toString());
    }

    public static AreaType getAreaTypeArgument(CommandContext<ServerCommandSource> ctx) throws CommandSyntaxException {
        return AreaArgumentType.getAreaType(ctx);
    }

    public static String getRegionNameArgument(CommandContext<ServerCommandSource> ctx) {
        return StringArgumentType.getString(ctx, REGION.toString());
    }

    public static IMarkableRegion getRegionArgument(CommandContext<ServerCommandSource> ctx) throws CommandSyntaxException {
        return RegionArgumentType.getRegion(ctx, REGION.toString());
    }

    public static IMarkableRegion getChildRegionArgument(CommandContext<ServerCommandSource> ctx) throws CommandSyntaxException {
        return RegionArgumentType.getRegion(ctx, CHILD.toString());
    }

    public static IMarkableRegion getParentRegionArgument(CommandContext<ServerCommandSource> ctx) throws CommandSyntaxException {
        return RegionArgumentType.getRegionInPlayerDim(ctx, PARENT.toString());
    }

    public static ServerPlayerEntity getPlayerArgument(CommandContext<ServerCommandSource> ctx) throws CommandSyntaxException {
        return EntityArgumentType.getPlayer(ctx, CommandConstants.PLAYER.toString());
    }

    public static String getFlagNameArgument(CommandContext<ServerCommandSource> ctx) {
        return StringArgumentType.getString(ctx, CommandConstants.FLAG.toString());
    }

    public static RegionFlag getFlagArgument(CommandContext<ServerCommandSource> ctx) throws CommandSyntaxException {
        return RegionFlagArgumentType.getFlag(ctx, CommandConstants.FLAG.toString());
    }

    public static FlagType getFlagTypeArgument(CommandContext<ServerCommandSource> ctx) {
        return FlagType.of(StringArgumentType.getString(ctx, CommandConstants.TYPE.toString()));
    }

    public static String getAffiliationArgument(CommandContext<ServerCommandSource> ctx) {
        return StringArgumentType.getString(ctx, CommandConstants.AFFILIATION.toString());
    }

    public static ServerPlayerEntity getOwnerArgument(CommandContext<ServerCommandSource> ctx) throws CommandSyntaxException {
        return EntityArgumentType.getPlayer(ctx, CommandConstants.OWNER.toString());
    }

    public static Team getTeamArgumentType(CommandContext<ServerCommandSource> ctx) throws CommandSyntaxException {
        return TeamArgumentType.getTeam(ctx, CommandConstants.TEAM.toString());
    }

    public static boolean getAlertArgument(CommandContext<ServerCommandSource> ctx) {
        return !BoolArgumentType.getBool(ctx, CommandConstants.ALERT.toString());
    }

    public static boolean getEnableArgument(CommandContext<ServerCommandSource> ctx) {
        return BoolArgumentType.getBool(ctx, CommandConstants.ENABLE.toString());
    }

    public static int getPriorityArgument(CommandContext<ServerCommandSource> ctx) {
        return IntegerArgumentType.getInteger(ctx, CommandConstants.PRIORITY.toString());
    }

    public static int getPageNoArgument(CommandContext<ServerCommandSource> ctx) {
        return IntegerArgumentType.getInteger(ctx, PAGE.toString());
    }

    public static String revertCommand(String cmd, CommandConstants toReplace, CommandConstants reverted) {
        String revertedCmd = cmd.replace(toReplace.toString(), reverted.toString());
        return cmd.startsWith("/") ? revertedCmd : "/" + revertedCmd ;
    }

    public static String revertCommand(String cmd, String toReplace, String reverted) {
        String revertedCmd = cmd.replace(toReplace, reverted);
        return cmd.startsWith("/") ? revertedCmd : "/" + revertedCmd ;
    }

    public static String buildCommandStr(String... cmdTokens) {
        String preamble = "/" + CommandPermissionConfig.BASE_CMD;
        String cmdStr = String.join(" ", cmdTokens);
        return preamble + " " + cmdStr;
    }
}
