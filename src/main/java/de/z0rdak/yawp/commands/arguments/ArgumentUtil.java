package de.z0rdak.yawp.commands.arguments;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.commands.CommandConstants;
import de.z0rdak.yawp.commands.arguments.flag.IFlagArgumentType;
import de.z0rdak.yawp.commands.arguments.flag.RegionFlagArgumentType;
import de.z0rdak.yawp.commands.arguments.region.ContainingOwnedRegionArgumentType;
import de.z0rdak.yawp.commands.arguments.region.RegionArgumentType;
import de.z0rdak.yawp.config.server.CommandPermissionConfig;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.core.flag.FlagType;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.*;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.command.argument.EntityArgumentType;
import net.minecraft.command.argument.TeamArgumentType;
import net.minecraft.command.argument.UuidArgumentType;
import net.minecraft.scoreboard.Team;
import net.minecraft.server.command.CommandManager;

import net.minecraft.server.command.ServerCommandSource;
import net.minecraft.server.network.ServerPlayerEntity;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Set;

import static de.z0rdak.yawp.commands.CommandConstants.*;

public class ArgumentUtil {

    public static LiteralArgumentBuilder<ServerCommandSource> literal(CommandConstants constant) {
        return CommandManager.literal(constant.toString());
    }

    public static DimensionRegionCache getDimCacheArgument(CommandContext<ServerCommandSource> ctx) {
        try {
            return DimensionCacheArgumentType.getDimRegion(ctx, CommandConstants.DIM.toString());
        } catch (CommandSyntaxException e) {
            YetAnotherWorldProtector.LOGGER.error(e.getMessage());
            throw new RuntimeException(e);
        }
    }

    public static DimensionalRegion getDimRegionFromArgument(CommandContext<ServerCommandSource> ctx) throws CommandSyntaxException {
        return DimensionCacheArgumentType.getDimRegion(ctx, CommandConstants.DIM.toString()).getDimensionalRegion();
    }

    public static GlobalRegion getGlobalRegion() {
        return RegionDataManager.get().getGlobalRegion();
    }

    public static AreaType getAreaTypeArgument(CommandContext<ServerCommandSource> ctx) throws CommandSyntaxException {
        return AreaArgumentType.getAreaType(ctx);
    }

    public static String getRegionNameArgument(CommandContext<ServerCommandSource> ctx) {
        return StringArgumentType.getString(ctx, CommandConstants.NAME.toString());
    }

    public static IMarkableRegion getRegionArgument(CommandContext<ServerCommandSource> ctx) {
        try {
            return RegionArgumentType.getRegion(ctx, LOCAL.toString());
        } catch (CommandSyntaxException e) {
            YetAnotherWorldProtector.LOGGER.error(e.getMessage());
            throw new RuntimeException(e);
        }
    }

    public static IProtectedRegion getRegion(CommandContext<ServerCommandSource> ctx, RegionType regionType) throws CommandSyntaxException {
        return RegionArgumentType.getRegion(ctx, regionType);
    }

    public static IProtectedRegion getTargetLocalRegionArgument(CommandContext<ServerCommandSource> ctx) throws CommandSyntaxException {
        return RegionArgumentType.getTargetRegion(ctx, TARGET_REGION.toString());
    }

    public static DimensionRegionCache getTargetDimRegionArgument(CommandContext<ServerCommandSource> ctx) throws CommandSyntaxException {
        return DimensionCacheArgumentType.getDimRegion(ctx, TARGET_DIM.toString());
    }


    public static IMarkableRegion getChildRegionArgument(CommandContext<ServerCommandSource> ctx) throws CommandSyntaxException {
        return RegionArgumentType.getRegion(ctx, CHILD.toString());
    }

    public static IMarkableRegion getContainingOwnedRegionArgument(CommandContext<ServerCommandSource> ctx) throws CommandSyntaxException {
        return ContainingOwnedRegionArgumentType.getRegion(ctx, PARENT.toString());
    }

    public static ServerPlayerEntity getPlayerArgument(CommandContext<ServerCommandSource> ctx) throws CommandSyntaxException {
        return EntityArgumentType.getPlayer(ctx, CommandConstants.PLAYER.toString());
    }

    public static Collection<ServerPlayerEntity> getPlayersArgument(CommandContext<ServerCommandSource> ctx) throws CommandSyntaxException {
        return EntityArgumentType.getPlayers(ctx, CommandConstants.PLAYER.toString());
    }

    public static java.util.UUID getPlayerUUIDArgument(CommandContext<ServerCommandSource> ctx) {
        return UuidArgumentType.getUuid(ctx, CommandConstants.PLAYER_UUID.toString());
    }

    public static List<String> getPlayerNamesArgument(CommandContext<ServerCommandSource> ctx) {
        String[] names = StringArgumentType.getString(ctx, PLAYER_NAMES.toString()).split(" ");
        return Arrays.asList(names);
    }

    public static String getFlagNameArgument(CommandContext<ServerCommandSource> ctx) {
        return StringArgumentType.getString(ctx, CommandConstants.FLAG.toString());
    }

    public static RegionFlag getFlagArgument(CommandContext<ServerCommandSource> ctx) throws CommandSyntaxException {
        return RegionFlagArgumentType.getFlag(ctx, CommandConstants.FLAG.toString());
    }

    public static Set<RegionFlag> getFlagArguments(CommandContext<ServerCommandSource> ctx) throws CommandSyntaxException {
        return RegionFlagArgumentType.getFlags(ctx, CommandConstants.FLAGS.toString());
    }

    public static IFlag getIFlagArgument(CommandContext<ServerCommandSource> ctx) throws CommandSyntaxException {
        return IFlagArgumentType.getFlag(ctx, CommandConstants.FLAG.toString());
    }

    public static String getFlagMsgArgument(CommandContext<ServerCommandSource> ctx) {
        return StringArgumentType.getString(ctx, CommandConstants.MSG.toString());
    }

    public static FlagType getFlagTypeArgument(CommandContext<ServerCommandSource> ctx) {
        return FlagType.of(StringArgumentType.getString(ctx, CommandConstants.TYPE.toString()));
    }

    public static String getGroupArgument(CommandContext<ServerCommandSource> ctx) {
        return StringArgumentType.getString(ctx, CommandConstants.GROUP.toString());
    }

    public static ServerPlayerEntity getOwnerArgument(CommandContext<ServerCommandSource> ctx) throws CommandSyntaxException {
        return EntityArgumentType.getPlayer(ctx, CommandConstants.OWNER.toString());
    }

    public static Team getTeamArgument(CommandContext<ServerCommandSource> ctx) throws CommandSyntaxException {
        return TeamArgumentType.getTeam(ctx, CommandConstants.TEAM.toString());
    }

    public static boolean getAlertArgument(CommandContext<ServerCommandSource> ctx) {
        return BoolArgumentType.getBool(ctx, CommandConstants.ALERT.toString());
    }

    public static boolean getMuteArgument(CommandContext<ServerCommandSource> ctx) {
        return BoolArgumentType.getBool(ctx, CommandConstants.MUTE.toString());
    }


    public static boolean getEnableArgument(CommandContext<ServerCommandSource> ctx) {
        return BoolArgumentType.getBool(ctx, CommandConstants.ENABLE.toString());
    }

    public static FlagState getFlagStateArgument(CommandContext<ServerCommandSource> ctx) {
        String state = StringArgumentType.getString(ctx, STATE.toString());
        return FlagState.from(state);
    }

    public static boolean getOverrideArgument(CommandContext<ServerCommandSource> ctx) {
        return BoolArgumentType.getBool(ctx, CommandConstants.OVERRIDE.toString());
    }

    public static int getPriorityArgument(CommandContext<ServerCommandSource> ctx) {
        return IntegerArgumentType.getInteger(ctx, CommandConstants.PRIORITY.toString());
    }

    public static int getPageNoArgument(CommandContext<ServerCommandSource> ctx) {
        return IntegerArgumentType.getInteger(ctx, PAGE.toString());
    }

    public static String revertCommand(String cmd, CommandConstants toReplace, CommandConstants reverted) {
        String revertedCmd = cmd.replace(toReplace.toString(), reverted.toString());
        return cmd.startsWith("/") ? revertedCmd : "/" + revertedCmd;
    }

    public static String revertCommand(String cmd, String toReplace, String reverted) {
        String revertedCmd = cmd.replace(toReplace, reverted);
        return cmd.startsWith("/") ? revertedCmd : "/" + revertedCmd;
    }

    public static String buildCommandStr(String... cmdTokens) {
        String preamble = "/" + CommandPermissionConfig.BASE_CMD;
        String cmdStr = String.join(" ", cmdTokens);
        return preamble + " " + cmdStr;
    }

    public static String buildSubCmdStr(String... cmdTokens) {
        return String.join(" ", cmdTokens);
    }

    public static String appendSubCommand(String cmd, String... subCommands) {
        return cmd + " " + String.join(" ", subCommands);
    }

    public static String appendSubCommand(String cmd, String subCmd) {
        return cmd + " " + subCmd;
    }
}
