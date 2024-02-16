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
import de.z0rdak.yawp.commands.arguments.region.RegionArgumentType;
import de.z0rdak.yawp.config.server.CommandPermissionConfig;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.flag.FlagType;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.*;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.command.CommandSource;
import net.minecraft.command.Commands;
import net.minecraft.command.arguments.EntityArgument;
import net.minecraft.command.arguments.TeamArgument;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.scoreboard.ScorePlayerTeam;

import java.util.Collection;
import java.util.Set;

import static de.z0rdak.yawp.commands.CommandConstants.*;

public class ArgumentUtil {

    public static LiteralArgumentBuilder<CommandSource> literal(CommandConstants constant) {
        return Commands.literal(constant.toString());
    }

    public static DimensionRegionCache getDimCacheArgument(CommandContext<CommandSource> ctx) {
        try {
            return DimensionCacheArgumentType.getDimRegion(ctx, CommandConstants.DIM.toString());
        } catch (CommandSyntaxException e) {
            YetAnotherWorldProtector.LOGGER.error(e.getMessage());
            throw new RuntimeException(e);
        }
    }

    public static DimensionalRegion getDimRegionFromArgument(CommandContext<CommandSource> ctx) throws CommandSyntaxException {
        return DimensionCacheArgumentType.getDimRegion(ctx, CommandConstants.DIM.toString()).getDimensionalRegion();
    }

    public static GlobalRegion getGlobalRegion() {
        return RegionDataManager.get().getGlobalRegion();
    }

    public static AreaType getAreaTypeArgument(CommandContext<CommandSource> ctx) throws CommandSyntaxException {
        return AreaArgumentType.getAreaType(ctx);
    }

    public static String getRegionNameArgument(CommandContext<CommandSource> ctx) {
        return StringArgumentType.getString(ctx, CommandConstants.NAME.toString());
    }

    public static IMarkableRegion getRegionArgument(CommandContext<CommandSource> ctx) {
        try {
            return RegionArgumentType.getRegion(ctx, CommandConstants.LOCAL.toString());
        } catch (CommandSyntaxException e) {
            YetAnotherWorldProtector.LOGGER.error(e.getMessage());
            throw new RuntimeException(e);
        }
    }

    public static IProtectedRegion getRegion(CommandContext<CommandSource> ctx, RegionType regionType) throws CommandSyntaxException {
        return RegionArgumentType.getRegion(ctx, regionType);
    }

    public static IProtectedRegion getTargetLocalRegionArgument(CommandContext<CommandSource> ctx) throws CommandSyntaxException {
        return RegionArgumentType.getTargetRegion(ctx, TARGET_REGION.toString());
    }

    public static DimensionRegionCache getTargetDimRegionArgument(CommandContext<CommandSource> ctx) throws CommandSyntaxException {
        return DimensionCacheArgumentType.getDimRegion(ctx, TARGET_DIM.toString());
    }


    public static IMarkableRegion getChildRegionArgument(CommandContext<CommandSource> ctx) throws CommandSyntaxException {
        return RegionArgumentType.getRegion(ctx, CHILD.toString());
    }

    public static IMarkableRegion getParentRegionArgument(CommandContext<CommandSource> ctx) throws CommandSyntaxException {
        return RegionArgumentType.getRegionInPlayerDim(ctx, PARENT.toString());
    }

    public static ServerPlayerEntity getPlayerArgument(CommandContext<CommandSource> ctx) throws CommandSyntaxException {
        return EntityArgument.getPlayer(ctx, CommandConstants.PLAYER.toString());
    }

    public static Collection<ServerPlayerEntity> getPlayersArgument(CommandContext<CommandSource> ctx) throws CommandSyntaxException {
        return EntityArgument.getPlayers(ctx, CommandConstants.PLAYER.toString());
    }


    public static String getFlagNameArgument(CommandContext<CommandSource> ctx) {
        return StringArgumentType.getString(ctx, CommandConstants.FLAG.toString());
    }

    public static RegionFlag getFlagArgument(CommandContext<CommandSource> ctx) throws CommandSyntaxException {
        return RegionFlagArgumentType.getFlag(ctx, CommandConstants.FLAG.toString());
    }

    public static Set<RegionFlag> getFlagArguments(CommandContext<CommandSource> ctx) throws CommandSyntaxException {
        return RegionFlagArgumentType.getFlags(ctx, CommandConstants.FLAG.toString());
    }

    public static IFlag getIFlagArgument(CommandContext<CommandSource> ctx) throws CommandSyntaxException {
        return IFlagArgumentType.getFlag(ctx, CommandConstants.FLAG.toString());
    }

    public static String getFlagMsgArgument(CommandContext<CommandSource> ctx) {
        return StringArgumentType.getString(ctx, CommandConstants.MSG.toString());
    }

    public static FlagType getFlagTypeArgument(CommandContext<CommandSource> ctx) {
        return FlagType.of(StringArgumentType.getString(ctx, CommandConstants.TYPE.toString()));
    }

    public static String getGroupArgument(CommandContext<CommandSource> ctx) {
        return StringArgumentType.getString(ctx, CommandConstants.GROUP.toString());
    }

    public static ServerPlayerEntity getOwnerArgument(CommandContext<CommandSource> ctx) throws CommandSyntaxException {
        return EntityArgument.getPlayer(ctx, CommandConstants.OWNER.toString());
    }

    public static ScorePlayerTeam getTeamArgument(CommandContext<CommandSource> ctx) throws CommandSyntaxException {
        return TeamArgument.getTeam(ctx, CommandConstants.TEAM.toString());
    }

    public static boolean getAlertArgument(CommandContext<CommandSource> ctx) {
        return !BoolArgumentType.getBool(ctx, CommandConstants.ALERT.toString());
    }

    public static boolean getMuteArgument(CommandContext<CommandSource> ctx) {
        return BoolArgumentType.getBool(ctx, CommandConstants.MUTE.toString());
    }


    public static boolean getEnableArgument(CommandContext<CommandSource> ctx) {
        return BoolArgumentType.getBool(ctx, CommandConstants.ENABLE.toString());
    }

    public static boolean getNegationArgument(CommandContext<CommandSource> ctx) {
        return BoolArgumentType.getBool(ctx, CommandConstants.OVERRIDE.toString());
    }

    public static int getPriorityArgument(CommandContext<CommandSource> ctx) {
        return IntegerArgumentType.getInteger(ctx, CommandConstants.PRIORITY.toString());
    }

    public static int getPageNoArgument(CommandContext<CommandSource> ctx) {
        return IntegerArgumentType.getInteger(ctx, PAGE.toString());
    }

    public static String revertCommand(String cmd, CommandConstants toReplace, CommandConstants reverted) {
        String revertedCmd = cmd.replace(toReplace.toString(), reverted.toString());
        return cmd.startsWith("/") ? revertedCmd : "/" + revertedCmd ;
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
