package de.z0rdak.yawp.commands.arguments;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.api.commands.CommandConstants;
import de.z0rdak.yawp.commands.arguments.flag.IFlagArgumentType;
import de.z0rdak.yawp.commands.arguments.flag.RegionFlagArgumentType;
import de.z0rdak.yawp.commands.arguments.region.ContainingOwnedRegionArgumentType;
import de.z0rdak.yawp.commands.arguments.region.RegionArgumentType;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.core.flag.FlagType;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.*;
import de.z0rdak.yawp.data.region.DimensionRegionCache;
import de.z0rdak.yawp.data.region.RegionDataManager;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.commands.arguments.TeamArgument;
import net.minecraft.commands.arguments.UuidArgument;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.scores.Team;
import org.jetbrains.annotations.Nullable;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Set;

import static de.z0rdak.yawp.api.commands.CommandConstants.*;

public class ArgumentUtil {

    public static LiteralArgumentBuilder<CommandSourceStack> literal(CommandConstants constant) {
        return Commands.literal(constant.toString());
    }

    public static DimensionRegionCache getDimCacheArgument(CommandContext<CommandSourceStack> ctx) {
        try {
            return DimensionCacheArgumentType.getDimRegion(ctx, CommandConstants.DIM.toString());
        } catch (CommandSyntaxException e) {
            Constants.LOGGER.error(e.getMessage());
            throw new RuntimeException(e);
        }
    }

    public static DimensionalRegion getDimRegionFromArgument(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        return DimensionCacheArgumentType.getDimRegion(ctx, CommandConstants.DIM.toString()).getDimensionalRegion();
    }

    public static GlobalRegion getGlobalRegion() {
        return RegionDataManager.get().getGlobalRegion();
    }

    public static AreaType getAreaTypeArgument(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        return AreaArgumentType.getAreaType(ctx);
    }

    public static String getRegionNameArgument(CommandContext<CommandSourceStack> ctx) {
        return StringArgumentType.getString(ctx, CommandConstants.NAME.toString());
    }

    public static IMarkableRegion getRegionArgument(CommandContext<CommandSourceStack> ctx) {
        try {
            return RegionArgumentType.getRegion(ctx, LOCAL.toString());
        } catch (CommandSyntaxException e) {
            Constants.LOGGER.error(e.getMessage());
            throw new RuntimeException(e);
        }
    }

    public static IProtectedRegion getRegion(CommandContext<CommandSourceStack> ctx, RegionType regionType) throws CommandSyntaxException {
        return RegionArgumentType.getRegion(ctx, regionType);
    }

    public static IProtectedRegion getTargetLocalRegionArgument(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        return RegionArgumentType.getTargetRegion(ctx, TARGET_REGION.toString());
    }

    public static DimensionRegionCache getTargetDimRegionArgument(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        return DimensionCacheArgumentType.getDimRegion(ctx, TARGET_DIM.toString());
    }


    public static IMarkableRegion getChildRegionArgument(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        return RegionArgumentType.getRegion(ctx, CHILD.toString());
    }

    public static IMarkableRegion getContainingOwnedRegionArgument(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        return ContainingOwnedRegionArgumentType.getRegion(ctx, PARENT.toString());
    }

    public static IMarkableRegion getContainingOwnedRegionArgumentWithMarker(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        return ContainingOwnedRegionArgumentType.getRegionWithMarker(ctx, PARENT.toString());
    }

    public static ServerPlayer getPlayerArgument(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        return EntityArgument.getPlayer(ctx, CommandConstants.PLAYER.toString());
    }

    public static Collection<ServerPlayer> getPlayersArgument(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        return EntityArgument.getPlayers(ctx, CommandConstants.PLAYER.toString());
    }

    public static java.util.UUID getPlayerUUIDArgument(CommandContext<CommandSourceStack> ctx) {
        return UuidArgument.getUuid(ctx, CommandConstants.PLAYER_UUID.toString());
    }

    public static List<String> getPlayerNamesArgument(CommandContext<CommandSourceStack> ctx) {
        String[] names = StringArgumentType.getString(ctx, PLAYER_NAMES.toString()).split(" ");
        return Arrays.asList(names);
    }

    public static String getFlagNameArgument(CommandContext<CommandSourceStack> ctx) {
        return StringArgumentType.getString(ctx, CommandConstants.FLAG.toString());
    }

    public static RegionFlag getFlagArgument(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        return RegionFlagArgumentType.getFlag(ctx, CommandConstants.FLAG.toString());
    }

    public static Set<RegionFlag> getFlagArguments(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        return RegionFlagArgumentType.getFlags(ctx, CommandConstants.FLAGS.toString());
    }

    @Nullable
    public static IFlag getIFlagArgument(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        return IFlagArgumentType.getFlag(ctx, CommandConstants.FLAG.toString());
    }

    public static String getFlagMsgArgument(CommandContext<CommandSourceStack> ctx) {
        return StringArgumentType.getString(ctx, CommandConstants.MSG.toString());
    }

    public static FlagType getFlagTypeArgument(CommandContext<CommandSourceStack> ctx) {
        return FlagType.of(StringArgumentType.getString(ctx, CommandConstants.TYPE.toString()));
    }

    public static String getGroupArgument(CommandContext<CommandSourceStack> ctx) {
        return StringArgumentType.getString(ctx, CommandConstants.GROUP.toString());
    }

    public static ServerPlayer getOwnerArgument(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        return EntityArgument.getPlayer(ctx, CommandConstants.OWNER.toString());
    }

    public static Team getTeamArgument(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        return TeamArgument.getTeam(ctx, CommandConstants.TEAM.toString());
    }

    public static boolean getAlertArgument(CommandContext<CommandSourceStack> ctx) {
        return BoolArgumentType.getBool(ctx, CommandConstants.ALERT.toString());
    }

    public static boolean muteArgument(CommandContext<CommandSourceStack> ctx) {
        return BoolArgumentType.getBool(ctx, CommandConstants.MUTE.toString());
    }


    public static boolean getEnableArgument(CommandContext<CommandSourceStack> ctx) {
        return BoolArgumentType.getBool(ctx, CommandConstants.ENABLE.toString());
    }

    public static FlagState getFlagStateArgument(CommandContext<CommandSourceStack> ctx) {
        String state = StringArgumentType.getString(ctx, STATE.toString());
        return FlagState.from(state);
    }

    public static boolean getOverrideArgument(CommandContext<CommandSourceStack> ctx) {
        return BoolArgumentType.getBool(ctx, CommandConstants.OVERRIDE.toString());
    }

    public static int getPriorityArgument(CommandContext<CommandSourceStack> ctx) {
        return IntegerArgumentType.getInteger(ctx, CommandConstants.PRIORITY.toString());
    }

    public static int getPageNoArgument(CommandContext<CommandSourceStack> ctx) {
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
}
