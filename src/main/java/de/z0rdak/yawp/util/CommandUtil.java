package de.z0rdak.yawp.util;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.commands.CommandConstants;
import de.z0rdak.yawp.commands.arguments.*;
import de.z0rdak.yawp.commands.arguments.flag.FlagArgumentType;
import de.z0rdak.yawp.commands.arguments.region.RegionArgumentType;
import de.z0rdak.yawp.config.server.CommandPermissionConfig;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.arguments.DimensionArgument;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.commands.arguments.TeamArgument;
import net.minecraft.resources.ResourceKey;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.level.Level;
import net.minecraft.world.scores.PlayerTeam;

import static de.z0rdak.yawp.commands.CommandConstants.*;

public class CommandUtil {

    public static LiteralArgumentBuilder<CommandSourceStack> literal(CommandConstants constant) {
        return Commands.literal(constant.toString());
    }

    public static ResourceKey<Level> getDimensionArgument(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        return DimensionArgument.getDimension(ctx, CommandConstants.DIMENSION.toString()).dimension();
    }

    public static DimensionalRegion getDimRegionArgument(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        return DimensionalRegionArgumentType.getDimRegion(ctx, CommandConstants.DIMENSION.toString());
    }

    public static DimensionRegionCache getDimCacheArgument(CommandContext<CommandSourceStack> ctx) {
        return DimensionCacheArgumentType.getDimRegion(ctx, CommandConstants.DIMENSION.toString());
    }

    public static AreaType getAreaTypeArgument(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        return AreaArgumentType.getAreaType(ctx);
    }

    public static String getRegionNameArgument(CommandContext<CommandSourceStack> ctx) {
        return StringArgumentType.getString(ctx, REGION.toString());
    }

    public static IMarkableRegion getRegionArgument(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        return RegionArgumentType.getRegion(ctx, REGION.toString());
    }

    public static IMarkableRegion getChildRegionArgument(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        return RegionArgumentType.getRegion(ctx, CHILD.toString());
    }

    public static IMarkableRegion getParentRegionArgument(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        return RegionArgumentType.getRegion(ctx, PARENT_REGION.toString());
    }


    public static ServerPlayer getPlayerArgument(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        return EntityArgument.getPlayer(ctx, CommandConstants.PLAYER.toString());
    }

    public static String getFlagNameArgument(CommandContext<CommandSourceStack> ctx) {
        return StringArgumentType.getString(ctx, CommandConstants.FLAG.toString());
    }

    public static IFlag getFlagArgument(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        return FlagArgumentType.getFlag(ctx, CommandConstants.FLAG.toString());
    }

    public static String getAffiliationArgument(CommandContext<CommandSourceStack> ctx) {
        return StringArgumentType.getString(ctx, CommandConstants.AFFILIATION.toString());
    }

    public static ServerPlayer getOwnerArgument(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        return EntityArgument.getPlayer(ctx, CommandConstants.OWNER.toString());
    }

    public static PlayerTeam getTeamArgument(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        return TeamArgument.getTeam(ctx, CommandConstants.TEAM.toString());
    }

    public static boolean getAlertArgument(CommandContext<CommandSourceStack> ctx) {
        return BoolArgumentType.getBool(ctx, CommandConstants.ALERT.toString());
    }
    public static boolean getEnableArgument(CommandContext<CommandSourceStack> ctx) {
        return BoolArgumentType.getBool(ctx, CommandConstants.ENABLE.toString());
    }

    public static int getPriorityArgument(CommandContext<CommandSourceStack> ctx) {
        return IntegerArgumentType.getInteger(ctx, CommandConstants.PRIORITY.toString());
    }

    public static String buildCommandStr(String... cmdTokens){
        String preamble = "/" + CommandPermissionConfig.BASE_CMD;
        String cmdStr = String.join(" ", cmdTokens);
        return preamble + " " + cmdStr;
    }
}
