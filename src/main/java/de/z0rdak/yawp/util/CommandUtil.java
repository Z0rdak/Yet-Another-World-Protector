package de.z0rdak.yawp.util;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.commands.CommandConstants;
import de.z0rdak.yawp.config.server.CommandPermissionConfig;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.arguments.DimensionArgument;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.commands.arguments.TeamArgument;
import net.minecraft.resources.ResourceKey;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.level.Level;
import net.minecraft.world.scores.PlayerTeam;
import net.minecraftforge.registries.RegistryObject;

public class CommandUtil {

    public static LiteralArgumentBuilder<CommandSourceStack> literal(CommandConstants constant) {
        return Commands.literal(constant.toString());
    }

    public static ResourceKey<Level> getDimensionArgument(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        return DimensionArgument.getDimension(ctx, CommandConstants.DIMENSION.toString()).dimension();
    }

    public static ServerPlayer getPlayerArgument(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        return EntityArgument.getPlayer(ctx, CommandConstants.PLAYER.toString());
    }

    public static void handleCommandWithoutPlayer(CommandSyntaxException e) {
        e.printStackTrace();
    }


    public static String getRegionNameArgument(CommandContext<CommandSourceStack> ctx) {
        return StringArgumentType.getString(ctx, CommandConstants.REGION.toString());
    }

    public static String getFlagNameArgument(CommandContext<CommandSourceStack> ctx) {
        return StringArgumentType.getString(ctx, CommandConstants.FLAG.toString());
    }

    public static String getAssociateArgument(CommandContext<CommandSourceStack> ctx) {
        return StringArgumentType.getString(ctx, CommandConstants.AFFILIATION.toString());
    }

    public static ServerPlayer getOwnerArgument(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        return EntityArgument.getPlayer(ctx, CommandConstants.OWNER.toString());
    }

    public static PlayerTeam getTeamArgument(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        return TeamArgument.getTeam(ctx, CommandConstants.TEAM.toString());
    }

    public static boolean getActivateArgument(CommandContext<CommandSourceStack> ctx) {
        return BoolArgumentType.getBool(ctx, CommandConstants.ACTIVATE.toString());
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
