package de.z0rdak.regionshield.commands;

import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.regionshield.config.ServerConfigBuilder;
import de.z0rdak.regionshield.util.PlayerUtils;
import de.z0rdak.regionshield.util.RegionUtil;
import net.minecraft.command.CommandSource;
import net.minecraft.command.Commands;
import net.minecraft.entity.player.PlayerEntity;

public class CommandRegionShield {

    private CommandRegionShield() {
    }

    public static LiteralArgumentBuilder<CommandSource> register() {
        return withSubCommands(Commands.literal(CommandConstants.BASE_CMD.toString()));
    }

    private static LiteralArgumentBuilder<CommandSource> withSubCommands(LiteralArgumentBuilder<CommandSource> baseCommand) {
        return baseCommand
                .requires(CommandRegionShield::hasPermission)
                .executes(ctx -> giveHelp(ctx.getSource()))
                .then(Commands.literal(CommandConstants.HELP.toString())
                        .executes(ctx -> giveHelp(ctx.getSource())))
                .then(RegionCommands.REGION_COMMAND)
                .then(RegionCommands.REGIONS_COMMAND)
                .then(DimensionCommands.DIMENSION_COMMAND);
                //.then(CommandExpand.EXPAND_COMMAND)
                //.then(CommandFlag.FLAG_COMMAND)
        //.then(CommandPlayer.PLAYER_COMMAND);
    }

    private static boolean hasPermission(CommandSource source) {
        try {
            PlayerEntity player = source.getPlayerOrException();
            return PlayerUtils.hasPermission(player) || source.hasPermission(ServerConfigBuilder.RS_CMD_OP_LEVEL.get());
        } catch (CommandSyntaxException e) {
            e.printStackTrace();
        }
        return false;
    }

    private static int giveHelp(CommandSource source) {
        try {
            PlayerEntity player = source.getPlayerOrException();
            // TODO: MessageUtil.promptBaseCommandHelp(player);
        } catch (CommandSyntaxException e) {
            e.printStackTrace();
        }
        return 0;

    }
}
