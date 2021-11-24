package de.z0rdak.regionshield.server.command;

import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.regionshield.server.config.ServerConfigBuilder;
import net.minecraft.command.CommandSource;
import net.minecraft.command.Commands;
import net.minecraft.entity.player.PlayerEntity;

public class CommandRegionShield {

    private CommandRegionShield() {
    }

    public static LiteralArgumentBuilder<CommandSource> register() {
        return withSubCommands(Commands.literal(Command.WP.toString()));
    }

    private static LiteralArgumentBuilder<CommandSource> withSubCommands(LiteralArgumentBuilder<CommandSource> baseCommand) {
        return baseCommand
                .requires(cs -> cs.hasPermission(ServerConfigBuilder.OP_COMMAND_PERMISSION_LEVEL.get()))
                .executes(ctx -> giveHelp(ctx.getSource()))
                .then(Commands.literal(Command.HELP.toString())
                        .executes(ctx -> giveHelp(ctx.getSource())));
                //.then(CommandRegion.REGION_COMMAND)
                //.then(CommandDimension.DIMENSION_COMMAND)
                //.then(CommandExpand.EXPAND_COMMAND)
                //.then(CommandFlag.FLAG_COMMAND)
        //.then(CommandPlayer.PLAYER_COMMAND);
    }

    private static int giveHelp(CommandSource source) {
        try {
            PlayerEntity player = source.getPlayerOrException();
            // TODO: MessageUtils.promptBaseCommandHelp(player);
        } catch (CommandSyntaxException e) {
            e.printStackTrace();
        }
        return 0;

    }
}
