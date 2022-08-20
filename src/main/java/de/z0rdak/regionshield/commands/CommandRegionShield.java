package de.z0rdak.regionshield.commands;

import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.regionshield.config.ServerConfigBuilder;
import de.z0rdak.regionshield.util.MessageUtil;
import de.z0rdak.regionshield.util.PlayerUtils;
import de.z0rdak.regionshield.util.RegionUtil;
import net.minecraft.command.CommandSource;
import net.minecraft.command.Commands;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.util.text.TranslationTextComponent;

import static de.z0rdak.regionshield.util.CommandUtil.helpLiteral;

public class CommandRegionShield {

    private CommandRegionShield() {
    }

    public static void init(CommandDispatcher<CommandSource> commandDispatcher) {
        commandDispatcher.register(register());
    }

    public static LiteralArgumentBuilder<CommandSource> register() {
        return withSubCommands(Commands.literal(CommandConstants.BASE_CMD.toString()));
    }

    private static LiteralArgumentBuilder<CommandSource> withSubCommands(LiteralArgumentBuilder<CommandSource> baseCommand) {
        return baseCommand
                .requires(source -> source.hasPermission(ServerConfigBuilder.RS_CMD_OP_LEVEL.get()))
                .executes(ctx -> giveHelp(ctx.getSource()))
                .then(helpLiteral
                        .executes(ctx -> giveHelp(ctx.getSource())))
                .then(DimensionCommands.DIMENSION_COMMAND)
                //.then(RegionCommands.REGION_COMMAND)
                //.then(RegionCommands.REGIONS_COMMAND)
                //.then(DimensionFlagCommands.DIMENSION_FLAGS_COMMAND);
                //.then(CommandExpand.EXPAND_COMMAND)
                //.then(CommandFlag.FLAG_COMMAND)
        //.then(CommandPlayer.PLAYER_COMMAND);
        ;
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


    public static int promptBaseCommandHelp(CommandSource src) {
        MessageUtil.sendCmdFeedback(src, MessageUtil.buildHelpHeader("help.wp.header"));
        MessageUtil.sendCmdFeedback(src, MessageUtil.buildHelpLink("help.wp.1", CommandConstants.EXPAND));
        MessageUtil.sendCmdFeedback(src, MessageUtil.buildHelpLink("help.wp.4", CommandConstants.REGION));
        MessageUtil.sendCmdFeedback(src, MessageUtil.buildHelpLink("help.wp.2", CommandConstants.FLAG));
        MessageUtil.sendCmdFeedback(src, MessageUtil.buildHelpLink("help.wp.3", CommandConstants.PLAYER));
        MessageUtil.sendCmdFeedback(src, new TranslationTextComponent("help.wp.5"));
        return 0;
    }

}
