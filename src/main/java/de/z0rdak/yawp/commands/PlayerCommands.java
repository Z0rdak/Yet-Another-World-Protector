package de.z0rdak.yawp.commands;

import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.commands.CommandSourceStack;

import static de.z0rdak.yawp.util.CommandUtil.literal;

public final class PlayerCommands {

    public static final LiteralArgumentBuilder<CommandSourceStack> PLAYER_COMMAND = register();

    private PlayerCommands(){}

    private static LiteralArgumentBuilder<CommandSourceStack> register() {
        return literal(CommandConstants.PLAYER).executes(ctx -> promptHelp(ctx.getSource()));
    }

    public static int promptHelp(CommandSourceStack src) {
        MessageUtil.sendCmdFeedback(src, MessageUtil.buildHelpHeader("help.players.header"));
        MessageUtil.sendCmdFeedback(src, MessageUtil.buildHelpSuggestionLink("help.players.1", CommandConstants.PLAYER, CommandConstants.ADD));
        MessageUtil.sendCmdFeedback(src, MessageUtil.buildHelpSuggestionLink("help.players.2", CommandConstants.PLAYER, CommandConstants.REMOVE));
        MessageUtil.sendCmdFeedback(src, MessageUtil.buildHelpSuggestionLink("help.players.3", CommandConstants.PLAYER, CommandConstants.ADD_OFFLINE));
        MessageUtil.sendCmdFeedback(src, MessageUtil.buildHelpSuggestionLink("help.players.4", CommandConstants.PLAYER, CommandConstants.REMOVE_OFFLINE));
        MessageUtil.sendCmdFeedback(src, MessageUtil.buildHelpSuggestionLink("help.players.5", CommandConstants.PLAYER, CommandConstants.LIST));
        return 0;
    }

}
