package de.z0rdak.yawp.commands;

import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.commands.CommandSourceStack;

import static de.z0rdak.yawp.util.CommandUtil.literal;

/**
 * Management for flag values for List and Int Flags as well as default flag values
 */
public final class FlagCommands {

    public static final LiteralArgumentBuilder<CommandSourceStack> FLAG_COMMAND = register();

    private FlagCommands(){}

    /**
     * /wp flag <flag> <type> set <argument>
     * /wp flag <flag> <type> add <argument>
     * /wp flag <flag> <type> remove <argument>
     * /wp flag entity_spawning list add minecraft:zombie
     * /wp flag entity_spawning list remove minecraft:zombie
     *
     * /wp flag <flag> mode whitelist|blacklist
     * /wp flag <flag> activate <true|false>
     * /wp flag <flag> remove-from <region>
     * /wp flag <flag> add-to <region>
     * @return
     */
    private static LiteralArgumentBuilder<CommandSourceStack> register() {
        return literal(CommandConstants.FLAG).executes(ctx -> promptHelp(ctx.getSource()));
    }


    public static int promptHelp(CommandSourceStack src) {
        MessageUtil.sendCmdFeedback(src, MessageUtil.buildHelpHeader("help.flags.header"));
        MessageUtil.sendCmdFeedback(src, MessageUtil.buildHelpSuggestionLink("help.flags.1", CommandConstants.FLAG, CommandConstants.ADD));
        MessageUtil.sendCmdFeedback(src, MessageUtil.buildHelpSuggestionLink("help.flags.2", CommandConstants.FLAG, CommandConstants.REMOVE));
        MessageUtil.sendCmdFeedback(src, MessageUtil.buildHelpSuggestionLink("help.flags.3", CommandConstants.FLAG, CommandConstants.LIST));
        return 0;
    }

}
