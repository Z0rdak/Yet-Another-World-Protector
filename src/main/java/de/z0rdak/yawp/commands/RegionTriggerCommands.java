package de.z0rdak.yawp.commands;

import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.command.CommandSource;

import static de.z0rdak.yawp.util.CommandUtil.literal;

public final class RegionTriggerCommands {

    private RegionTriggerCommands(){}

    public static final LiteralArgumentBuilder<CommandSource> TRIGGER_COMMAND = register();

    /**
     *
     * /rs global-flag add|remove <flag>
     * <p>
     * /rs trigger region add enter allow
     * /rs trigger region remove leave deny
     * /rs trigger region add enter msg-title
     * /rs trigger region add enter msg-body
     */
    private static LiteralArgumentBuilder<CommandSource> register() {
        return literal(CommandConstants.TRIGGER).executes(ctx -> promptHelp(ctx.getSource()));
    }

    public static int promptHelp(CommandSource src) {
        MessageUtil.sendCmdFeedback(src, MessageUtil.buildHeader("cli.msg.trigger.help"));
        return 0;
    }


}
