package de.z0rdak.yawp.commands;

import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.commands.CommandSourceStack;

import static de.z0rdak.yawp.util.CommandUtil.literal;

public final class RegionTriggerCommands {

    private RegionTriggerCommands(){}

    public static final LiteralArgumentBuilder<CommandSourceStack> TRIGGER_COMMAND = register();

    /**
     *
     * /rs global-flag add|remove <flag>
     * <p>
     * /rs trigger region add enter allow
     * /rs trigger region remove leave deny
     * /rs trigger region add enter msg-title
     * /rs trigger region add enter msg-body
     */
    private static LiteralArgumentBuilder<CommandSourceStack> register() {
        return literal(CommandConstants.TRIGGER).executes(ctx -> promptHelp(ctx.getSource()));
    }

    public static int promptHelp(CommandSourceStack src) {
        MessageUtil.sendCmdFeedback(src, MessageUtil.buildHelpHeader("cli.msg.trigger.help"));
        return 0;
    }


}
