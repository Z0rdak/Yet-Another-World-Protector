package de.z0rdak.yawp.commands;

import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.command.CommandSource;

import static de.z0rdak.yawp.util.CommandUtil.literal;

public class RegionTemplateCommands {

    private RegionTemplateCommands(){}

    public static final LiteralArgumentBuilder<CommandSource> TEMPLATE_COMMAND = register();

    private static LiteralArgumentBuilder<CommandSource> register() {
        return literal(CommandConstants.TEMPLATE).executes(ctx -> promptHelp(ctx.getSource()));
    }

    public static int promptHelp(CommandSource src) {
        MessageUtil.sendCmdFeedback(src, MessageUtil.buildHelpHeader("cli.msg.template.help"));
        return 0;
    }
}
