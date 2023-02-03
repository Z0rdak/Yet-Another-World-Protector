package de.z0rdak.yawp.commands;

import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.commands.CommandSourceStack;

import static de.z0rdak.yawp.util.CommandUtil.literal;

public class RegionTemplateCommands {

    private RegionTemplateCommands(){}

    public static final LiteralArgumentBuilder<CommandSourceStack> TEMPLATE_COMMAND = register();

    private static LiteralArgumentBuilder<CommandSourceStack> register() {
        return literal(CommandConstants.TEMPLATE).executes(ctx -> promptHelp(ctx.getSource()));
    }

    public static int promptHelp(CommandSourceStack src) {
        MessageUtil.sendCmdFeedback(src, MessageUtil.buildHeader("cli.msg.template.help"));
        return 0;
    }
}
