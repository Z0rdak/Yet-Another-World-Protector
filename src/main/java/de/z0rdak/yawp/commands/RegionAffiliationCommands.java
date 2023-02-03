package de.z0rdak.yawp.commands;

import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.commands.CommandSourceStack;

import static de.z0rdak.yawp.util.CommandUtil.literal;

public class RegionAffiliationCommands {

    private RegionAffiliationCommands(){}

    public static final LiteralArgumentBuilder<CommandSourceStack> AFFILIATION_COMMAND = register();

    private static LiteralArgumentBuilder<CommandSourceStack> register() {
        return literal(CommandConstants.AFFILIATION).executes(ctx -> promptHelp(ctx.getSource()));
    }

    public static int promptHelp(CommandSourceStack src) {
        MessageUtil.sendCmdFeedback(src, MessageUtil.buildHeader("cli.msg.affiliation.help"));
        return 0;
    }
}
