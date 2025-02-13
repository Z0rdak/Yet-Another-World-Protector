package de.z0rdak.yawp.commands;

import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import de.z0rdak.yawp.api.commands.CommandConstants;
import de.z0rdak.yawp.api.permission.Permissions;
import de.z0rdak.yawp.commands.arguments.ArgumentUtil;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.util.text.Messages;
import net.minecraft.commands.CommandBuildContext;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;

import static de.z0rdak.yawp.util.ChatComponentBuilder.buildHeader;
import static de.z0rdak.yawp.util.ChatComponentBuilder.buildHelpStartComponent;
import static de.z0rdak.yawp.util.ChatLinkBuilder.buildWikiLink;
import static de.z0rdak.yawp.api.MessageSender.sendCmdFeedback;

public final class CommandRegistry {

    private CommandRegistry() {
    }

    public static void registerCommands(CommandDispatcher<CommandSourceStack> cmdDispatcher, CommandBuildContext registryAccess, Commands.CommandSelection env) {
        if (env == Commands.CommandSelection.DEDICATED || env == Commands.CommandSelection.INTEGRATED) {
            try {
                LiteralArgumentBuilder<CommandSourceStack> modCmds = buildCommands();
                cmdDispatcher.register(modCmds);
            }
            catch (Exception e) {
                // Nothing to do here. Since multi project structure was introduced,
                // the config loads differently and will need more than one attempt to register the commands
            }
        }
    }

    private static LiteralArgumentBuilder<CommandSourceStack> buildCommands() {
        return Commands.literal(Constants.MOD_ID)
                .requires(Permissions.get()::isAllowedForNonOp)
                .executes(ctx -> promptHelp(ctx.getSource()))
                .then(ArgumentUtil.literal(CommandConstants.HELP)
                        .executes(ctx -> promptHelp(ctx.getSource())))
                .then(FlagCommands.build())
                .then(MarkerCommands.build())
                .then(GlobalCommands.build())
                .then(DimensionCommands.build())
                .then(RegionCommands.build());
    }

    private static int promptHelp(CommandSourceStack src) {
        sendCmdFeedback(src, buildHeader(Component.translatableWithFallback("help.header", "== Yet Another World Protector - Help ==")));
        MutableComponent wikiHint = Component.translatableWithFallback("help.tooltip.wiki.detail", "The in-game help is under construction. Visit the online wiki for a guide on how to use the mod.");
        MutableComponent wikiText = Component.translatableWithFallback("help.tooltip.wiki", "Online-Wiki");
        sendCmdFeedback(src, wikiHint);
        sendCmdFeedback(src, Messages.substitutable("%s: %s", wikiText, buildWikiLink()));
        sendCmdFeedback(src, Messages.substitutable(" => %s", buildHelpStartComponent()));
        return 0;
    }
}
