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

import static de.z0rdak.yawp.YAWPCommon.VERSION;
import static de.z0rdak.yawp.util.ChatComponentBuilder.buildHeader;
import static de.z0rdak.yawp.util.ChatComponentBuilder.buildHelpStartComponent;
import static de.z0rdak.yawp.api.MessageSender.sendCmdFeedback;
import static de.z0rdak.yawp.util.ChatLinkBuilder.*;

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
                .then(GlobalCommands.build())
                .then(DimensionCommands.build())
                .then(RegionCommands.build())
                .then(ShortcutCommands.buildShow())
                .then(ShortcutCommands.buildHide())
                .then(ShortcutCommands.buildInfoLocal())
                .then(ShortcutCommands.buildCreateLocal())
                .then(ShortcutCommands.buildDeleteLocal());
    }

    private static int promptHelp(CommandSourceStack src) {
        var versionCopyLink = buildVersionCopyLink(VERSION);
        sendCmdFeedback(src, buildHeader(Component.translatableWithFallback("help.header", "== Yet Another World Protector ==")));
        MutableComponent versionDisclaimer = Component.translatableWithFallback("help.tooltip.version.info.disclaimer", "Disclaimer: This version of YAWP is on the cutting edge of development.");
        MutableComponent versionInfo = Component.translatableWithFallback("help.tooltip.version.info",  "You are running YAWP '%s'. Please provide the version info when requesting help.", versionCopyLink);
        MutableComponent wikiInfo = Component.translatableWithFallback("help.tooltip.wiki.info", "The wiki is likely to be outdated in comparison of the features present in this version. Please visit the discord for help/questions.");
        sendCmdFeedback(src, versionDisclaimer);
        sendCmdFeedback(src, versionInfo);
        sendCmdFeedback(src, wikiInfo);
        sendCmdFeedback(src, Messages.substitutable(" => %s", buildDiscordLink()));
        sendCmdFeedback(src, Messages.substitutable(" => %s", buildWikiLink()));
        sendCmdFeedback(src, Messages.substitutable(" => %s", buildHelpStartComponent()));
        return 0;
    }
}
