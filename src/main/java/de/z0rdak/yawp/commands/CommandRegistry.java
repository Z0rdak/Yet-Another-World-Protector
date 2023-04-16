package de.z0rdak.yawp.commands;

import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import de.z0rdak.yawp.config.server.CommandPermissionConfig;
import de.z0rdak.yawp.util.CommandUtil;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.server.command.CommandManager;
import net.minecraft.server.command.ServerCommandSource;
import net.minecraft.text.ClickEvent;
import net.minecraft.text.LiteralText;
import net.minecraft.text.MutableText;
import net.minecraft.text.TranslatableText;

import static de.z0rdak.yawp.util.MessageUtil.buildExecuteCmdComponent;
import static de.z0rdak.yawp.util.MessageUtil.buildHeader;
import static net.minecraft.util.Formatting.AQUA;
import static net.minecraft.util.Formatting.GREEN;

public class CommandRegistry {

    private CommandRegistry() {
    }

    private static CommandDispatcher<ServerCommandSource> dispatcher;
    private static boolean isConfigInitialized = false;

    public static void registerCommands(CommandDispatcher<ServerCommandSource> commandDispatcher, boolean isDedicated) {
        dispatcher = commandDispatcher;
        if (isDedicated) {
            if (isConfigInitialized) {
                CommandRegistry.register(CommandPermissionConfig.BASE_CMD);
            }
        }
    }

    public static void register(String modRootCmd) {
        dispatcher.register(buildCommands(modRootCmd));
        CommandRegistry.isConfigInitialized = true;
    }

    private static LiteralArgumentBuilder<ServerCommandSource> buildCommands(String baseCmd) {
        return CommandManager.literal(baseCmd)
                .executes(ctx -> promptHelp(ctx.getSource()))
                .then(CommandUtil.literal(CommandConstants.HELP)
                        .executes(ctx -> promptHelp(ctx.getSource())))
                .then(DimensionCommands.build())
                .then(MarkerCommands.build())
                .then(RegionCommands.build());
    }

    private static int promptHelp(ServerCommandSource src) {
        MessageUtil.sendCmdFeedback(src, buildHeader("cli.msg.help.header"));
        String command = CommandUtil.buildCommandStr(CommandConstants.DIM.toString());
        MutableText cmdStr = new TranslatableText("cli.msg.help.1", CommandPermissionConfig.BASE_CMD);
        MessageUtil.sendCmdFeedback(src, buildExecuteCmdComponent(
                new LiteralText("=> "),
                new TranslatableText("help.tooltip.dim"),
                command, ClickEvent.Action.SUGGEST_COMMAND, GREEN).append(cmdStr));
        MutableText wikiText1 = new TranslatableText("help.tooltip.info.wiki.1");
        MutableText wikiText2 = new TranslatableText("help.tooltip.info.wiki.2");
        MutableText wikiText3 = new TranslatableText("help.tooltip.info.wiki.3");
        MutableText wikiLinkHover = new TranslatableText("help.tooltip.info.wiki.link.hover");
        MutableText wikiLink = new TranslatableText("help.tooltip.info.wiki.link.text");
        MutableText wikiCopyToClipboardLink = buildExecuteCmdComponent(wikiLink, wikiLinkHover, "", ClickEvent.Action.OPEN_URL, AQUA);
        wikiText1.append("\n")
                .append(wikiText2)
                .append("\n")
                .append(wikiText3)
                .append(": ")
                .append(wikiCopyToClipboardLink);
        MessageUtil.sendCmdFeedback(src, wikiText1);
        return 0;
    }
}
