package de.z0rdak.yawp.commands;

import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import de.z0rdak.yawp.config.server.CommandPermissionConfig;
import de.z0rdak.yawp.util.CommandUtil;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.network.chat.ClickEvent;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.network.chat.TranslatableComponent;

import static de.z0rdak.yawp.util.MessageUtil.*;
import static net.minecraft.ChatFormatting.AQUA;
import static net.minecraft.ChatFormatting.GREEN;

public class CommandRegistry {

    private CommandRegistry() {
    }

    public static void register(CommandDispatcher<CommandSourceStack> commandDispatcher, String modRootCmd) {
        commandDispatcher.register(buildCommands(modRootCmd));
    }

    public static LiteralArgumentBuilder<CommandSourceStack> buildCommands(String baseCmd) {
        return Commands.literal(baseCmd)
                .executes(ctx -> promptHelp(ctx.getSource()))
                .then(CommandUtil.literal(CommandConstants.HELP)
                        .executes(ctx -> promptHelp(ctx.getSource())))
                .then(DimensionCommands.build())
                .then(MarkerCommands.build())
                .then(RegionCommands.build());
    }

    private static int promptHelp(CommandSourceStack src) {
        sendCmdFeedback(src, buildHeader("cli.msg.help.header"));
        String command = CommandUtil.buildCommandStr(CommandConstants.DIM.toString());
        MutableComponent cmdStr = new TranslatableComponent("cli.msg.help.1", CommandPermissionConfig.BASE_CMD);
        sendCmdFeedback(src, buildExecuteCmdComponent(
                new TextComponent("=> "),
                new TranslatableComponent("help.tooltip.dim"),
                command, ClickEvent.Action.SUGGEST_COMMAND, GREEN).append(cmdStr));
        MutableComponent wikiText1 = new TranslatableComponent("help.tooltip.info.wiki.1");
        MutableComponent wikiText2 = new TranslatableComponent("help.tooltip.info.wiki.2");
        MutableComponent wikiText3 = new TranslatableComponent("help.tooltip.info.wiki.3");
        MutableComponent wikiLinkHover = new TranslatableComponent("help.tooltip.info.wiki.link.hover");
        MutableComponent wikiLink = new TranslatableComponent("help.tooltip.info.wiki.link.text");
        MutableComponent wikiCopyToClipboardLink = buildExecuteCmdComponent(wikiLink, wikiLinkHover, "", ClickEvent.Action.OPEN_URL, AQUA);
        wikiText1.append("\n")
                .append(wikiText2)
                .append("\n")
                .append(wikiText3)
                .append(": ")
                .append(wikiCopyToClipboardLink);
        sendCmdFeedback(src, wikiText1);
        return 0;
    }
}
