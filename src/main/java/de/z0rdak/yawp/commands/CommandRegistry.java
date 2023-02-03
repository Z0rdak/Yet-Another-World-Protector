package de.z0rdak.yawp.commands;

import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import de.z0rdak.yawp.config.server.CommandPermissionConfig;
import de.z0rdak.yawp.util.CommandUtil;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.network.chat.ClickEvent;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;

import static de.z0rdak.yawp.util.MessageUtil.*;
import static de.z0rdak.yawp.util.MessageUtil.buildExecuteCmdComponent;
import static de.z0rdak.yawp.util.MessageUtil.buildHeader;
import static net.minecraft.ChatFormatting.AQUA;
import static net.minecraft.ChatFormatting.GREEN;

public class CommandRegistry {

    private CommandRegistry() {
    }

    public static void init(CommandDispatcher<CommandSourceStack> commandDispatcher) {
        commandDispatcher.register(buildCommands(CommandPermissionConfig.WP));
        commandDispatcher.register(buildCommands(CommandPermissionConfig.YAWP));
    }

    public static LiteralArgumentBuilder<CommandSourceStack> buildCommands(String baseCmd) {
        return withSubCommands(Commands.literal(baseCmd));
    }

    private static LiteralArgumentBuilder<CommandSourceStack> withSubCommands(LiteralArgumentBuilder<CommandSourceStack> baseCommand) {
        return baseCommand
                .executes(ctx -> promptHelp(ctx.getSource()))
                .then(CommandUtil.literal(CommandConstants.HELP)
                        .executes(ctx -> promptHelp(ctx.getSource())))
                .then(DimensionCommands.DIMENSION_COMMAND)
                .then(MarkerCommands.MARKER_COMMAND)
                .then(RegionCommands.REGION_COMMAND);
    }

    private static int promptHelp(CommandSourceStack src) {
        sendCmdFeedback(src, buildHeader("cli.msg.help.header"));
        String command = CommandUtil.buildCommandStr(CommandConstants.DIMENSION.toString());
        MutableComponent cmdStr = Component.translatable("cli.msg.help.1", CommandPermissionConfig.WP);
        sendCmdFeedback(src, buildExecuteCmdComponent(
                Component.literal("=> "),
                Component.translatable("help.tooltip.dim"),
                command, ClickEvent.Action.SUGGEST_COMMAND, GREEN).append(cmdStr));
        MutableComponent wikiText1 = Component.translatable("help.tooltip.info.wiki.1");
        MutableComponent wikiText2 = Component.translatable("help.tooltip.info.wiki.2");
        MutableComponent wikiText3 = Component.translatable("help.tooltip.info.wiki.3");
        MutableComponent wikiLinkHover = Component.translatable("help.tooltip.info.wiki.link.hover");
        MutableComponent wikiLink = Component.translatable("help.tooltip.info.wiki.link.text");
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
