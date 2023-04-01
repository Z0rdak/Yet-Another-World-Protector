package de.z0rdak.yawp.commands;

import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import de.z0rdak.yawp.config.server.CommandPermissionConfig;
import de.z0rdak.yawp.util.CommandUtil;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.server.command.CommandManager;
import net.minecraft.server.command.ServerCommandSource;
import net.minecraft.text.ClickEvent;
import net.minecraft.text.MutableText;
import net.minecraft.text.Text;

import static de.z0rdak.yawp.util.MessageUtil.buildExecuteCmdComponent;
import static de.z0rdak.yawp.util.MessageUtil.buildHeader;
import static net.minecraft.util.Formatting.AQUA;
import static net.minecraft.util.Formatting.GREEN;

public class CommandRegistry {

    private CommandRegistry() {
    }

    public static void register(CommandDispatcher<ServerCommandSource> commandDispatcher, String modRootCmd) {
        commandDispatcher.register(buildCommands(modRootCmd));
    }

    public static LiteralArgumentBuilder<ServerCommandSource> buildCommands(String baseCmd) {
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
        MutableText cmdStr = Text.translatable("cli.msg.help.1", CommandPermissionConfig.BASE_CMD);
        MessageUtil.sendCmdFeedback(src, buildExecuteCmdComponent(
                Text.literal("=> "),
                Text.translatable("help.tooltip.dim"),
                command, ClickEvent.Action.SUGGEST_COMMAND, GREEN).append(cmdStr));
        MutableText wikiText1 = Text.translatable("help.tooltip.info.wiki.1");
        MutableText wikiText2 = Text.translatable("help.tooltip.info.wiki.2");
        MutableText wikiText3 = Text.translatable("help.tooltip.info.wiki.3");
        MutableText wikiLinkHover = Text.translatable("help.tooltip.info.wiki.link.hover");
        MutableText wikiLink = Text.translatable("help.tooltip.info.wiki.link.text");
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
