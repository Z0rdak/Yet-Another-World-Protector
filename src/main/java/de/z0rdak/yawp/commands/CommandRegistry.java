package de.z0rdak.yawp.commands;

import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import de.z0rdak.yawp.config.server.CommandPermissionConfig;
import de.z0rdak.yawp.util.CommandUtil;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.command.CommandRegistryAccess;
import net.minecraft.server.command.CommandManager;
import net.minecraft.server.command.ServerCommandSource;
import net.minecraft.text.ClickEvent;
import net.minecraft.text.MutableText;
import net.minecraft.text.Text;

import static de.z0rdak.yawp.util.MessageUtil.*;
import static de.z0rdak.yawp.util.MessageUtil.sendCmdFeedback;
import static net.minecraft.util.Formatting.AQUA;
import static net.minecraft.util.Formatting.GREEN;

public class CommandRegistry {

    private CommandRegistry() {
    }

    private static CommandDispatcher<ServerCommandSource> dispatcher;
    private static boolean isConfigInitialized = false;

    public static void registerCommands(CommandDispatcher<ServerCommandSource> commandDispatcher, CommandRegistryAccess cra, CommandManager.RegistrationEnvironment re) {
        dispatcher = commandDispatcher;
        if (re.dedicated) {
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
                // TODO: Alpha1 - RegionMarker disabled
                // .then(MarkerCommands.build())
                .then(RegionCommands.build());
    }

    private static int promptHelp(ServerCommandSource src) {
        sendCmdFeedback(src, buildHeader("cli.msg.help.header", "YetAnotherWorldProtector help"));
        String command = CommandUtil.buildCommandStr(CommandConstants.DIM.toString());
        MutableText cmdStr = Text.translatableWithFallback("cli.msg.help.1", "Use '/%s dim info | list | add | remove | activate' to manage dimensional regions.", CommandPermissionConfig.BASE_CMD);
        sendCmdFeedback(src, buildExecuteCmdComponent(
                Text.literal("=> "),
                Text.translatableWithFallback("help.tooltip.dim", "Manage dimensional regions with /wp dim <dim> ..."),
                command, ClickEvent.Action.SUGGEST_COMMAND, GREEN).append(cmdStr));
        MutableText wikiText1 = Text.translatableWithFallback("help.tooltip.info.wiki.1","The in-game help is under construction.");
        MutableText wikiText2 = Text.translatableWithFallback("help.tooltip.info.wiki.2","Visit the online wiki for a guide on how to use the mod.");
        MutableText wikiText3 = Text.translatableWithFallback("help.tooltip.info.wiki.3",  "Online-Wiki");
        MutableText wikiLinkHover = Text.translatableWithFallback("help.tooltip.info.wiki.link.hover","Open Wiki in default browser");
        MutableText wikiLink = Text.translatableWithFallback("help.tooltip.info.wiki.link.text","https://github.com/Z0rdak/Yet-Another-World-Protector/wiki");
        MutableText wikiCopyToClipboardLink = buildExecuteCmdComponent(wikiLink, wikiLinkHover, "", ClickEvent.Action.OPEN_URL, AQUA);
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
