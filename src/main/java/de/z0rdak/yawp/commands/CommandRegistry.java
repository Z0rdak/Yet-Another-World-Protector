package de.z0rdak.yawp.commands;

import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.commands.arguments.ArgumentUtil;
import de.z0rdak.yawp.config.server.CommandPermissionConfig;
import de.z0rdak.yawp.util.CommandUtil;
import de.z0rdak.yawp.util.ChatComponentBuilder;
import net.minecraft.command.CommandRegistryAccess;
import net.minecraft.server.command.CommandManager;
import net.minecraft.server.command.ServerCommandSource;
import net.minecraft.text.ClickEvent;
import net.minecraft.text.MutableText;
import net.minecraft.text.Text;

import static de.z0rdak.yawp.util.ChatComponentBuilder.*;
import static de.z0rdak.yawp.util.MessageSender.sendCmdFeedback;

public final class CommandRegistry {

    private CommandRegistry() {
    }

    private static CommandDispatcher<ServerCommandSource> dispatcher;
    private static boolean isConfigInitialized = false;

    public static void registerCommands(CommandDispatcher<ServerCommandSource> commandDispatcher, CommandRegistryAccess cra, CommandManager.RegistrationEnvironment re) {
        dispatcher = commandDispatcher;
        if (re.dedicated || re.integrated) {
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
                .requires(CommandPermissionConfig::isAllowedForNonOp)
                .executes(ctx -> promptHelp(ctx.getSource()))
                .then(ArgumentUtil.literal(CommandConstants.HELP)
                        .executes(ctx -> promptHelp(ctx.getSource())))
                .then(FlagCommands.build())
                .then(MarkerCommands.build())
                .then(GlobalCommands.build())
                .then(DimensionCommands.build())
                .then(RegionCommands.build());
    }

    private static int promptHelp(ServerCommandSource src) {
        sendCmdFeedback(src, buildHeader(Text.translatableWithFallback("help.header", "== Yet Another World Protector - Help ==")));
        MutableText wikiHint = Text.translatableWithFallback("help.tooltip.wiki.detail", "The in-game help is under construction. Visit the online wiki for a guide on how to use the mod.");
        MutableText wikiText = Text.translatableWithFallback("help.tooltip.wiki", "Online-Wiki");
        sendCmdFeedback(src, wikiHint);
        sendCmdFeedback(src, wikiText.append(": ").append(buildWikiLink()));
        sendCmdFeedback(src, Text.literal(" => ").append(buildHelpStartComponent()));
        return 0;
    }
}
