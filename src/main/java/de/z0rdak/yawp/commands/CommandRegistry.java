package de.z0rdak.yawp.commands;

import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.commands.arguments.ArgumentUtil;
import de.z0rdak.yawp.config.server.CommandPermissionConfig;
import de.z0rdak.yawp.handler.CommandInterceptor;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.network.chat.ClickEvent;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraftforge.event.RegisterCommandsEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;

import static net.minecraft.ChatFormatting.AQUA;
import static net.minecraft.ChatFormatting.GREEN;
import static net.minecraftforge.fml.common.Mod.EventBusSubscriber.Bus.FORGE;

@Mod.EventBusSubscriber(modid = YetAnotherWorldProtector.MODID, bus = FORGE)
public final class CommandRegistry {

    private CommandRegistry() {
    }

    private static CommandDispatcher<CommandSourceStack> dispatcher;
    private static boolean isConfigInitialized = false;

    @SubscribeEvent
    public static void registerCommands(RegisterCommandsEvent event) {
        dispatcher = event.getDispatcher();
        if (isConfigInitialized) {
            CommandRegistry.register(CommandPermissionConfig.BASE_CMD);
        }
    }

    public static void register(String modRootCmd) {
        dispatcher.register(buildCommands(modRootCmd));
        CommandRegistry.isConfigInitialized = true;
    }

    private static LiteralArgumentBuilder<CommandSourceStack> buildCommands(String baseCmd) {
        return Commands.literal(baseCmd)
                .requires(CommandInterceptor::isAllowedForNonOp)
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
        sendCmdFeedback(src, buildHeader("help.header"));
        MutableComponent wikiHint = new TranslatableComponent("help.tooltip.wiki.detail");
        MutableComponent wikiText = new TranslatableComponent("help.tooltip.wiki");
        sendCmdFeedback(src, wikiHint);
        sendCmdFeedback(src, wikiText.append(": ").append(buildWikiLink()));
        sendCmdFeedback(src, new TextComponent(" => ").append(buildHelpStartComponent()));
        return 0;
    }
}
