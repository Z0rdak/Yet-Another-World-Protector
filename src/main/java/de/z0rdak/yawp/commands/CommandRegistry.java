package de.z0rdak.yawp.commands;

import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.config.server.CommandPermissionConfig;
import de.z0rdak.yawp.util.CommandUtil;
import net.minecraft.command.CommandSource;
import net.minecraft.command.Commands;
import net.minecraft.util.text.IFormattableTextComponent;
import net.minecraft.util.text.StringTextComponent;
import net.minecraft.util.text.TranslationTextComponent;
import net.minecraft.util.text.event.ClickEvent;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.event.RegisterCommandsEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;

import static de.z0rdak.yawp.util.MessageUtil.*;
import static net.minecraft.util.text.TextFormatting.AQUA;
import static net.minecraft.util.text.TextFormatting.GREEN;
import static net.minecraftforge.fml.common.Mod.EventBusSubscriber.Bus.FORGE;

@Mod.EventBusSubscriber(modid = YetAnotherWorldProtector.MODID, value = Dist.DEDICATED_SERVER, bus = FORGE)
public final class CommandRegistry {

    private CommandRegistry() {
    }

    private static CommandDispatcher<CommandSource> dispatcher;
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

    private static LiteralArgumentBuilder<CommandSource> buildCommands(String baseCmd) {
        return Commands.literal(baseCmd)
                .executes(ctx -> promptHelp(ctx.getSource()))
                .then(CommandUtil.literal(CommandConstants.HELP)
                        .executes(ctx -> promptHelp(ctx.getSource())))
                .then(DimensionCommands.build())
                .then(FlagCommands.build())
                .then(MarkerCommands.build())
                .then(RegionCommands.build());
    }

    private static int promptHelp(CommandSource src) {
        sendCmdFeedback(src, buildHeader("cli.msg.help.header"));
        String command = CommandUtil.buildCommandStr(CommandConstants.DIM.toString());
        IFormattableTextComponent cmdStr = new TranslationTextComponent("cli.msg.help.1", CommandPermissionConfig.BASE_CMD);
        sendCmdFeedback(src, buildExecuteCmdComponent(
                new StringTextComponent("=> "),
                new TranslationTextComponent("help.tooltip.dim"),
                command, ClickEvent.Action.SUGGEST_COMMAND, GREEN).append(cmdStr));
        IFormattableTextComponent wikiText1 = new TranslationTextComponent("help.tooltip.info.wiki.1");
        IFormattableTextComponent wikiText2 = new TranslationTextComponent("help.tooltip.info.wiki.2");
        IFormattableTextComponent wikiText3 = new TranslationTextComponent("help.tooltip.info.wiki.3");
        IFormattableTextComponent wikiLinkHover = new TranslationTextComponent("help.tooltip.info.wiki.link.hover");
        IFormattableTextComponent wikiLink = new TranslationTextComponent("help.tooltip.info.wiki.link.text");
        IFormattableTextComponent wikiCopyToClipboardLink = buildExecuteCmdComponent(wikiLink, wikiLinkHover, "", ClickEvent.Action.OPEN_URL, AQUA);
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
