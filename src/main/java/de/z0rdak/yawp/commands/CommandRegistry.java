package de.z0rdak.yawp.commands;

import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.config.server.CommandPermissionConfig;
import de.z0rdak.yawp.util.CommandUtil;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.ChatFormatting;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.arguments.DimensionArgument;
import net.minecraft.network.chat.ClickEvent;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.network.chat.TranslatableComponent;

import static de.z0rdak.yawp.util.CommandUtil.getDimRegionArgument;
import static de.z0rdak.yawp.util.CommandUtil.literal;
import static de.z0rdak.yawp.util.MessageUtil.buildExecuteCmdComponent;
import static de.z0rdak.yawp.util.MessageUtil.buildHelpHeader;

public class CommandRegistry {

    private CommandRegistry() {
    }

    public static void init(CommandDispatcher<CommandSourceStack> commandDispatcher) {
        commandDispatcher.register(register());
    }

    public static LiteralArgumentBuilder<CommandSourceStack> register() {
        return withSubCommands(Commands.literal(CommandPermissionConfig.BASE_CMD));
    }

    private static LiteralArgumentBuilder<CommandSourceStack> withSubCommands(LiteralArgumentBuilder<CommandSourceStack> baseCommand) {
        return baseCommand
                //.requires(CommandPermissionConfig::hasPermission)
                .executes(ctx -> promptHelp(ctx.getSource()))
                .then(CommandUtil.literal(CommandConstants.HELP)
                        .executes(ctx -> promptHelp(ctx.getSource())))
                //.then(literal(CommandConstants.SELECT)
                //        .then(Commands.argument(CommandConstants.DIMENSION.toString(), DimensionArgument.dimension())
                //                .executes(ctx -> DimensionCommands.selectReferenceDim(ctx.getSource(), getDimRegionArgument(ctx)))))
                .then(DimensionCommands.DIMENSION_COMMAND)
                .then(RegionCommands.REGION_COMMAND)
                //.then(FlagCommands.FLAG_COMMAND)
                //.then(RegionCommands.REGIONS_COMMAND)
                //.then(DimensionFlagCommands.DIMENSION_FLAGS_COMMAND);
                //.then(CommandExpand.EXPAND_COMMAND)
                //.then(CommandPlayer.PLAYER_COMMAND);
                ;
    }

    private static int promptHelp(CommandSourceStack src) {
        MessageUtil.sendCmdFeedback(src, buildHelpHeader("cli.msg.help.header"));
        String command = CommandUtil.buildCommandStr(CommandConstants.DIMENSION.toString());
        TranslatableComponent cmdStr = new TranslatableComponent("cli.msg.help.1", CommandPermissionConfig.BASE_CMD);
        MessageUtil.sendCmdFeedback(src, buildExecuteCmdComponent("=>", "Manage dimensional regions", command, ClickEvent.Action.SUGGEST_COMMAND, ChatFormatting.GREEN).append(cmdStr));
        String wikiLink = "https://github.com/Z0rdak/Yet-Another-World-Protector";
        TextComponent wikiInfo = new TextComponent("The in-game help is under construction.\nVisit the online wiki for a guide on how to use the mod.\nOnline-Wiki: ");
        MessageUtil.sendCmdFeedback(src, wikiInfo.append(buildExecuteCmdComponent(YetAnotherWorldProtector.MODID_LONG + " online wiki", "Open online wiki in your browser", wikiLink,
                ClickEvent.Action.OPEN_URL, ChatFormatting.AQUA)));
        return 0;
    }
}
