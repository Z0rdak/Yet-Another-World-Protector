package de.z0rdak.regionshield.commands;

import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.regionshield.RegionShield;
import de.z0rdak.regionshield.config.ServerConfigBuilder;
import de.z0rdak.regionshield.util.CommandUtil;
import de.z0rdak.regionshield.util.MessageUtil;
import de.z0rdak.regionshield.util.PlayerUtils;
import net.minecraft.command.CommandSource;
import net.minecraft.command.Commands;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.util.text.StringTextComponent;
import net.minecraft.util.text.TextFormatting;
import net.minecraft.util.text.TranslationTextComponent;
import net.minecraft.util.text.event.ClickEvent;

import static de.z0rdak.regionshield.util.CommandUtil.helpLiteral;
import static de.z0rdak.regionshield.util.MessageUtil.buildExecuteCmdComponent;
import static de.z0rdak.regionshield.util.MessageUtil.buildHelpHeader;

public class CommandRegionShield {

    private CommandRegionShield() {
    }

    public static void init(CommandDispatcher<CommandSource> commandDispatcher) {
        commandDispatcher.register(register());
    }

    public static LiteralArgumentBuilder<CommandSource> register() {
        return withSubCommands(Commands.literal(CommandConstants.BASE_CMD.toString()));
    }

    private static LiteralArgumentBuilder<CommandSource> withSubCommands(LiteralArgumentBuilder<CommandSource> baseCommand) {
        return baseCommand
                .requires(source -> source.hasPermission(ServerConfigBuilder.RS_CMD_OP_LEVEL.get()))
                .executes(ctx -> promptHelp(ctx.getSource()))
                .then(helpLiteral
                        .executes(ctx -> promptHelp(ctx.getSource())))
                .then(DimensionCommands.DIMENSION_COMMAND)
                //.then(RegionCommands.REGION_COMMAND)
                //.then(RegionCommands.REGIONS_COMMAND)
                //.then(DimensionFlagCommands.DIMENSION_FLAGS_COMMAND);
                //.then(CommandExpand.EXPAND_COMMAND)
                //.then(CommandFlag.FLAG_COMMAND)
        //.then(CommandPlayer.PLAYER_COMMAND);
        ;
    }

    private static boolean hasPermission(CommandSource source) {
        try {
            PlayerEntity player = source.getPlayerOrException();
            return PlayerUtils.hasPermission(player) || source.hasPermission(ServerConfigBuilder.RS_CMD_OP_LEVEL.get());
        } catch (CommandSyntaxException e) {
            e.printStackTrace();
        }
        return false;
    }

    private static int promptHelp(CommandSource src) {
        MessageUtil.sendCmdFeedback(src, buildHelpHeader("cli.msg.help.header"));
        MessageUtil.sendCmdFeedback(src, buildExecuteCmdComponent(" => ", CommandUtil.buildCommandStr(CommandConstants.DIMENSION.toString(), " "), TextFormatting.GREEN, "Manage dimensional regions", ClickEvent.Action.SUGGEST_COMMAND).append(new TranslationTextComponent("cli.msg.help.1")));
        String wikiLink = "https://github.com/Z0rdak/RegionShield/wiki";
        StringTextComponent wikiInfo = new StringTextComponent("The in-game help is under construction.\nVisit the online wiki for more guide on how to use the mod.\nOnline-Wiki: ");
        MessageUtil.sendCmdFeedback(src, wikiInfo.append(buildExecuteCmdComponent(RegionShield.MODID_LONG + " online wiki", wikiLink,
                TextFormatting.AQUA, "Open online wiki in your browser", ClickEvent.Action.OPEN_URL)));
        return 0;
    }
}
