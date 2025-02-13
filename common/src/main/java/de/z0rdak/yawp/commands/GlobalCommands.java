package de.z0rdak.yawp.commands;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import de.z0rdak.yawp.data.region.RegionDataManager;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.network.chat.Component;

import static de.z0rdak.yawp.api.commands.CommandConstants.*;
import static de.z0rdak.yawp.commands.arguments.ArgumentUtil.*;
import static de.z0rdak.yawp.util.ChatLinkBuilder.buildRegionInfoLink;
import static de.z0rdak.yawp.api.MessageSender.sendCmdFeedback;

public class GlobalCommands {

    private GlobalCommands() {
    }

    public static LiteralArgumentBuilder<CommandSourceStack> build() {
        return literal(GLOBAL)
                .executes(ctx -> CommandUtil.promptRegionInfo(ctx, getGlobalRegion()))
                .then(literal(INFO)
                        .executes(ctx -> CommandUtil.promptRegionInfo(ctx, getGlobalRegion())))
                .then(CommandUtil.buildClearSubCommand((ctx) -> getGlobalRegion()))
                .then(CommandUtil.buildListSubCommand((ctx) -> getGlobalRegion()))
                .then(CommandUtil.buildAddSubCommand((ctx) -> getGlobalRegion()))
                .then(CommandUtil.buildRemoveSubCommand((ctx) -> getGlobalRegion()))
                .then(literal(STATE)
                        .executes(ctx -> CommandUtil.promptRegionState(ctx, getGlobalRegion()))
                        .then(literal(ALERT)
                                .executes(ctx -> CommandUtil.setAlertState(ctx, getGlobalRegion(), getGlobalRegion().isMuted()))
                                .then(Commands.argument(ALERT.toString(), BoolArgumentType.bool())
                                        .executes(ctx -> CommandUtil.setAlertState(ctx, getGlobalRegion(), getAlertArgument(ctx))))
                        )
                        .then(literal(ENABLE)
                                .executes(ctx -> CommandUtil.setActiveState(ctx, getGlobalRegion(), !getGlobalRegion().isActive()))
                                .then(Commands.argument(ENABLE.toString(), BoolArgumentType.bool())
                                        .executes(ctx -> CommandUtil.setActiveState(ctx, getGlobalRegion(), getEnableArgument(ctx))))
                        )
                )
                .then(literal(RESET).executes(GlobalCommands::resetGlobalRegion));
    }

    public static int resetGlobalRegion(CommandContext<CommandSourceStack> ctx) {
        RegionDataManager.get().resetGlobalRegion();
        sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.global.reset", "Successfully reset global region", buildRegionInfoLink(getGlobalRegion())));
        return 0;
    }
}
