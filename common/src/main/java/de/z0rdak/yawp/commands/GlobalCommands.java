package de.z0rdak.yawp.commands;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import de.z0rdak.yawp.api.core.RegionManager;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.arguments.DimensionArgument;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerLevel;

import static de.z0rdak.yawp.api.MessageSender.sendCmdFeedback;
import static de.z0rdak.yawp.api.commands.CommandConstants.*;
import static de.z0rdak.yawp.commands.arguments.ArgumentUtil.*;
import static de.z0rdak.yawp.util.ChatLinkBuilder.buildRegionInfoLink;

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
                .then(literal(RESET).executes(GlobalCommands::resetGlobalRegion))
                // TODO: Only suggest levels which are not tracked yet
                .then(literal(TRACK)
                        .then(Commands.argument(DIM.toString(), DimensionArgument.dimension())
                        .executes(ctx -> trackLevel(ctx, DimensionArgument.getDimension(ctx, DIM.toString()))))
                )
                // TODO: Only suggest levels which are already tracked
                // .then(literal(UNTRACK)
                //         .then(Commands.argument(DIM.toString(), DimensionArgument.dimension())
                //                 .executes(ctx -> untrackLevel(ctx, DimensionArgument.getDimension(ctx, DIM.toString()))))
                // )
        ;
    }

    private static int untrackLevel(CommandContext<CommandSourceStack> ctx, ServerLevel level) {
        var maybeLrd = RegionManager.get().getLevelRegionData(level.dimension());
        if (!maybeLrd.isPresent()) {
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.global.level-not-tracked", "The level '%s' is currently not tracked by YAWP.", level.dimension().identifier().toString()));
            return 1;
        }
        RegionManager.get().untrackLevel(level.dimension());
        sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.global.level.untracked", "The level '%s' is no longer tracked. Its regions are disabled from now on.", level.dimension().identifier().toString()));
        return 0;
    }

    private static int trackLevel(CommandContext<CommandSourceStack> ctx, ServerLevel level) {
        var maybeLrd = RegionManager.get().getLevelRegionData(level.dimension());
        if (maybeLrd.isPresent()) {
            var lrd = maybeLrd.get();
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.global.level.already-tracked", "The level '%s' is already tracked.", buildRegionInfoLink(lrd.getDim())));
            return 1;
        }
        var levelRegionData = RegionManager.get().trackLevel(level.dimension());
        sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.global.level.tracked", "The level '%s' is now tracked and available to create regions.", buildRegionInfoLink(levelRegionData.getDim())));
        return 0;
    }

    public static int resetGlobalRegion(CommandContext<CommandSourceStack> ctx) {
        RegionManager.get().resetGlobal();
        sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.global.reset", "Successfully reset global region", buildRegionInfoLink(getGlobalRegion())));
        return 0;
    }
}
