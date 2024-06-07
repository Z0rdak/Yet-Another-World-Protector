package de.z0rdak.yawp.commands;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.builder.RequiredArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import de.z0rdak.yawp.commands.arguments.ArgumentUtil;
import de.z0rdak.yawp.commands.arguments.flag.IFlagArgumentType;
import de.z0rdak.yawp.commands.arguments.region.RegionArgumentType;
import de.z0rdak.yawp.core.flag.FlagMessage;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.command.CommandSource;
import net.minecraft.command.argument.DimensionArgumentType;
import net.minecraft.server.command.CommandManager;
import net.minecraft.server.command.ServerCommandSource;
import net.minecraft.text.MutableText;
import net.minecraft.util.Identifier;
import net.minecraft.text.Text;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.commands.arguments.ArgumentUtil.*;
import static de.z0rdak.yawp.util.ChatComponentBuilder.*;
import static de.z0rdak.yawp.util.MessageSender.sendCmdFeedback;

public final class FlagCommands {

    private FlagCommands() {
    }

    public static LiteralArgumentBuilder<ServerCommandSource> build() {
        return literal(FLAG)
                .then(literal(GLOBAL)
                        .executes(ctx -> CommandUtil.promptRegionFlagList(ctx, getGlobalRegion(), 0))
                        .then(flagSubCmd((ctx) -> getGlobalRegion())))
                .then(literal(DIM)
                        .then(flagDimSubCommands()))
                .then(literal(LOCAL)
                        .then(flagLocalSubCommands()));
    }

    public static RequiredArgumentBuilder<ServerCommandSource, Identifier> flagDimSubCommands() {
        return CommandManager.argument(DIM.toString(), DimensionArgumentType.dimension())
                .executes(ctx -> CommandUtil.promptRegionFlagList(ctx, getDimCacheArgument(ctx).getDimensionalRegion(), 0))
                .then(flagSubCmd((ctx) -> getDimCacheArgument(ctx).getDimensionalRegion()));
    }

    public static RequiredArgumentBuilder<ServerCommandSource, Identifier> flagLocalSubCommands() {
        return CommandManager.argument(DIM.toString(), DimensionArgumentType.dimension())
                .then(CommandManager.argument(CommandConstants.LOCAL.toString(), StringArgumentType.word())
                        .suggests((ctx, builder) -> RegionArgumentType.region().listSuggestions(ctx, builder))
                        .executes(ctx -> CommandUtil.promptRegionFlagList(ctx, getDimCacheArgument(ctx).getDimensionalRegion(), 0))
                        .then(flagSubCmd(ArgumentUtil::getRegionArgument))
                );
    }

    private static RequiredArgumentBuilder<ServerCommandSource, String> flagSubCmd(Function<CommandContext<ServerCommandSource>, IProtectedRegion> regionSupplier) {
        return CommandManager.argument(FLAG.toString(), StringArgumentType.word())
                .suggests((ctx, builder) -> IFlagArgumentType.flag().listSuggestions(ctx, builder))
                .executes(ctx -> promptFlagInfo(ctx, regionSupplier.apply(ctx), getIFlagArgument(ctx)))
                .then(literal(INFO)
                        .executes(ctx -> promptFlagInfo(ctx, regionSupplier.apply(ctx), getIFlagArgument(ctx)))
                )
                .then(literal(STATE)
                        .executes(ctx -> setFlagState(ctx, regionSupplier.apply(ctx), getIFlagArgument(ctx)))
                        .then(CommandManager.argument(STATE.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> CommandSource.suggestMatching(FlagState.ValidFlagStates(), builder))
                                .executes(ctx -> setFlagState(ctx, regionSupplier.apply(ctx), getIFlagArgument(ctx), getFlagStateArgument(ctx))))
                )
                .then(literal(OVERRIDE)
                        .executes(ctx -> setOverride(ctx, regionSupplier.apply(ctx), getIFlagArgument(ctx)))
                        .then(CommandManager.argument(OVERRIDE.toString(), BoolArgumentType.bool())
                                .executes(ctx -> setOverride(ctx, regionSupplier.apply(ctx), getIFlagArgument(ctx), getOverrideArgument(ctx))))
                )
                .then(literal(MSG)
                        .then(literal(MUTE)
                                .executes(ctx -> setFlagMuteState(ctx, regionSupplier.apply(ctx), getIFlagArgument(ctx)))
                                .then(CommandManager.argument(MUTE.toString(), BoolArgumentType.bool())
                                        .executes(ctx -> setFlagMuteState(ctx, regionSupplier.apply(ctx), getIFlagArgument(ctx), getMuteArgument(ctx))))
                        )
                        .then(literal(SET)
                                .then(CommandManager.argument(MSG.toString(), StringArgumentType.string())
                                        .suggests((ctx, builder) -> CommandSource.suggestMatching(flagMsgExamples(), builder))
                                        .executes(ctx -> setRegionFlagMsg(ctx, regionSupplier.apply(ctx), getIFlagArgument(ctx), getFlagMsgArgument(ctx))))
                        )
                        .then(literal(CLEAR)
                                .executes(ctx -> setRegionFlagMsg(ctx, regionSupplier.apply(ctx), getIFlagArgument(ctx), FlagMessage.CONFIG_MSG))
                        )
                );
    }

    private static List<String> flagMsgExamples() {
        final int amountOfExamples = 10;
        List<String> examples = new ArrayList<>(amountOfExamples);
        for (int i = 0; i < amountOfExamples; i++) {
            examples.add(Text.translatableWithFallback("cli.flag.msg.text.example." + i, "<Your flag message here>").getString());
        }
        return examples;
    }

    /**
     * Builds the flag info component for the given flag and region. <br></br>
     * == Flag info for [flagname] of [region] == <br></br>
     * State: state [set state] <br></br>
     * Alert: [active] <br></br>
     * Override: [false] <br></br>
     * Message: [set] [x]: 'msg' <br></br>
     */
    private static int promptFlagInfo(CommandContext<ServerCommandSource> ctx, IProtectedRegion region, IFlag flag) {
        sendCmdFeedback(ctx.getSource(), buildFlagInfoHeader(region, flag));
        sendCmdFeedback(ctx.getSource(), buildInfoComponent("cli.flag.state", "State", buildFlagStateComponent(region, flag)));
        sendCmdFeedback(ctx.getSource(), buildInfoComponent("cli.flag.override", "Override", buildFlagOverrideToggleLink(region, flag, false)));
        if (RegionFlag.hasPlayerCategory(flag)) {
            sendCmdFeedback(ctx.getSource(), buildInfoComponent("cli.flag.msg.mute", "Alert", buildFlagMuteToggleLink(region, flag, false)));
            sendCmdFeedback(ctx.getSource(), buildInfoComponent("cli.flag.msg.text", "Message", buildFlagMessageComponent(region, flag)));
        }
        return 0;
    }

    private static int setFlagMuteState(CommandContext<ServerCommandSource> ctx, IProtectedRegion region, IFlag regionFlag) {
        if (region.containsFlag(regionFlag.getName())) {
            IFlag flag = region.getFlag(regionFlag.getName());
            return setFlagMuteState(ctx, region, flag, !flag.getFlagMsg().isMuted());
        } else {
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.info.region.flag.not-present", "Region '%s' does not contain flag '%s'",
                    buildRegionInfoLink(region), regionFlag.getName()));
            return 1;
        }
    }

    private static int setFlagMuteState(CommandContext<ServerCommandSource> ctx, IProtectedRegion region, IFlag flag, boolean setMuted) {
        flag.getFlagMsg().mute(setMuted);
        MutableText undoLink = buildRegionActionUndoLink(ctx.getInput(), String.valueOf(!setMuted), String.valueOf(setMuted));
        MutableText msg = Text.translatableWithFallback("cli.flag.msg.mute.success.text", "Set mute state of %s to: '%s'",
                        buildFlagInfoLink(region, flag), flag.getFlagMsg().isMuted())
                .append(" ")
                .append(undoLink);
        sendCmdFeedback(ctx.getSource(), msg);
        RegionDataManager.save();
        return 0;

    }

    private static int setRegionFlagMsg(CommandContext<ServerCommandSource> ctx, IProtectedRegion region, IFlag flag, String flagMsgStr) {
        String oldFlagMsg = flag.getFlagMsg().getMsg();
        FlagMessage flagMsg = new FlagMessage(flagMsgStr, flag.getFlagMsg().isMuted());
        flag.setFlagMsg(flagMsg);
        MutableText undoLink = buildRegionActionUndoLink(ctx.getInput(), flagMsgStr, oldFlagMsg);
        MutableText msg = Text.translatableWithFallback("cli.flag.msg.msg.success.text", "Set message of %s to: '%s'",
                        buildFlagInfoLink(region, flag), flagMsgStr)
                .append(" ")
                .append(undoLink);
        sendCmdFeedback(ctx.getSource(), msg);
        RegionDataManager.save();
        return 0;
    }

    private static int setFlagState(CommandContext<ServerCommandSource> ctx, IProtectedRegion region, IFlag regionFlag) {
        if (region.containsFlag(regionFlag.getName())) {
            IFlag flag = region.getFlag(regionFlag.getName());
            if (flag.getState() == FlagState.ALLOWED || flag.getState() == FlagState.DENIED) {
                return setFlagState(ctx, region, regionFlag, FlagState.invert(flag.getState()));
            }
            if (flag.getState() == FlagState.DISABLED) {
                return setFlagState(ctx, region, regionFlag, FlagState.DENIED);
            }
            return setFlagState(ctx, region, regionFlag, flag.getState());
        } else {
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.info.region.flag.not-present", "Region '%s' does not contain flag '%s'",
                    buildRegionInfoLink(region), regionFlag.getName()));
            return 1;
        }
    }

    private static int setFlagState(CommandContext<ServerCommandSource> ctx, IProtectedRegion region, IFlag flag, FlagState flagState) {
        FlagState oldState = flag.getState();
        flag.setState(flagState);
        MutableText undoLink = buildRegionActionUndoLink(ctx.getInput(), oldState.name(), flagState.name());
        MutableText msg = Text.translatableWithFallback("cli.flag.state.success.text", "Set flag state of %s to: '%s'",
                        buildFlagInfoLink(region, flag), flag.isActive())
                .append(" ")
                .append(undoLink);
        sendCmdFeedback(ctx.getSource(), msg);
        RegionDataManager.save();
        return 0;

    }

    public static int setOverride(CommandContext<ServerCommandSource> ctx, IProtectedRegion region, IFlag regionFlag) {
        if (region.containsFlag(regionFlag.getName())) {
            IFlag flag = region.getFlag(regionFlag.getName());
            return setOverride(ctx, region, flag, !flag.doesOverride());
        } else {
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.info.region.flag.not-present", "Region '%s' does not contain flag '%s'",
                    buildRegionInfoLink(region), regionFlag.getName()));
            return 1;
        }
    }

    public static int setOverride(CommandContext<ServerCommandSource> ctx, IProtectedRegion region, IFlag flag, boolean override) {
        flag.setOverride(override);
        MutableText undoLink = buildRegionActionUndoLink(ctx.getInput(), String.valueOf(!override), String.valueOf(override));
        MutableText msg = Text.translatableWithFallback("cli.flag.override.success.text", "Set flag override for %s to %s",
                        buildFlagInfoLink(region, flag), flag.doesOverride())
                .append(" ")
                .append(undoLink);
        sendCmdFeedback(ctx.getSource(), msg);
        RegionDataManager.save();
        return 0;
    }

}
