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
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.SharedSuggestionProvider;
import net.minecraft.commands.arguments.DimensionArgument;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.resources.ResourceLocation;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.commands.arguments.ArgumentUtil.*;
import static de.z0rdak.yawp.util.MessageUtil.*;

public final class FlagCommands {

    private FlagCommands() {
    }

    public static LiteralArgumentBuilder<CommandSourceStack> build() {
        return literal(FLAG)
                .then(literal(GLOBAL)
                        .then(flagSubCmd((ctx) -> getGlobalRegion())))
                .then(literal(DIM)
                        .then(flagDimSubCommands()))
                .then(literal(LOCAL)
                        .then(flagLocalSubCommands()));
    }

    public static RequiredArgumentBuilder<CommandSourceStack, ResourceLocation> flagDimSubCommands() {
        return Commands.argument(DIM.toString(), DimensionArgument.dimension())
                .then(flagSubCmd((ctx) -> getDimCacheArgument(ctx).getDimensionalRegion()));
    }

    public static RequiredArgumentBuilder<CommandSourceStack, ResourceLocation> flagLocalSubCommands() {
        return Commands.argument(DIM.toString(), DimensionArgument.dimension())
                .then(Commands.argument(CommandConstants.LOCAL.toString(), StringArgumentType.word())
                        .suggests((ctx, builder) -> RegionArgumentType.region().listSuggestions(ctx, builder))
                        .then(flagSubCmd(ArgumentUtil::getRegionArgument))
                );
    }

    private static RequiredArgumentBuilder<CommandSourceStack, String> flagSubCmd(Function<CommandContext<CommandSourceStack>, IProtectedRegion> regionSupplier) {
        return Commands.argument(FLAG.toString(), StringArgumentType.word())
                .suggests((ctx, builder) -> IFlagArgumentType.flag().listSuggestions(ctx, builder))
                .executes(ctx -> promptFlagInfo(ctx, regionSupplier.apply(ctx), getIFlagArgument(ctx)))
                .then(literal(INFO)
                        .executes(ctx -> promptFlagInfo(ctx, regionSupplier.apply(ctx), getIFlagArgument(ctx)))
                )
                .then(literal(STATE)
                        .executes(ctx -> setFlagState(ctx, regionSupplier.apply(ctx), getIFlagArgument(ctx)))
                        .then(Commands.argument(STATE.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(FlagState.ValidFlagStates(), builder))
                                .executes(ctx -> setFlagState(ctx, regionSupplier.apply(ctx), getIFlagArgument(ctx), getFlagStateArgument(ctx))))
                )
                .then(literal(OVERRIDE)
                        .executes(ctx -> setOverride(ctx, regionSupplier.apply(ctx), getIFlagArgument(ctx)))
                        .then(Commands.argument(OVERRIDE.toString(), BoolArgumentType.bool())
                                .executes(ctx -> setOverride(ctx, regionSupplier.apply(ctx), getIFlagArgument(ctx), getOverrideArgument(ctx))))
                )
                .then(literal(MSG)
                        .then(literal(MUTE)
                                .executes(ctx -> setFlagMuteState(ctx, regionSupplier.apply(ctx), getIFlagArgument(ctx)))
                                .then(Commands.argument(MUTE.toString(), BoolArgumentType.bool())
                                        .executes(ctx -> setFlagMuteState(ctx, regionSupplier.apply(ctx), getIFlagArgument(ctx), getMuteArgument(ctx))))
                        )
                        .then(literal(SET)
                                .then(Commands.argument(MSG.toString(), StringArgumentType.string())
                                        .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(flagMsgExamples(), builder))
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
            examples.add(new TranslatableComponent("cli.flag.msg.text.example." + i).getString());
        }
        return examples;
    }

    /**
     * Builds the flag info component for the given flag and region. <br></br>
     * == Flag info for [flagname] of [region] == <br></br>
     * State: state [set state] <br></br>
     * Muted: [no] <br></br>
     * Override: [false] <br></br>
     * Message: [set] [x]: 'msg' <br></br>
     */
    private static int promptFlagInfo(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, IFlag flag) {
        sendCmdFeedback(ctx.getSource(), buildFlagInfoHeader(region, flag));
        sendCmdFeedback(ctx.getSource(), buildInfoComponent("cli.flag.state", buildFlagStateComponent(region, flag)));
        MutableComponent overrideComponent = buildFlagOverrideInfoComponent(flag)
                .append(" ")
                .append(buildFlagOverrideToggleLink(region, flag));
        sendCmdFeedback(ctx.getSource(), buildInfoComponent("cli.flag.override", overrideComponent));
        if (RegionFlag.hasPlayerCategory(flag)) {
            sendCmdFeedback(ctx.getSource(), buildInfoComponent("cli.flag.msg.mute", buildFlagMuteToggleLink(region, flag)));
            sendCmdFeedback(ctx.getSource(), buildInfoComponent("cli.flag.msg.text", buildFlagMessageComponent(region, flag)));
        }
        return 0;
    }

    private static int setFlagMuteState(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, IFlag regionFlag) {
        if (region.containsFlag(regionFlag.getName())) {
            IFlag flag = region.getFlag(regionFlag.getName());
            return setFlagMuteState(ctx, region, flag, !flag.getFlagMsg().isMuted());
        } else {
            MessageUtil.sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.info.region.flag.not-present",
                    buildRegionInfoLink(region), regionFlag.getName()));
            return 1;
        }
    }

    private static int setFlagMuteState(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, IFlag flag, boolean setMuted) {
        flag.getFlagMsg().mute(setMuted);
        MutableComponent undoLink = buildRegionActionUndoLink(ctx.getInput(), String.valueOf(!setMuted), String.valueOf(setMuted));
        MutableComponent msg = new TranslatableComponent("cli.flag.msg.mute.success.text",
                buildFlagInfoLink(region, flag), flag.getFlagMsg().isMuted())
                .append(" ")
                .append(undoLink);
        MessageUtil.sendCmdFeedback(ctx.getSource(), msg);
        RegionDataManager.save();
        return 0;

    }

    private static int setRegionFlagMsg(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, IFlag flag, String flagMsgStr) {
        String oldFlagMsg = flag.getFlagMsg().getMsg();
        FlagMessage flagMsg = new FlagMessage(flagMsgStr, flag.getFlagMsg().isMuted());
        flag.setFlagMsg(flagMsg);
        MutableComponent undoLink = buildRegionActionUndoLink(ctx.getInput(), flagMsgStr, oldFlagMsg);
        MutableComponent msg = new TranslatableComponent("cli.flag.msg.msg.success.text",
                buildFlagInfoLink(region, flag), flagMsgStr)
                .append(" ")
                .append(undoLink);
        MessageUtil.sendCmdFeedback(ctx.getSource(), msg);
        RegionDataManager.save();
        return 0;
    }

    private static int setFlagState(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, IFlag regionFlag) {
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
            MessageUtil.sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.info.region.flag.not-present",
                    buildRegionInfoLink(region), regionFlag.getName()));
            return 1;
        }
    }

    private static int setFlagState(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, IFlag flag, FlagState flagState) {
        FlagState oldState = flag.getState();
        flag.setState(flagState);
        MutableComponent undoLink = buildRegionActionUndoLink(ctx.getInput(), oldState.name(), flagState.name());
        MutableComponent msg = new TranslatableComponent("cli.flag.enable.success.text",
                buildFlagInfoLink(region, flag), flag.isActive())
                .append(" ")
                .append(undoLink);
        MessageUtil.sendCmdFeedback(ctx.getSource(), msg);
        RegionDataManager.save();
        return 0;

    }

    public static int setOverride(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, IFlag regionFlag) {
        if (region.containsFlag(regionFlag.getName())) {
            IFlag flag = region.getFlag(regionFlag.getName());
            return setOverride(ctx, region, flag, !flag.doesOverride());
        } else {
            MessageUtil.sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.info.region.flag.not-present",
                    buildRegionInfoLink(region), regionFlag.getName()));
            return 1;
        }
    }

    public static int setOverride(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, IFlag flag, boolean override) {
        flag.setOverride(override);
        MutableComponent undoLink = buildRegionActionUndoLink(ctx.getInput(), String.valueOf(!override), String.valueOf(override));
        MutableComponent msg = new TranslatableComponent("cli.flag.override.success.text",
                buildFlagInfoLink(region, flag), flag.doesOverride())
                .append(" ")
                .append(undoLink);
        MessageUtil.sendCmdFeedback(ctx.getSource(), msg);
        RegionDataManager.save();
        return 0;
    }

}
