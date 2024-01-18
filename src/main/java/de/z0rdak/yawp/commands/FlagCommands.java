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
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.core.region.RegionType;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.command.CommandSource;
import net.minecraft.command.Commands;
import net.minecraft.command.ISuggestionProvider;
import net.minecraft.command.arguments.DimensionArgument;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.text.IFormattableTextComponent;
import net.minecraft.util.text.TranslationTextComponent;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.commands.arguments.ArgumentUtil.*;
import static de.z0rdak.yawp.util.MessageUtil.*;

public final class FlagCommands {

    private FlagCommands() {
    }

    public static LiteralArgumentBuilder<CommandSource> build() {

        return literal(FLAG)
                .then(literal(GLOBAL)
                        .then(flagSubCmd((ctx) -> getGlobalRegion())))
                .then(literal(DIM)
                        .then(flagDimSubCommands()))
                .then(literal(LOCAL)
                        .then(flagLocalSubCommands()));
    }

    public static RequiredArgumentBuilder<CommandSource, ResourceLocation> flagDimSubCommands() {
        return Commands.argument(DIM.toString(), DimensionArgument.dimension())
                .then(flagSubCmd((ctx) -> getDimCacheArgument(ctx).getDimensionalRegion()));
    }

    public static RequiredArgumentBuilder<CommandSource, ResourceLocation> flagLocalSubCommands() {
        return Commands.argument(DIM.toString(), DimensionArgument.dimension())
                .then(Commands.argument(CommandConstants.LOCAL.toString(), StringArgumentType.word())
                        .suggests((ctx, builder) -> RegionArgumentType.region().listSuggestions(ctx, builder))
                        .then(flagSubCmd(ArgumentUtil::getRegionArgument))
                );
    }

    private static RequiredArgumentBuilder<CommandSource, String> flagSubCmd(Function<CommandContext<CommandSource>, IProtectedRegion> regionSupplier) {
        return Commands.argument(FLAG.toString(), StringArgumentType.word())
                .suggests((ctx, builder) -> IFlagArgumentType.flag().listSuggestions(ctx, builder))
                .executes(ctx -> promptFlagInfo(ctx, regionSupplier.apply(ctx), getIFlagArgument(ctx)))
                .then(literal(INFO)
                        .executes(ctx -> promptFlagInfo(ctx, regionSupplier.apply(ctx), getIFlagArgument(ctx)))
                )
                .then(literal(ENABLE)
                        .executes(ctx -> setEnableState(ctx, regionSupplier.apply(ctx), getIFlagArgument(ctx)))
                        .then(Commands.argument(ENABLE.toString(), BoolArgumentType.bool())
                                .executes(ctx -> setEnableState(ctx, regionSupplier.apply(ctx), getIFlagArgument(ctx), getEnableArgument(ctx))))
                )
                .then(literal(OVERRIDE)
                        .executes(ctx -> setInvertState(ctx, regionSupplier.apply(ctx), getIFlagArgument(ctx)))
                        .then(Commands.argument(OVERRIDE.toString(), BoolArgumentType.bool())
                                .executes(ctx -> setInvertState(ctx, regionSupplier.apply(ctx), getIFlagArgument(ctx), getNegationArgument(ctx))))
                )
                .then(literal(MSG)
                        .then(literal(MUTE)
                                .executes(ctx -> setFlagMuteState(ctx, regionSupplier.apply(ctx), getIFlagArgument(ctx)))
                                .then(Commands.argument(MUTE.toString(), BoolArgumentType.bool())
                                        .executes(ctx -> setFlagMuteState(ctx, regionSupplier.apply(ctx), getIFlagArgument(ctx), getMuteArgument(ctx))))
                        )
                        .then(literal(SET)
                                .then(Commands.argument(MSG.toString(), StringArgumentType.string())
                                        .suggests((ctx, builder) -> ISuggestionProvider.suggest(flagMsgExamples(), builder))
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
            examples.add(new TranslationTextComponent("cli.info.flag.state.msg.text.example." + i).getString());
        }
        return examples;
    }

    /**
     * Builds the flag info component for the given flag and region. <br></br>
     * == Flag info for [flagname] of [region] == <br></br>
     * Enabled: [yes] <br></br>
     * Inverted: [no] <br></br>
     * Muted: [no] <br></br>
     * Msg [set] [x]: 'msg' <br></br>
     */
    private static int promptFlagInfo(CommandContext<CommandSource> ctx, IProtectedRegion region, IFlag flag) {
        sendCmdFeedback(ctx.getSource(), buildFlagInfoHeader(region, flag));
        sendCmdFeedback(ctx.getSource(), buildInfoComponent("cli.info.flag.state.enable", buildFlagActiveToggleLink(region, flag)));
        sendCmdFeedback(ctx.getSource(), buildInfoComponent("cli.info.flag.state.override", buildFlagInvertToggleLink(region, flag)));
        sendCmdFeedback(ctx.getSource(), buildInfoComponent("cli.info.flag.state.msg.mute", buildFlagMuteToggleLink(region, flag)));
        sendCmdFeedback(ctx.getSource(), buildInfoComponent("cli.info.flag.state.msg.text", buildFlagMessageEditLink(region, flag)));
        return 0;
    }

    private static int promptFlagInfo(CommandContext<CommandSource> ctx, IProtectedRegion region, RegionType regionType, IFlag regionFlag) {
        if (region.containsFlag(regionFlag.getName())) {
            IFlag flag = region.getFlag(regionFlag.getName());
            promptFlagInfo(ctx, region, flag);
            return 0;
        } else {
            MessageUtil.sendCmdFeedback(ctx.getSource(), new TranslationTextComponent("cli.msg.info.region.flag.not-present",
                    buildRegionInfoLink(region), regionFlag));
            return 1;
        }
    }

    private static int setFlagMuteState(CommandContext<CommandSource> ctx, IProtectedRegion region, IFlag regionFlag) {
        if (region.containsFlag(regionFlag.getName())) {
            IFlag flag = region.getFlag(regionFlag.getName());
            return setFlagMuteState(ctx, region, flag, !flag.getFlagMsg().isMuted());
        } else {
            MessageUtil.sendCmdFeedback(ctx.getSource(), new TranslationTextComponent("cli.msg.info.region.flag.not-present",
                    buildRegionInfoLink(region), regionFlag.getName()));
            return 1;
        }
    }

    private static int setFlagMuteState(CommandContext<CommandSource> ctx, IProtectedRegion region, IFlag flag, boolean setMuted) {
        flag.getFlagMsg().mute(setMuted);
        IFormattableTextComponent undoLink = buildRegionActionUndoLink(ctx.getInput(), String.valueOf(!setMuted), String.valueOf(setMuted));
        IFormattableTextComponent msg = new TranslationTextComponent("cli.flag.msg.mute.success.text",
                buildFlagInfoLink(region, flag), flag.getFlagMsg().isMuted())
                .append(" ")
                .append(undoLink);
        MessageUtil.sendCmdFeedback(ctx.getSource(), msg);
        RegionDataManager.save();
        return 0;

    }

    private static int setRegionFlagMsg(CommandContext<CommandSource> ctx, IProtectedRegion region, IFlag flag, String flagMsgStr) {
        String oldFlagMsg = flag.getFlagMsg().getMsg();
        FlagMessage flagMsg = new FlagMessage(flagMsgStr, flag.getFlagMsg().isMuted());
        flag.setFlagMsg(flagMsg);
        IFormattableTextComponent undoLink = buildRegionActionUndoLink(ctx.getInput(), flagMsgStr, oldFlagMsg);
        IFormattableTextComponent msg = new TranslationTextComponent("cli.flag.msg.msg.success.text",
                buildFlagInfoLink(region, flag), flagMsgStr)
                .append(" ")
                .append(undoLink);
        MessageUtil.sendCmdFeedback(ctx.getSource(), msg);
        RegionDataManager.save();
        return 0;
    }

    private static int setEnableState(CommandContext<CommandSource> ctx, IProtectedRegion region, IFlag regionFlag) {
        if (region.containsFlag(regionFlag.getName())) {
            IFlag flag = region.getFlag(regionFlag.getName());
            return setEnableState(ctx, region, regionFlag, !flag.isActive());
        } else {
            MessageUtil.sendCmdFeedback(ctx.getSource(), new TranslationTextComponent("cli.msg.info.region.flag.not-present",
                    buildRegionInfoLink(region), regionFlag.getName()));
            return 1;
        }
    }

    private static int setEnableState(CommandContext<CommandSource> ctx, IProtectedRegion region, IFlag flag, boolean enable) {

        flag.setIsActive(enable);
        IFormattableTextComponent undoLink = buildRegionActionUndoLink(ctx.getInput(), String.valueOf(!enable), String.valueOf(enable));
        IFormattableTextComponent msg = new TranslationTextComponent("cli.flag.enable.success.text",
                buildFlagInfoLink(region, flag), flag.isActive())
                .append(" ")
                .append(undoLink);
        MessageUtil.sendCmdFeedback(ctx.getSource(), msg);
        RegionDataManager.save();
        return 0;

    }

    public static int setInvertState(CommandContext<CommandSource> ctx, IProtectedRegion region, IFlag regionFlag) {
        if (region.containsFlag(regionFlag.getName())) {
            IFlag flag = region.getFlag(regionFlag.getName());
            return setInvertState(ctx, region, flag, !flag.doesOverride());
        } else {
            MessageUtil.sendCmdFeedback(ctx.getSource(), new TranslationTextComponent("cli.msg.info.region.flag.not-present",
                    buildRegionInfoLink(region), regionFlag.getName()));
            return 1;
        }
    }

    public static int setInvertState(CommandContext<CommandSource> ctx, IProtectedRegion region, IFlag flag, boolean invert) {
        flag.setOverride(invert);
        IFormattableTextComponent undoLink = buildRegionActionUndoLink(ctx.getInput(), String.valueOf(!invert), String.valueOf(invert));
        IFormattableTextComponent msg = new TranslationTextComponent("cli.flag.invert.success.text",
                buildFlagInfoLink(region, flag), flag.doesOverride())
                .append(" ")
                .append(undoLink);
        MessageUtil.sendCmdFeedback(ctx.getSource(), msg);
        RegionDataManager.save();
        return 0;
    }

}
