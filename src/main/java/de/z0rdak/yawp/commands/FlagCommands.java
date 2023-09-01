package de.z0rdak.yawp.commands;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.builder.RequiredArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import de.z0rdak.yawp.commands.arguments.flag.IFlagArgumentType;
import de.z0rdak.yawp.commands.arguments.region.RegionArgumentType;
import de.z0rdak.yawp.core.flag.FlagMessage;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.core.region.RegionType;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.FlagCorrelation;
import de.z0rdak.yawp.util.LocalRegions;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.command.CommandSource;
import net.minecraft.command.Commands;
import net.minecraft.command.ISuggestionProvider;
import net.minecraft.command.arguments.DimensionArgument;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.text.IFormattableTextComponent;
import net.minecraft.util.text.TranslationTextComponent;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.util.CommandUtil.*;
import static de.z0rdak.yawp.util.MessageUtil.*;

public final class FlagCommands {

    private static final List<String> flagMsgExamples = Arrays.asList(
            "<Your flag message here>",
            "You can't place blocks here in the region {region}, {player}!",
            "Sorry, you are not allowed to break blocks here, {player}!",
            "Team '{team}' is not allowed to place blocks here!",
            "You are not allowed to use this block ('{block}' @ '{pos}')!",
            "Only players and teams which are '{affiliation}' are allowed to participate in PVP here!",
            "PVP is disabled in this region {region}!",
            "This action is denied here because of the '{flag}' flag!",
            "Entity {entity} at {pos} can't be tamed because of the active flag '{flag}' in the region '{region}'!",
            "You shall not pass!!!"
    );

    private FlagCommands() {
    }

    public static LiteralArgumentBuilder<CommandSource> build() {
        return literal(FLAG)
                //.then(literal(GLOBAL)
                //        .then(flagGlobalSubCommands()))
                //.then(literal(DIM)
                //        .then(flagDimSubCommands()))
                .then(literal(LOCAL)
                        .then(flagLocalSubCommands()));
    }

    public static RequiredArgumentBuilder<CommandSource, ResourceLocation> flagDimSubCommands() {
        return Commands.argument(DIM.toString(), DimensionArgument.dimension())
                .then(Commands.argument(FLAG.toString(), StringArgumentType.word())
                        .suggests((ctx, builder) -> IFlagArgumentType.flag().listSuggestions(ctx, builder))
                        .executes(ctx -> promptDimFlagInfo(ctx, getDimCacheArgument(ctx).getDimensionalRegion(), getFlagArgument(ctx)))
                        .then(literal(INFO)
                                .executes(ctx -> promptDimFlagInfo(ctx, getDimCacheArgument(ctx).getDimensionalRegion(), getFlagArgument(ctx)))
                        )
                        .then(literal(ENABLE)
                                .executes(ctx -> setEnableState(ctx, getRegionArgument(ctx), getIFlagArgument(ctx)))
                                .then(Commands.argument(ENABLE.toString(), BoolArgumentType.bool())
                                        .executes(ctx -> setEnableState(ctx, getRegionArgument(ctx), getIFlagArgument(ctx), getEnableArgument(ctx))))
                        )
                        .then(literal(OVERRIDE)
                                .executes(ctx -> setInvertState(ctx, getRegionArgument(ctx), getIFlagArgument(ctx)))
                                .then(Commands.argument(OVERRIDE.toString(), BoolArgumentType.bool())
                                        .executes(ctx -> setInvertState(ctx, getRegionArgument(ctx), getIFlagArgument(ctx), getNegationArgument(ctx))))
                        )
                        .then(literal(MSG)
                                .then(literal(MUTE)
                                        .executes(ctx -> setFlagMuteState(ctx, getRegionArgument(ctx), getIFlagArgument(ctx)))
                                        .then(Commands.argument(MUTE.toString(), BoolArgumentType.bool())
                                                .executes(ctx -> setFlagMuteState(ctx, getRegionArgument(ctx), getIFlagArgument(ctx), getMuteArgument(ctx))))
                                )
                                .then(literal(SET)
                                        .then(Commands.argument(MSG.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> ISuggestionProvider.suggest(flagMsgExamples, builder))
                                                .executes(ctx -> setRegionFlagMsg(ctx, getRegionArgument(ctx), getIFlagArgument(ctx), getFlagMsgArgument(ctx))))
                                )
                                .then(literal(CLEAR)
                                        .executes(ctx -> setRegionFlagMsg(ctx, getRegionArgument(ctx), getIFlagArgument(ctx), FlagMessage.CONFIG_MSG))
                                )
                        )

                );
    }

    public static LiteralArgumentBuilder<CommandSource> flagGlobalSubCommands() {
        return literal(GLOBAL)
                .then(Commands.argument(FLAG.toString(), StringArgumentType.word())
                        // FIXME: suggestions not working for global region
                        .suggests((ctx, builder) -> IFlagArgumentType.flag().listSuggestions(ctx, builder))
                        .executes(ctx -> promptGlobalFlagInfo(ctx, getFlagArgument(ctx)))
                        .then(literal(INFO)
                                .executes(ctx -> promptGlobalFlagInfo(ctx, getFlagArgument(ctx)))
                        )
                        .then(literal(ENABLE)
                                .executes(ctx -> setGlobalEnableState(ctx, getFlagArgument(ctx)))
                                .then(Commands.argument(ENABLE.toString(), BoolArgumentType.bool())
                                        .executes(ctx -> setGlobalEnableState(ctx, getFlagArgument(ctx), getEnableArgument(ctx))))
                        )
                        .then(literal(OVERRIDE)
                                .executes(ctx -> setGlobalInvertState(ctx, getFlagArgument(ctx)))
                                .then(Commands.argument(OVERRIDE.toString(), BoolArgumentType.bool())
                                        .executes(ctx -> setGlobalInvertState(ctx, getFlagArgument(ctx), getNegationArgument(ctx))))
                        )
                        .then(literal(MSG)
                                .then(literal(MUTE)
                                        .executes(ctx -> setGlobalFlagMuteState(ctx, getFlagArgument(ctx)))
                                        .then(Commands.argument(MUTE.toString(), BoolArgumentType.bool())
                                                .executes(ctx -> setGlobalFlagMuteState(ctx, getFlagArgument(ctx), getMuteArgument(ctx))))
                                )
                                .then(literal(SET)
                                        .then(Commands.argument(MSG.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> ISuggestionProvider.suggest(flagMsgExamples, builder))
                                                .executes(ctx -> setGlobalRegionFlagMsg(ctx, getFlagArgument(ctx), getFlagMsgArgument(ctx))))
                                )
                                .then(literal(CLEAR)
                                        .executes(ctx -> setGlobalRegionFlagMsg(ctx, getFlagArgument(ctx), FlagMessage.CONFIG_MSG))
                                )
                        )
                );
    }

    private static int promptGlobalFlagInfo(CommandContext<CommandSource> ctx, RegionFlag flagArgument) {
        return 0;
    }

    private static int setGlobalEnableState(CommandContext<CommandSource> ctx, RegionFlag flagArgument, boolean enableArgument) {
        return 0;
    }

    private static int setGlobalEnableState(CommandContext<CommandSource> ctx, RegionFlag flagArgument) {
        return 0;
    }

    private static int setGlobalInvertState(CommandContext<CommandSource> ctx, RegionFlag flagArgument, boolean negationArgument) {
        return 1;
    }

    private static int setGlobalInvertState(CommandContext<CommandSource> ctx, RegionFlag flagArgument) {
        return 1;
    }

    private static int setGlobalFlagMuteState(CommandContext<CommandSource> ctx, RegionFlag flagArgument, boolean muteArgument) {
        return 0;
    }

    private static int setGlobalFlagMuteState(CommandContext<CommandSource> ctx, RegionFlag flagArgument) {
        return 0;
    }

    private static int setGlobalRegionFlagMsg(CommandContext<CommandSource> ctx, RegionFlag flagArgument, String configMsg) {
        return 0;
    }

    private static int setGlobalRegionFlagMsg(CommandContext<CommandSource> ctx, RegionFlag flagArgument) {
        return 0;
    }

    public static RequiredArgumentBuilder<CommandSource, ResourceLocation> flagLocalSubCommands() {
        return Commands.argument(DIM.toString(), DimensionArgument.dimension())
                .then(Commands.argument(REGION.toString(), StringArgumentType.word())
                        .suggests((ctx, builder) -> RegionArgumentType.region().listSuggestions(ctx, builder))
                        .then(Commands.argument(FLAG.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> IFlagArgumentType.flag().listSuggestions(ctx, builder))
                                .executes(ctx -> promptFlagInfo(ctx, getRegionArgument(ctx), getIFlagArgument(ctx)))
                                .then(literal(INFO)
                                        .executes(ctx -> promptFlagInfo(ctx, getRegionArgument(ctx), getIFlagArgument(ctx)))
                                )
                                .then(literal(ENABLE)
                                        .executes(ctx -> setEnableState(ctx, getRegionArgument(ctx), getIFlagArgument(ctx)))
                                        .then(Commands.argument(ENABLE.toString(), BoolArgumentType.bool())
                                                .executes(ctx -> setEnableState(ctx, getRegionArgument(ctx), getIFlagArgument(ctx), getEnableArgument(ctx))))
                                )
                                .then(literal(OVERRIDE)
                                        .executes(ctx -> setInvertState(ctx, getRegionArgument(ctx), getIFlagArgument(ctx)))
                                        .then(Commands.argument(OVERRIDE.toString(), BoolArgumentType.bool())
                                                .executes(ctx -> setInvertState(ctx, getRegionArgument(ctx), getIFlagArgument(ctx), getNegationArgument(ctx))))
                                )
                                .then(literal(MSG)
                                        .then(literal(MUTE)
                                                .executes(ctx -> setFlagMuteState(ctx, getRegionArgument(ctx), getIFlagArgument(ctx)))
                                                .then(Commands.argument(MUTE.toString(), BoolArgumentType.bool())
                                                        .executes(ctx -> setFlagMuteState(ctx, getRegionArgument(ctx), getIFlagArgument(ctx), getMuteArgument(ctx))))
                                        )
                                        .then(literal(SET)
                                                .then(Commands.argument(MSG.toString(), StringArgumentType.string())
                                                        .suggests((ctx, builder) -> ISuggestionProvider.suggest(flagMsgExamples, builder))
                                                        .executes(ctx -> setRegionFlagMsg(ctx, getRegionArgument(ctx), getIFlagArgument(ctx), getFlagMsgArgument(ctx))))
                                        )
                                        .then(literal(CLEAR)
                                                .executes(ctx -> setRegionFlagMsg(ctx, getRegionArgument(ctx), getIFlagArgument(ctx), FlagMessage.CONFIG_MSG))
                                        )
                                )
                        )
                );
    }

    private static int promptFlagInfo(CommandContext<CommandSource> ctx, IMarkableRegion region, IFlag flag) {
        MessageUtil.sendCmdFeedback(ctx.getSource(), MessageUtil.buildFlagInfoComponent(region, flag, RegionType.LOCAL));
        List<FlagCorrelation> flags = new ArrayList<>();
        LocalRegions.getFlagsRecursive(region, flags);

        return 0;
    }

    private static int promptDimFlagInfo(CommandContext<CommandSource> ctx, IProtectedRegion region, RegionFlag regionFlag) {
        if (region.containsFlag(regionFlag)) {
            IFlag flag = region.getFlag(regionFlag.name);
            MessageUtil.sendCmdFeedback(ctx.getSource(), MessageUtil.buildFlagInfoComponent(region, flag, RegionType.DIMENSION));
            return 0;
        } else {
            return 1;
        }
    }

    private static int setFlagMuteState(CommandContext<CommandSource> ctx, IMarkableRegion region, IFlag flag) {
        return setFlagMuteState(ctx, region, flag, !flag.getFlagMsg().isMuted());
    }

    private static int setFlagMuteState(CommandContext<CommandSource> ctx, IMarkableRegion region, IFlag flag, boolean setMuted) {
        if (region.containsFlag(flag.getName())) {
            flag.getFlagMsg().mute(setMuted);
            IFormattableTextComponent undoLink = buildRegionActionUndoLink(ctx.getInput(), String.valueOf(!setMuted), String.valueOf(setMuted));
            IFormattableTextComponent msg = new TranslationTextComponent("cli.flag.msg.mute.success.text",
                    buildFlagInfoLink(region, flag, RegionType.LOCAL), flag.getFlagMsg().isMuted())
                    .append(" ")
                    .append(undoLink);
            MessageUtil.sendCmdFeedback(ctx.getSource(), msg);
            RegionDataManager.save();
            return 0;
        } else {
            MessageUtil.sendCmdFeedback(ctx.getSource(), new TranslationTextComponent("cli.msg.info.region.flag.missing",
                    buildRegionInfoLink(region, RegionType.LOCAL), flag.getName()));
            return 1;
        }
    }

    private static int setRegionFlagMsg(CommandContext<CommandSource> ctx, IMarkableRegion region, IFlag flag, String flagMsgStr) {
        if (region.containsFlag(flag.getName())) {
            String oldFlagMsg = flag.getFlagMsg().getMsg();
            FlagMessage flagMsg = new FlagMessage(flagMsgStr, flag.getFlagMsg().isMuted());
            flag.setFlagMsg(flagMsg);
            IFormattableTextComponent undoLink = buildRegionActionUndoLink(ctx.getInput(), flagMsgStr, oldFlagMsg);
            IFormattableTextComponent msg = new TranslationTextComponent("cli.flag.msg.msg.success.text",
                    buildFlagInfoLink(region, flag, RegionType.LOCAL), flagMsgStr)
                    .append(" ")
                    .append(undoLink);
            MessageUtil.sendCmdFeedback(ctx.getSource(), msg);
            RegionDataManager.save();
            return 0;
        } else {
            MessageUtil.sendCmdFeedback(ctx.getSource(), new TranslationTextComponent("cli.msg.info.region.flag.missing",
                    buildRegionInfoLink(region, RegionType.LOCAL), flag.getName()));
            return 1;
        }
    }

    private static int setEnableState(CommandContext<CommandSource> ctx, IMarkableRegion region, IFlag flag) {
        return setEnableState(ctx, region, flag, !flag.isActive());
    }

    private static int setEnableState(CommandContext<CommandSource> ctx, IMarkableRegion region, IFlag flag, boolean enable) {
        if (region.containsFlag(flag.getName())) {
            flag.setIsActive(enable);
            IFormattableTextComponent undoLink = buildRegionActionUndoLink(ctx.getInput(), String.valueOf(!enable), String.valueOf(enable));
            IFormattableTextComponent msg = new TranslationTextComponent("cli.flag.enable.success.text",
                    buildFlagInfoLink(region, flag, RegionType.LOCAL), flag.isActive())
                    .append(" ")
                    .append(undoLink);
            MessageUtil.sendCmdFeedback(ctx.getSource(), msg);
            RegionDataManager.save();
            return 0;

        } else {
            MessageUtil.sendCmdFeedback(ctx.getSource(), new TranslationTextComponent("cli.msg.info.region.flag.missing",
                    buildRegionInfoLink(region, RegionType.LOCAL), flag.getName()));
            return 1;
        }
    }

    public static int setInvertState(CommandContext<CommandSource> ctx, IMarkableRegion region, IFlag flag) {
        return setInvertState(ctx, region, flag, !flag.doesOverride());
    }

    public static int setInvertState(CommandContext<CommandSource> ctx, IMarkableRegion region, IFlag flag, boolean invert) {
        if (region.containsFlag(flag.getName())) {
            flag.setOverride(invert);
            IFormattableTextComponent undoLink = buildRegionActionUndoLink(ctx.getInput(), String.valueOf(!invert), String.valueOf(invert));
            IFormattableTextComponent msg = new TranslationTextComponent("cli.flag.invert.success.text",
                    buildFlagInfoLink(region, flag, RegionType.LOCAL), flag.doesOverride())
                    .append(" ")
                    .append(undoLink);
            MessageUtil.sendCmdFeedback(ctx.getSource(), msg);
            RegionDataManager.save();
            return 0;
        } else {
            MessageUtil.sendCmdFeedback(ctx.getSource(), new TranslationTextComponent("cli.msg.info.region.flag.missing",
                    buildRegionInfoLink(region, RegionType.LOCAL), flag.getName()));
            return 1;
        }
    }

}
