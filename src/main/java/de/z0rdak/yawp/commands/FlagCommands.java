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
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.SharedSuggestionProvider;
import net.minecraft.commands.arguments.DimensionArgument;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.resources.ResourceLocation;

import static de.z0rdak.yawp.util.CommandUtil.literal;
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

    public static LiteralArgumentBuilder<CommandSourceStack> build() {
        return literal(FLAG)
                //.then(literal(GLOBAL)
                //        .then(flagGlobalSubCommands()))
                //.then(literal(DIM)
                //        .then(flagDimSubCommands()))
                .then(literal(LOCAL)
                        .then(flagLocalSubCommands()));
    }

    public static RequiredArgumentBuilder<CommandSourceStack, ResourceLocation> flagDimSubCommands() {
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
                        .then(literal(NEGATE)
                                .executes(ctx -> setInvertState(ctx, getRegionArgument(ctx), getIFlagArgument(ctx)))
                                .then(Commands.argument(NEGATE.toString(), BoolArgumentType.bool())
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
                                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(flagMsgExamples, builder))
                                                .executes(ctx -> setRegionFlagMsg(ctx, getRegionArgument(ctx), getIFlagArgument(ctx), getFlagMsgArgument(ctx))))
                                )
                                .then(literal(CLEAR)
                                        .executes(ctx -> setRegionFlagMsg(ctx, getRegionArgument(ctx), getIFlagArgument(ctx), FlagMessage.CONFIG_MSG))
                                )
                        )

                );
    }

    public static LiteralArgumentBuilder<CommandSourceStack> flagGlobalSubCommands() {
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
                        .then(literal(NEGATE)
                                .executes(ctx -> setGlobalInvertState(ctx, getFlagArgument(ctx)))
                                .then(Commands.argument(NEGATE.toString(), BoolArgumentType.bool())
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
                                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(flagMsgExamples, builder))
                                                .executes(ctx -> setGlobalRegionFlagMsg(ctx, getFlagArgument(ctx), getFlagMsgArgument(ctx))))
                                )
                                .then(literal(CLEAR)
                                        .executes(ctx -> setGlobalRegionFlagMsg(ctx, getFlagArgument(ctx), FlagMessage.CONFIG_MSG))
                                )
                        )
                );
    }

    private static int promptGlobalFlagInfo(CommandContext<CommandSourceStack> ctx, RegionFlag flagArgument) {
        return 0;
    }

    private static int setGlobalEnableState(CommandContext<CommandSourceStack> ctx, RegionFlag flagArgument, boolean enableArgument) {
        return 0;
    }

    private static int setGlobalEnableState(CommandContext<CommandSourceStack> ctx, RegionFlag flagArgument) {
        return 0;
    }

    private static int setGlobalInvertState(CommandContext<CommandSourceStack> ctx, RegionFlag flagArgument, boolean negationArgument) {
        return 1;
    }

    private static int setGlobalInvertState(CommandContext<CommandSourceStack> ctx, RegionFlag flagArgument) {
        return 1;
    }

    private static int setGlobalFlagMuteState(CommandContext<CommandSourceStack> ctx, RegionFlag flagArgument, boolean muteArgument) {
        return 0;
    }

    private static int setGlobalFlagMuteState(CommandContext<CommandSourceStack> ctx, RegionFlag flagArgument) {
        return 0;
    }

    private static int setGlobalRegionFlagMsg(CommandContext<CommandSourceStack> ctx, RegionFlag flagArgument, String configMsg) {
        return 0;
    }

    private static int setGlobalRegionFlagMsg(CommandContext<CommandSourceStack> ctx, RegionFlag flagArgument) {
        return 0;
    }

    public static RequiredArgumentBuilder<CommandSourceStack, ResourceLocation> flagLocalSubCommands() {
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
                                .then(literal(NEGATE)
                                        .executes(ctx -> setInvertState(ctx, getRegionArgument(ctx), getIFlagArgument(ctx)))
                                        .then(Commands.argument(NEGATE.toString(), BoolArgumentType.bool())
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
                                                        .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(flagMsgExamples, builder))
                                                        .executes(ctx -> setRegionFlagMsg(ctx, getRegionArgument(ctx), getIFlagArgument(ctx), getFlagMsgArgument(ctx))))
                                        )
                                        .then(literal(CLEAR)
                                                .executes(ctx -> setRegionFlagMsg(ctx, getRegionArgument(ctx), getIFlagArgument(ctx), FlagMessage.CONFIG_MSG))
                                        )

                                )
                        )
                );
    }


    private static int promptFlagInfo(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, IFlag flag) {
        MessageUtil.sendCmdFeedback(ctx.getSource(), MessageUtil.buildFlagInfoComponent(region, flag, RegionType.LOCAL));
        return 0;
    }

    private static int promptDimFlagInfo(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, RegionFlag regionFlag) {
        if (region.containsFlag(regionFlag)) {
            IFlag flag = region.getFlag(regionFlag.name);
            MessageUtil.sendCmdFeedback(ctx.getSource(), MessageUtil.buildFlagInfoComponent(region, flag, RegionType.DIMENSION));
            return 0;
        } else {
            return 1;
        }
    }

    private static int setFlagMuteState(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, IFlag flag) {
        return setFlagMuteState(ctx, region, flag, !flag.getFlagMsg().isMuted());
    }

    private static int setFlagMuteState(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, IFlag flag, boolean setMuted) {
        if (region.containsFlag(flag.getName())) {
            flag.getFlagMsg().mute(setMuted);
            MutableComponent undoLink = buildRegionActionUndoLink(ctx.getInput(), String.valueOf(!setMuted), String.valueOf(setMuted));
            MutableComponent msg = new TranslatableComponent("cli.flag.msg.mute.success.text",
                    buildFlagInfoLink(region, flag, RegionType.LOCAL), flag.getFlagMsg().isMuted())
                    .append(" ")
                    .append(undoLink);
            MessageUtil.sendCmdFeedback(ctx.getSource(), msg);
            RegionDataManager.save();
            return 0;
        } else {
            MessageUtil.sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.info.region.flag.missing",
                    buildRegionInfoLink(region, RegionType.LOCAL), flag.getName()));
            return 1;
        }
    }

    private static int setRegionFlagMsg(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, IFlag flag, String flagMsgStr) {
        if (region.containsFlag(flag.getName())) {
            String oldFlagMsg = flag.getFlagMsg().getMsg();
            FlagMessage flagMsg = new FlagMessage(flagMsgStr, flag.getFlagMsg().isMuted());
            flag.setFlagMsg(flagMsg);
            MutableComponent undoLink = buildRegionActionUndoLink(ctx.getInput(), flagMsgStr, oldFlagMsg);
            MutableComponent msg = new TranslatableComponent("cli.flag.msg.msg.success.text",
                    buildFlagInfoLink(region, flag, RegionType.LOCAL), flagMsgStr)
                    .append(" ")
                    .append(undoLink);
            MessageUtil.sendCmdFeedback(ctx.getSource(), msg);
            RegionDataManager.save();
            return 0;
        } else {
            MessageUtil.sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.info.region.flag.missing",
                    buildRegionInfoLink(region, RegionType.LOCAL), flag.getName()));
            return 1;
        }
    }

    private static int setEnableState(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, IFlag flag) {
        return setEnableState(ctx, region, flag, !flag.isActive());
    }

    private static int setEnableState(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, IFlag flag, boolean enable) {
        if (region.containsFlag(flag.getName())) {
            flag.setIsActive(enable);
            MutableComponent undoLink = buildRegionActionUndoLink(ctx.getInput(), String.valueOf(!enable), String.valueOf(enable));
            MutableComponent msg = new TranslatableComponent("cli.flag.enable.success.text",
                    buildFlagInfoLink(region, flag, RegionType.LOCAL), flag.isActive())
                    .append(" ")
                    .append(undoLink);
            MessageUtil.sendCmdFeedback(ctx.getSource(), msg);
            RegionDataManager.save();
            return 0;

        } else {
            MessageUtil.sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.info.region.flag.missing",
                    buildRegionInfoLink(region, RegionType.LOCAL), flag.getName()));
            return 1;
        }
    }

    public static int setInvertState(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, IFlag flag) {
        return setInvertState(ctx, region, flag, !flag.isInverted());
    }

    public static int setInvertState(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, IFlag flag, boolean invert) {
        if (region.containsFlag(flag.getName())) {
            flag.setInverted(invert);
            MutableComponent undoLink = buildRegionActionUndoLink(ctx.getInput(), String.valueOf(!invert), String.valueOf(invert));
            MutableComponent msg = new TranslatableComponent("cli.flag.invert.success.text",
                    buildFlagInfoLink(region, flag, RegionType.LOCAL), flag.isInverted())
                    .append(" ")
                    .append(undoLink);
            MessageUtil.sendCmdFeedback(ctx.getSource(), msg);
            RegionDataManager.save();
            return 0;
        } else {
            MessageUtil.sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.info.region.flag.missing",
                    buildRegionInfoLink(region, RegionType.LOCAL), flag.getName()));
            return 1;
        }
    }

}
