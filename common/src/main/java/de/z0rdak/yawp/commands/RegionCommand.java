package de.z0rdak.yawp.commands;

import com.mojang.brigadier.Command;
import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import de.z0rdak.yawp.api.commands.CommandConstants;
import de.z0rdak.yawp.api.core.region.hierarchy.HierarchyValidationResult;
import de.z0rdak.yawp.api.core.region.hierarchy.RegionHierarchy;
import de.z0rdak.yawp.commands.arguments.ArgumentUtil;
import de.z0rdak.yawp.commands.suggestions.*;
import de.z0rdak.yawp.core.area.*;
import de.z0rdak.yawp.core.area.visuals.DisplayType;
import de.z0rdak.yawp.core.flag.FlagMessage;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.core.region.*;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.SharedSuggestionProvider;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.commands.arguments.IdentifierArgument;
import net.minecraft.commands.arguments.coordinates.BlockPosArgument;
import net.minecraft.network.chat.Component;

import java.util.List;
import java.util.function.Function;

import static de.z0rdak.yawp.api.MessageSender.sendCmdFeedback;
import static de.z0rdak.yawp.api.MessageSender.sendError;
import static de.z0rdak.yawp.api.commands.CommandConstants.*;
import static de.z0rdak.yawp.api.commands.CommandConstants.REGION;
import static de.z0rdak.yawp.commands.CommandUtil.*;
import static de.z0rdak.yawp.commands.RegionCommandHelper.*;
import static de.z0rdak.yawp.commands.arguments.ArgumentUtil.*;
import static de.z0rdak.yawp.commands.arguments.ArgumentUtil.getRegionArgument;
import static de.z0rdak.yawp.constants.Constants.MAX_BUILD_LIMIT;
import static de.z0rdak.yawp.constants.Constants.MIN_BUILD_LIMIT;
import static de.z0rdak.yawp.util.ChatLinkBuilder.buildRegionInfoLink;


class RegionCommand {

    private RegionCommand() {
    }


    static LiteralArgumentBuilder<CommandSourceStack> build() {
        return literal(REGION)
                .then(Commands.argument(REGION.toString(), IdentifierArgument.id())
                        .suggests(new RegionSuggestionProvider())
                        .executes(ctx -> promptRegionInfo(ctx, getRegionArgument(ctx)))
                        .then(literal(INFO).executes(ctx -> promptRegionInfo(ctx, getRegionArgument(ctx))))
                        .then(buildAddSubCommand(ArgumentUtil::getRegionArgument))
                        .then(literal(ADD)
                                .then(literal(SUBREGION) // need containment check
                                        .then(buildCreateCuboid()) // validate region is not global
                                        .then(buildCreateSphere()) // validate region is not global
                                        .then(literal(EXISTING)
                                                .then(Commands.argument(SUBREGION.toString(), IdentifierArgument.id())
                                                        .suggests(new ValidChildRegionSuggestionProvider())
                                                        .executes(ctx -> addSubRegion(ctx, getRegionArgument(ctx), getSubRegionArgument(ctx)))
                                                )
                                        )
                                )
                        )
                        .then(buildRemoveSubCommand(ArgumentUtil::getRegionArgument))
                        .then(buildClearSubCommand(ArgumentUtil::getRegionArgument))
                        .then(buildListSubCommand(ArgumentUtil::getRegionArgument))
                        .then(buildCopyCommand(ArgumentUtil::getRegionArgument))
                        .then(flagSubCmd(ArgumentUtil::getRegionArgument))

                        .then(literal(STATE)
                                .executes(ctx -> CommandUtil.promptRegionState(ctx, getRegionArgument(ctx)))
                                .then(literal(ALERT)
                                        .executes(ctx -> setAlertState(ctx, getRegionArgument(ctx), getRegionArgument(ctx).isMuted()))
                                        .then(Commands.argument(ALERT.toString(), BoolArgumentType.bool())
                                                .executes(ctx -> setAlertState(ctx, getRegionArgument(ctx), getAlertArgument(ctx)))))
                                .then(literal(ENABLE)
                                        .executes(ctx -> setActiveState(ctx, getRegionArgument(ctx), !getRegionArgument(ctx).isActive()))
                                        .then(Commands.argument(ENABLE.toString(), BoolArgumentType.bool())
                                                .executes(ctx -> setActiveState(ctx, getRegionArgument(ctx), getEnableArgument(ctx)))))
                                // TODO: level only
                                .then(literal(ALERT_LOCAL)
                                        .then(Commands.argument(ALERT.toString(), BoolArgumentType.bool())
                                                .executes(ctx -> setAlertStateForAllLocal(ctx, getLevelDataArgument(ctx), getAlertArgument(ctx))))
                                )
                                // TODO: level only
                                .then(literal(ENABLE_LOCAL)
                                        .then(Commands.argument(ENABLE.toString(), BoolArgumentType.bool())
                                                .executes(ctx -> setActiveStateForAllLocal(ctx, getLevelDataArgument(ctx), getEnableArgument(ctx))))
                                )
                                // TODO this is only valid for local regions and will be caught by the getLocalRegionArgument
                                .then(literal(PRIORITY)
                                        .then(Commands.argument(PRIORITY.toString(), IntegerArgumentType.integer())
                                                .executes(ctx -> setPriority(ctx, getLocalRegionArgument(ctx), getPriorityArgument(ctx))))
                                        .then(literal(INC)
                                                .then(Commands.argument(PRIORITY.toString(), IntegerArgumentType.integer())
                                                        .executes(ctx -> setPriority(ctx, getLocalRegionArgument(ctx), getPriorityArgument(ctx), 1))))
                                        .then(literal(DEC)
                                                .then(Commands.argument(PRIORITY.toString(), IntegerArgumentType.integer())
                                                        .executes(ctx -> setPriority(ctx, getLocalRegionArgument(ctx), getPriorityArgument(ctx), -1))))))

                        // Local Region exclusive
                        .then(buildAreaSubCmd(ArgumentUtil::getLocalRegionArgument))
                        .then(buildTpAnchorSubCmd(ArgumentUtil::getLocalRegionArgument))
                        .then(buildShowSubCmd(ArgumentUtil::getLocalRegionArgument))
                );
    }

    private static int addSubRegion(CommandContext<CommandSourceStack> ctx, IProtectedRegion parent, IProtectedRegion child) {
        HierarchyValidationResult result = RegionHierarchy.validateParent(child, parent);
        if (!result.valid()) {
            sendError(ctx.getSource(), Component.translatableWithFallback("","Cannot add region %s", result.reason()));
            return 0;
        }
        RegionHierarchy.setParent(child, parent);
        sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("","Added region %s to %s", buildRegionInfoLink(child), buildRegionInfoLink(parent) ));
        return Command.SINGLE_SUCCESS;
    }

    public static LiteralArgumentBuilder<CommandSourceStack> buildCreateCuboid() {
        Command<CommandSourceStack> command =
                ctx -> createCuboidRegion(ctx, getRegionNameArgument(ctx),
                BlockPosArgument.getSpawnablePos(ctx, POS1.toString()),
                BlockPosArgument.getSpawnablePos(ctx, POS2.toString()));

        return Commands.literal(AreaType.CUBOID.areaType)
                .then(Commands.argument(CommandConstants.NAME.toString(), StringArgumentType.word())
                        .then(Commands.argument(POS1.toString(), BlockPosArgument.blockPos())
                                .then(Commands.argument(POS2.toString(), BlockPosArgument.blockPos())
                                        .executes(command))));
    }

    public static LiteralArgumentBuilder<CommandSourceStack> buildCreateSphere() {
        Command<CommandSourceStack> command = ctx ->
                createSphereRegion(ctx, getRegionNameArgument(ctx),
                        BlockPosArgument.getSpawnablePos(ctx, CENTER_POS.toString()),
                        IntegerArgumentType.getInteger(ctx, RADIUS.toString()));

        return Commands.literal(AreaType.SPHERE.areaType)
                .then(Commands.argument(CommandConstants.NAME.toString(), StringArgumentType.word())
                        .then(Commands.argument(CENTER_POS.toString(), BlockPosArgument.blockPos())
                                .then(Commands.argument(RADIUS.toString(), IntegerArgumentType.integer(0))
                                        .executes(command))));
    }

    static LiteralArgumentBuilder<CommandSourceStack> buildShowSubCmd(Function<CommandContext<CommandSourceStack>, IMarkableRegion> regionSupplier) {
        return literal(SHOW)
                .executes(ctx -> promptVisualizationOptions(ctx, regionSupplier.apply(ctx)))
                .then(literal(LOCAL)
                        .executes(ctx -> showRegion(ctx, regionSupplier.apply(ctx), DisplayType.FRAME))
                        .then(Commands.argument(STYLE.toString(), StringArgumentType.word())
                                .suggests(new DisplayTypeSuggestionProvider())
                                .executes(ctx -> showRegion(ctx, regionSupplier.apply(ctx), getDisplayTypeArgument(ctx)))
                                .then(Commands.argument(BLOCK.toString(), IdentifierArgument.id())
                                        .executes(ctx -> showRegion(ctx, regionSupplier.apply(ctx),
                                                getDisplayTypeArgument(ctx),
                                                getDisplayBlockArgument(ctx)))
                                        .then(Commands.argument(GLOW.toString(), BoolArgumentType.bool())
                                                .executes(ctx -> showRegion(ctx, regionSupplier.apply(ctx),
                                                        getDisplayTypeArgument(ctx),
                                                        getDisplayBlockArgument(ctx),
                                                        getDisplayGlowArgument(ctx)))
                                                .then(Commands.argument(LIGHT_LEVEL.toString(), IntegerArgumentType.integer(0, 15))
                                                        .executes(ctx -> showRegion(ctx, regionSupplier.apply(ctx),
                                                                getDisplayTypeArgument(ctx),
                                                                getDisplayBlockArgument(ctx),
                                                                getDisplayGlowArgument(ctx),
                                                                IntegerArgumentType.getInteger(ctx, LIGHT_LEVEL.toString())))
                                                )
                                        )
                                )
                        )
                )
                .then(literal(HIERARCHY)
                        .executes(ctx -> showRegionHierarchy(ctx, regionSupplier.apply(ctx), DisplayType.FRAME, true))
                        .then(Commands.argument(STYLE.toString(), StringArgumentType.word())
                                .suggests(new DisplayTypeSuggestionProvider())
                                .executes(ctx -> showRegionHierarchy(ctx, regionSupplier.apply(ctx), getDisplayTypeArgument(ctx), false))
                                .then(Commands.argument(RECURSIVE.toString(), BoolArgumentType.bool())
                                        .executes(ctx -> showRegionHierarchy(ctx, regionSupplier.apply(ctx), getDisplayTypeArgument(ctx), BoolArgumentType.getBool(ctx, RECURSIVE.toString())))
                                )
                        )
                )
                .then(literal(INTERSECTING)
                        .executes(ctx -> showRegionsIntersecting(ctx, regionSupplier.apply(ctx), DisplayType.FRAME))
                        .then(Commands.argument(STYLE.toString(), StringArgumentType.word())
                                .suggests(new DisplayTypeSuggestionProvider())
                                .executes(ctx -> showRegionsIntersecting(ctx, regionSupplier.apply(ctx), getDisplayTypeArgument(ctx)))
                        )
                );
    }
    static LiteralArgumentBuilder<CommandSourceStack> buildTpAnchorSubCmd(Function<CommandContext<CommandSourceStack>, IMarkableRegion> regionSupplier) {
        return literal(TP_ANCHOR)
                .then(literal(RENAME)
                        .then(Commands.argument(NAME.toString(), StringArgumentType.word())
                                .then(Commands.argument(RENAME.toString(), StringArgumentType.word())
                                        .executes(ctx -> renameTeleportAnchor(ctx, regionSupplier.apply(ctx), getTeleportAnchorNameArgument(ctx), getNewTeleportAnchorNameArgument(ctx)))
                                )
                        )
                )
                .then(literal(SET)
                        .then(Commands.argument(NAME.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(List.of("tpAnchor-name"), builder))
                                .then(Commands.argument(TP_ANCHOR.toString(), BlockPosArgument.blockPos())
                                        .executes(ctx -> updateTeleportAnchor(ctx, regionSupplier.apply(ctx), getTeleportAnchorPosArgument(ctx), getTeleportAnchorNameArgument(ctx)))
                                )
                        )
                )
                .then(literal(HIDE)
                        .then(Commands.argument(TP_ANCHOR.toString(), StringArgumentType.word())
                                .executes(ctx -> hideTpAnchor(ctx, regionSupplier.apply(ctx), StringArgumentType.getString(ctx, TP_ANCHOR.toString())))
                        )
                )
                .then(literal(SHOW)
                        .then(Commands.argument(TP_ANCHOR.toString(), StringArgumentType.word())
                                .executes(ctx -> showTpAnchor(ctx, regionSupplier.apply(ctx), StringArgumentType.getString(ctx, TP_ANCHOR.toString())))
                        )
                )
                .then(literal(TELEPORT)
                        .then(Commands.argument(TP_ANCHOR.toString(), StringArgumentType.word())
                                .executes(ctx -> teleport(ctx, regionSupplier.apply(ctx), StringArgumentType.getString(ctx, TP_ANCHOR.toString())))
                                .then(Commands.argument(PLAYER.toString(), EntityArgument.player())
                                        .executes(ctx -> teleport(ctx, regionSupplier.apply(ctx), StringArgumentType.getString(ctx, TP_ANCHOR.toString()), getPlayerArgument(ctx)))
                                )
                        )
                );
    }
    static LiteralArgumentBuilder<CommandSourceStack> buildAreaSubCmd(Function<CommandContext<CommandSourceStack>, IMarkableRegion> regionSupplier) {
        return literal(AREA)
                .executes(ctx -> promptRegionAreaInfo(ctx, regionSupplier.apply(ctx)))
                .then(literal(SET)
                        .then(Commands.literal(AreaType.CUBOID.areaType)
                                .then(Commands.argument(POS1.toString(), BlockPosArgument.blockPos())
                                        .then(Commands.argument(POS2.toString(), BlockPosArgument.blockPos())
                                                .executes(ctx -> setCuboidArea(ctx, regionSupplier.apply(ctx), BlockPosArgument.getSpawnablePos(ctx, POS1.toString()), BlockPosArgument.getSpawnablePos(ctx, POS2.toString()))))))
                        .then(Commands.literal(AreaType.SPHERE.areaType)
                                .then(Commands.argument(CENTER_POS.toString(), BlockPosArgument.blockPos())
                                        .then(Commands.argument(RADIUS.toString(), IntegerArgumentType.integer(0))
                                                .executes(ctx -> setSphereArea(ctx, regionSupplier.apply(ctx), BlockPosArgument.getSpawnablePos(ctx, CENTER_POS.toString()), IntegerArgumentType.getInteger(ctx, RADIUS.toString()))))))
                )
                .then(literal(EXPAND)
                        .then(Commands.literal(AreaType.CUBOID.areaType)
                                .executes(ctx -> expandCuboid(ctx, regionSupplier.apply(ctx), MIN_BUILD_LIMIT, MAX_BUILD_LIMIT))
                                .then(Commands.argument(Y_MIN.toString(), IntegerArgumentType.integer())
                                        .then(Commands.argument(Y_MAX.toString(), IntegerArgumentType.integer())
                                                .executes(ctx -> expandCuboid(ctx, regionSupplier.apply(ctx), IntegerArgumentType.getInteger(ctx, Y_MIN.toString()), IntegerArgumentType.getInteger(ctx, Y_MAX.toString()))))))
                        .then(Commands.literal(AreaType.SPHERE.areaType)
                                .executes(ctx -> expandSphere(ctx, regionSupplier.apply(ctx), 1))
                                .then(Commands.argument(EXPANSION.toString(), IntegerArgumentType.integer())
                                        .executes(ctx -> expandSphere(ctx, regionSupplier.apply(ctx), IntegerArgumentType.getInteger(ctx, EXPANSION.toString()))))
                        )
                );
    }


    private static LiteralArgumentBuilder<CommandSourceStack> flagSubCmd(Function<CommandContext<CommandSourceStack>, IProtectedRegion> regionSupplier) {
        return literal(FLAG)
                .executes(ctx -> CommandUtil.promptRegionFlagList(ctx, regionSupplier.apply(ctx), 0))
                .then(Commands.argument(FLAG.toString(), StringArgumentType.word())
                        .suggests(new ExistingFlagsSuggestionProvider())
                        //   .suggests(new ExistingFlagsSuggestionProvider())
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
                                                .executes(ctx -> setFlagMuteState(ctx, regionSupplier.apply(ctx), getIFlagArgument(ctx), muteArgument(ctx))))
                                )
                                .then(literal(SET)
                                        .then(Commands.argument(MSG.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(flagMsgExamples(), builder))
                                                .executes(ctx -> setRegionFlagMsg(ctx, regionSupplier.apply(ctx), getIFlagArgument(ctx), getFlagMsgArgument(ctx))))
                                )
                                .then(literal(CLEAR)
                                        .executes(ctx -> setRegionFlagMsg(ctx, regionSupplier.apply(ctx), getIFlagArgument(ctx), FlagMessage.CONFIG_MSG))
                                )
                        ));
    }


    static LiteralArgumentBuilder<CommandSourceStack> buildOld() {
        return literal(LOCAL)
                .then(literal(DELETE)
                        .executes(ctx -> attemptDeleteRegion(ctx, getLevelDataArgument(ctx), getLocalRegionArgument(ctx)))
                        .then(literal(FOR_SURE)
                                .executes(ctx -> deleteRegion(ctx, getLevelDataArgument(ctx), getLocalRegionArgument(ctx)))))
                .then(literal(ADD).then(literal(CHILD)
                        .then(Commands.argument(CHILD.toString(), StringArgumentType.word())
                                .suggests(new ValidChildRegionSuggestionProvider())
                                .executes(ctx -> addChildren(ctx, getLocalRegionArgument(ctx), getChildRegionArgument(ctx))))))
                .then(literal(REMOVE).then(literal(CHILD)
                        .then(Commands.argument(CHILD.toString(), StringArgumentType.word())
                                .suggests(new ChildRegionSuggestionProvider())
                                .executes(ctx -> removeChildren(ctx, getLevelDataArgument(ctx), getLocalRegionArgument(ctx), getChildRegionArgument(ctx))))))
                .then(literal(LIST)
                        .then(literal(TP_ANCHOR)
                                .executes(ctx -> promptTeleportAnchorPagination(ctx, getLocalRegionArgument(ctx), 0))
                                .then(Commands.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                        .executes(ctx -> promptTeleportAnchorPagination(ctx, getLocalRegionArgument(ctx), getPageNoArgument(ctx)))
                                )
                        )
                )
                .then(literal(ADD)
                        .then(literal(TP_ANCHOR)
                                .then(Commands.argument(NAME.toString(), StringArgumentType.word())
                                        .then(Commands.argument(TP_ANCHOR.toString(), BlockPosArgument.blockPos())
                                                .executes(ctx -> updateTeleportAnchor(ctx, getLocalRegionArgument(ctx), getTeleportAnchorPosArgument(ctx), getTeleportAnchorNameArgument(ctx)))
                                        )
                                )
                        )
                )
                .then(literal(REMOVE)
                        .then(literal(TP_ANCHOR)
                                .then(Commands.argument(NAME.toString(), StringArgumentType.word())
                                        .suggests(new TeleportAnchorSuggestionProvider())
                                        .executes(ctx -> removeTeleportAnchor(ctx, getLocalRegionArgument(ctx), getTeleportAnchorNameArgument(ctx)))
                                )
                        )
                )

                .then(literal(RENAME)
                        .then(Commands.argument(NAME.toString(), StringArgumentType.word())
                                .executes(ctx -> renameRegion(ctx, getLocalRegionArgument(ctx), getRegionNameArgument(ctx), getLevelDataArgument(ctx)))
                        )
                )
                .then(literal(HIDE)
                        .then(literal(LOCAL)
                                .executes(ctx -> hideRegion(ctx, getLocalRegionArgument(ctx), DisplayType.FRAME))
                                .then(Commands.argument(STYLE.toString(), StringArgumentType.word())
                                        .suggests(new DisplayTypeSuggestionProvider())
                                        .executes(ctx -> hideRegion(ctx, getLocalRegionArgument(ctx), getDisplayTypeArgument(ctx)))
                                )
                        )
                        .then(literal(HIERARCHY)
                                .executes(ctx -> hideRegionHierarchy(ctx, getLocalRegionArgument(ctx), DisplayType.FRAME, true))
                                .then(Commands.argument(STYLE.toString(), StringArgumentType.word())
                                        .suggests(new DisplayTypeSuggestionProvider())
                                        .executes(ctx -> hideRegionHierarchy(ctx, getLocalRegionArgument(ctx), getDisplayTypeArgument(ctx), false))
                                        .then(Commands.argument(RECURSIVE.toString(), BoolArgumentType.bool())
                                                .executes(ctx -> hideRegionHierarchy(ctx, getLocalRegionArgument(ctx), getDisplayTypeArgument(ctx), BoolArgumentType.getBool(ctx, RECURSIVE.toString())))
                                        )
                                )
                        )
                        .then(literal(INTERSECTING)
                                .executes(ctx -> hideRegionsIntersecting(ctx, getLocalRegionArgument(ctx), DisplayType.FRAME))
                                .then(Commands.argument(STYLE.toString(), StringArgumentType.word())
                                        .suggests(new DisplayTypeSuggestionProvider())
                                        .executes(ctx -> hideRegionsIntersecting(ctx, getLocalRegionArgument(ctx), getDisplayTypeArgument(ctx)))
                                )
                        )
                )
                .then(literal(DISPLAY)
                        .executes(ctx -> promptDisplaySettings(ctx, getLocalRegionArgument(ctx)))
                        .then(literal(BLOCK)
                                .then(Commands.argument(BLOCK.toString(), IdentifierArgument.id())
                                        .executes(ctx -> setDisplayBlock(ctx, getLocalRegionArgument(ctx), getDisplayBlockArgument(ctx)))
                                )
                        )
                        .then(literal(GLOW)
                                .then(Commands.argument(GLOW.toString(), BoolArgumentType.bool())
                                        .executes(ctx -> setDisplayGlow(ctx, getLocalRegionArgument(ctx), getDisplayGlowArgument(ctx)))
                                )
                        )
                        .then(literal(LIGHT_LEVEL)
                                .then(Commands.argument(LIGHT_LEVEL.toString(), IntegerArgumentType.integer(0, 15))
                                        .executes(ctx -> setDisplayLightLevel(ctx, getLocalRegionArgument(ctx), IntegerArgumentType.getInteger(ctx, LIGHT_LEVEL.toString())))
                                )
                        )
                        .then(literal(RESET)
                                .executes(ctx -> resetDisplaySettings(ctx, getLocalRegionArgument(ctx)))
                        )


                );
    }

}
