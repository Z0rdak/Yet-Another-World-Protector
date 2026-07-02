package de.z0rdak.yawp.commands;

import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import de.z0rdak.yawp.api.commands.CommandConstants;
import de.z0rdak.yawp.api.core.RegionManager;
import de.z0rdak.yawp.api.permission.Permissions;
import de.z0rdak.yawp.commands.arguments.ArgumentUtil;
import de.z0rdak.yawp.commands.suggestions.TrackedLevelSuggestionProvider;
import de.z0rdak.yawp.commands.suggestions.UntrackedLevelSuggestionProvider;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.area.SphereArea;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.data.region.LevelData;
import de.z0rdak.yawp.platform.Services;
import de.z0rdak.yawp.util.text.Messages;
import de.z0rdak.yawp.util.text.messages.multiline.MultiLineMessage;
import de.z0rdak.yawp.util.text.messages.pagination.InvalidPageNumberException;
import de.z0rdak.yawp.util.text.messages.pagination.RegionsInDimensionPagination;
import net.minecraft.commands.CommandBuildContext;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.arguments.DimensionArgument;
import net.minecraft.commands.arguments.IdentifierArgument;
import net.minecraft.commands.arguments.coordinates.BlockPosArgument;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.server.level.ServerLevel;

import java.util.Comparator;
import java.util.List;

import static de.z0rdak.yawp.YAWPCommon.VERSION;
import static de.z0rdak.yawp.api.MessageSender.sendError;
import static de.z0rdak.yawp.api.commands.CommandConstants.*;
import static de.z0rdak.yawp.commands.RegionCommand.*;
import static de.z0rdak.yawp.commands.RegionCommandHelper.*;
import static de.z0rdak.yawp.commands.arguments.ArgumentUtil.*;
import static de.z0rdak.yawp.commands.arguments.ArgumentUtil.getRegionNameArgument;
import static de.z0rdak.yawp.util.ChatComponentBuilder.buildHeader;
import static de.z0rdak.yawp.util.ChatComponentBuilder.buildHelpStartComponent;
import static de.z0rdak.yawp.api.MessageSender.sendCmdFeedback;
import static de.z0rdak.yawp.util.ChatLinkBuilder.*;

public final class CommandRegistry {

    private CommandRegistry() {
    }

    public static void registerCommands(CommandDispatcher<CommandSourceStack> cmdDispatcher, CommandBuildContext registryAccess, Commands.CommandSelection env) {
        if (env == Commands.CommandSelection.DEDICATED || env == Commands.CommandSelection.INTEGRATED) {
            try {
                LiteralArgumentBuilder<CommandSourceStack> modCmds = buildCommands();
                cmdDispatcher.register(modCmds);
            }
            catch (Exception e) {
                // Nothing to do here. Since multi project structure was introduced,
                // the config loads differently and will need more than one attempt to register the commands
            }
        }
    }

    private static LiteralArgumentBuilder<CommandSourceStack> buildCommands() {
        return Commands.literal(Constants.MOD_ID)
                .requires(Permissions.get()::isAllowedForNonOp)
                .executes(ctx -> promptHelp(ctx.getSource()))
                .then(ArgumentUtil.literal(CommandConstants.HELP).executes(ctx -> promptHelp(ctx.getSource())))

                .then(AdminCommand.build())

                .then(RegionCommand.build())

                // TODO feature flag from config
                .then(ClaimCommands.build())

                .then(buildCreateInLocal())
                .then(buildCreateLocal())
                .then(buildDeleteLocal())

                .then(literal(TRACK)
                        .then(Commands.argument(LEVEL.toString(), DimensionArgument.dimension())
                                .suggests(new UntrackedLevelSuggestionProvider())
                                .executes(ctx -> trackLevel(ctx, DimensionArgument.getDimension(ctx, LEVEL.toString()))))
                )
                ;
    }

    static LiteralArgumentBuilder<CommandSourceStack> buildDeleteLocal() {
        return literal(DELETE)
                .then(Commands.argument(LOCAL.toString(), StringArgumentType.word())
                       // .suggests((ctx, builder) -> RegionArgumentType.region().listSuggestionsIn(ctx, builder, ctx.getSource().getLevel()))
                        .executes(ctx -> deleteRegion(ctx, getRegionIn(ctx, ctx.getSource().getLevel())))
                )
                .then(literal(DELETE)
                        .executes(ctx -> attemptDeleteRegion(ctx, getLevelDataArgument(ctx), getLocalRegionArgument(ctx)))
                        .then(literal(FOR_SURE)
                                .executes(ctx -> deleteRegion(ctx, getLevelDataArgument(ctx), getLocalRegionArgument(ctx)))))
                ;
    }

    static LiteralArgumentBuilder<CommandSourceStack> buildCreateLocal() {
        var createBuilder = literal(CREATE)
                .then(buildCreateCuboid())
                .then(buildCreateSphere());

        var we_enabled = true;
        if (we_enabled) {
            return createBuilder
                    .then(literal(MARKED)
                            .then(Commands.argument(CommandConstants.NAME.toString(), StringArgumentType.word())
                                    .executes(ctx -> createMarkedRegion(ctx, getRegionNameArgument(ctx)))));
        }
        return createBuilder;
    }


    private static int createMarkedRegion(CommandContext<CommandSourceStack> ctx, String regionName){
        // WE SELECTION
        // TODO Only enable if WE is loaded
        sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("", "Dummy WE not yet supported"));
        return 0;
    }

    static LiteralArgumentBuilder<CommandSourceStack> buildCreateInLocal() {
        return literal(CREATE_IN)
                .then(Commands.argument(LEVEL.toString(), IdentifierArgument.id())
                        .suggests(new TrackedLevelSuggestionProvider())
                        .then(Commands.argument(CommandConstants.NAME.toString(), StringArgumentType.word())
                                .then(Commands.literal(AreaType.CUBOID.areaType)
                                        .then(Commands.argument(POS1.toString(), BlockPosArgument.blockPos())
                                                .then(Commands.argument(POS2.toString(), BlockPosArgument.blockPos())
                                                        .executes(ctx -> {
                                                            var levelId = IdentifierArgument.getId(ctx, LEVEL.toString());
                                                            var name = getRegionNameArgument(ctx);
                                                            var area = new CuboidArea(BlockPosArgument.getSpawnablePos(ctx, POS1.toString()), BlockPosArgument.getSpawnablePos(ctx, POS2.toString()));
                                                            return createRegionIn(ctx, levelId, name, area);
                                                        })
                                                ))
                                )
                                .then(Commands.literal(AreaType.SPHERE.areaType)
                                        .then(Commands.argument(CENTER_POS.toString(), BlockPosArgument.blockPos())
                                                .then(Commands.argument(RADIUS.toString(), IntegerArgumentType.integer(0))
                                                        .executes(ctx -> {
                                                            var levelId = IdentifierArgument.getId(ctx, LEVEL.toString());
                                                            var name = getRegionNameArgument(ctx);
                                                            var area = new SphereArea(BlockPosArgument.getSpawnablePos(ctx, CENTER_POS.toString()), IntegerArgumentType.getInteger(ctx, RADIUS.toString()));
                                                            return createRegionIn(ctx, levelId, name, area);
                                                        })
                                                ))
                                )
                        ))
                ;
    }

    // TODO figure out how to handle this properly
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

    public static int trackLevel(CommandContext<CommandSourceStack> ctx, ServerLevel level) {
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

    private static int promptDimensionRegionList(CommandContext<CommandSourceStack> ctx, LevelData levelData, int pageNo) {
        List<IProtectedRegion> regionsForDim = levelData.getLocalList().stream()
                .map(region -> (IProtectedRegion) region)
                .sorted(Comparator.comparing(IProtectedRegion::getName))
                .toList();
        try {
            int paginationSize = Services.REGION_CONFIG.getPaginationSize();
            RegionsInDimensionPagination childRegionPagination = new RegionsInDimensionPagination(levelData, regionsForDim, pageNo, paginationSize);
            MultiLineMessage.send(ctx.getSource(), childRegionPagination);
        } catch (InvalidPageNumberException e) {
            sendError(ctx.getSource(), e.getError());
            return -1;
        }
        return 0;
    }

    private static int promptHelp(CommandSourceStack src) {
        var versionCopyLink = buildVersionCopyLink(VERSION);
        sendCmdFeedback(src, buildHeader(Component.translatableWithFallback("help.header", "== Yet Another World Protector ==")));
        MutableComponent versionDisclaimer = Component.translatableWithFallback("help.tooltip.version.info.disclaimer", "Disclaimer: This version of YAWP is on the cutting edge of development.");
        MutableComponent versionInfo = Component.translatableWithFallback("help.tooltip.version.info",  "You are running YAWP '%s'. Please provide the version info when requesting help.", versionCopyLink);
        MutableComponent wikiInfo = Component.translatableWithFallback("help.tooltip.wiki.info", "The wiki is likely to be outdated in comparison of the features present in this version. Please visit the discord for help/questions.");
        sendCmdFeedback(src, versionDisclaimer);
        sendCmdFeedback(src, versionInfo);
        sendCmdFeedback(src, wikiInfo);
        sendCmdFeedback(src, Messages.substitutable(" => %s", buildDiscordLink()));
        sendCmdFeedback(src, Messages.substitutable(" => %s", buildWikiLink()));
        sendCmdFeedback(src, Messages.substitutable(" => %s", buildHelpStartComponent()));
        return 0;
    }
}
