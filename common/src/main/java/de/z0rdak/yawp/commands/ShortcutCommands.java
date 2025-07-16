package de.z0rdak.yawp.commands;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.api.commands.CommandConstants;
import de.z0rdak.yawp.api.core.IDimensionRegionApi;
import de.z0rdak.yawp.api.core.RegionManager;
import de.z0rdak.yawp.api.visualization.VisualizationManager;
import de.z0rdak.yawp.commands.arguments.region.ContainingOwnedRegionArgumentType;
import de.z0rdak.yawp.commands.arguments.region.RegionArgumentType;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.area.DisplayType;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.platform.Services;
import de.z0rdak.yawp.util.text.messages.multiline.MultiLineMessage;
import de.z0rdak.yawp.util.text.messages.pagination.InvalidPageNumberException;
import de.z0rdak.yawp.util.text.messages.pagination.RegionsInDimensionPagination;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.SharedSuggestionProvider;
import net.minecraft.commands.arguments.coordinates.BlockPosArgument;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.level.Level;

import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.api.MessageSender.sendError;
import static de.z0rdak.yawp.api.commands.CommandConstants.*;
import static de.z0rdak.yawp.commands.DimensionCommands.*;
import static de.z0rdak.yawp.commands.RegionCommands.*;
import static de.z0rdak.yawp.commands.arguments.ArgumentUtil.*;


class ShortcutCommands {

    private ShortcutCommands() {
    }

    static LiteralArgumentBuilder<CommandSourceStack> buildInfoLocal() {
        return literal(INFO)
                    .then(Commands.argument(LOCAL.toString(), StringArgumentType.word())
                            .suggests((ctx, builder) -> RegionArgumentType.region().listSuggestionsIn(ctx, builder, ctx.getSource().getLevel()))
                            .executes(ctx -> CommandUtil.promptRegionInfo(ctx, getRegionIn(ctx, ctx.getSource().getLevel())))
                    );
    }

    static LiteralArgumentBuilder<CommandSourceStack> buildDeleteLocal() {
        return literal(DELETE)
                    .then(Commands.argument(LOCAL.toString(), StringArgumentType.word())
                            .suggests((ctx, builder) -> RegionArgumentType.region().listSuggestionsIn(ctx, builder, ctx.getSource().getLevel()))
                            .executes(ctx -> DimensionCommands.deleteRegion(ctx, getRegionIn(ctx, ctx.getSource().getLevel())))
                    );
    }

    static LiteralArgumentBuilder<CommandSourceStack> buildCreateLocal() {
        return literal(CREATE)
                .then(Commands.argument(CommandConstants.NAME.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(Collections.singletonList(getRandomExample()), builder))
                                .then(Commands.literal(AreaType.CUBOID.areaType)
                                        .then(Commands.argument(POS1.toString(), BlockPosArgument.blockPos())
                                                .then(Commands.argument(POS2.toString(), BlockPosArgument.blockPos())
                                                        .executes(ctx -> createCuboidRegion(ctx, getRegionNameArgument(ctx),
                                                                BlockPosArgument.getSpawnablePos(ctx, POS1.toString()),
                                                                BlockPosArgument.getSpawnablePos(ctx, POS2.toString()), null))
                                                        .then(Commands.argument(CommandConstants.PARENT.toString(), StringArgumentType.word())
                                                                .suggests((ctx, builder) -> ContainingOwnedRegionArgumentType.owningRegions().listSuggestions(ctx, builder))
                                                                .executes(ctx -> createCuboidRegion(ctx, getRegionNameArgument(ctx),
                                                                        BlockPosArgument.getSpawnablePos(ctx, POS1.toString()),
                                                                        BlockPosArgument.getSpawnablePos(ctx, POS2.toString()), getContainingOwnedRegionArgument(ctx))))))
                                )
                                .then(Commands.literal(AreaType.SPHERE.areaType)
                                        .then(Commands.argument(CENTER_POS.toString(), BlockPosArgument.blockPos())
                                                .then(Commands.argument(RADIUS.toString(), IntegerArgumentType.integer(0))
                                                        .executes(ctx -> createSphereRegion(ctx, getRegionNameArgument(ctx),
                                                                BlockPosArgument.getSpawnablePos(ctx, CENTER_POS.toString()),
                                                                IntegerArgumentType.getInteger(ctx, RADIUS.toString()), null))
                                                        .then(Commands.argument(CommandConstants.PARENT.toString(), StringArgumentType.word())
                                                                .suggests((ctx, builder) -> ContainingOwnedRegionArgumentType.owningRegions().listSuggestions(ctx, builder))
                                                                .executes(ctx -> createSphereRegion(ctx, getRegionNameArgument(ctx),
                                                                        BlockPosArgument.getSpawnablePos(ctx, CENTER_POS.toString()),
                                                                        IntegerArgumentType.getInteger(ctx, RADIUS.toString()), getContainingOwnedRegionArgument(ctx))))))
                                )
                        );
    }

    // TODO: HIDE LOCAL HIERARCHY/INTERSECTING
    static LiteralArgumentBuilder<CommandSourceStack> buildHide() {
        return literal(HIDE)
                .then(literal(LOCAL)
                        .then(Commands.argument(LOCAL.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> RegionArgumentType.region().listSuggestionsIn(ctx, builder, ctx.getSource().getLevel()))
                                .executes(ctx -> hideRegion(ctx, getRegionIn(ctx, ctx.getSource().getLevel()), DisplayType.FRAME))
                                .then(Commands.argument(STYLE.toString(), StringArgumentType.word())
                                        .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(DisplayType.entries(), builder))
                                        .executes(ctx -> hideRegion(ctx, getRegionIn(ctx, ctx.getSource().getLevel()), getDisplayTypeArgument(ctx)))
                                )
                        )
                ).then(buildHideAll())
                .then(buildHideNear());
    }

    private static LiteralArgumentBuilder<CommandSourceStack> buildHideAll() {
        return literal(ALL)
                .executes(ShortcutCommands::hideRegions)
                .then(Commands.argument(UNTRACKED.toString(), BoolArgumentType.bool())
                        .executes(ctx -> hideRegions(ctx, BoolArgumentType.getBool(ctx, UNTRACKED.toString())))
                );
    }

    private static LiteralArgumentBuilder<CommandSourceStack> buildHideNear() {
        return literal(NEAR)
                .executes(ctx -> hideRegionsAroundPlayer(ctx, 192))
                .then(Commands.argument(RADIUS.toString(), IntegerArgumentType.integer(0, 800))
                        .executes(ctx -> hideRegionsAroundPlayer(ctx, IntegerArgumentType.getInteger(ctx, RADIUS.toString())))
                );
    }

    private static LiteralArgumentBuilder<CommandSourceStack> buildShowNear() {
        return literal(NEAR)
                // TODO: Region pagination list something thing
                //.then(literal(LIST)
                //        .executes(ctx -> promptRegionsAroundPlayer(ctx, 192))
                //        .then(Commands.argument(RADIUS.toString(), IntegerArgumentType.integer(10, 800))
                //                .executes(ctx -> promptRegionsAroundPlayer(ctx, IntegerArgumentType.getInteger(ctx, RADIUS.toString())))))
                //.then(literal(DISPLAY)
                .executes(ctx -> showRegionsAroundPlayer(ctx, DisplayType.FRAME, 192))
                .then(Commands.argument(RADIUS.toString(), IntegerArgumentType.integer(0, 800))
                                .executes(ctx -> showRegionsAroundPlayer(ctx, DisplayType.FRAME, IntegerArgumentType.getInteger(ctx, RADIUS.toString())))
                                .then(Commands.argument(STYLE.toString(), StringArgumentType.word())
                                        .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(DisplayType.entries(), builder))
                                        .executes(ctx -> showRegionsAroundPlayer(ctx, getDisplayTypeArgument(ctx), IntegerArgumentType.getInteger(ctx, RADIUS.toString())))
                                )
                );
    }

    // TODO: SHOW LOCAL HIERARCHY/INTERSECTING
    static LiteralArgumentBuilder<CommandSourceStack> buildShow() {
        return literal(SHOW)
                .then(literal(LOCAL)
                        .then(Commands.argument(LOCAL.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> RegionArgumentType.region().listSuggestionsIn(ctx, builder, ctx.getSource().getLevel()))
                                .executes(ctx -> showRegion(ctx, getRegionIn(ctx, ctx.getSource().getLevel()), DisplayType.FRAME))
                                .then(Commands.argument(STYLE.toString(), StringArgumentType.word())
                                        .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(DisplayType.entries(), builder))
                                        .executes(ctx -> showRegion(ctx, getRegionIn(ctx, ctx.getSource().getLevel()), getDisplayTypeArgument(ctx)))
                                )
                        )
                )
                .then(buildShowNear());
    }

    private static int promptRegionsAroundPlayer(CommandContext<CommandSourceStack> ctx, int blockRadius) throws CommandSyntaxException {
        Level level = ctx.getSource().getLevel();
        ServerPlayer player = ctx.getSource().getPlayerOrException();

        Optional<IDimensionRegionApi> maybeDimRegionApi = RegionManager.get().getDimRegionApi(level.dimension());
        if (maybeDimRegionApi.isPresent()) {
            IDimensionRegionApi dimRegionApi = maybeDimRegionApi.get();
            List<IProtectedRegion> regionsAround = dimRegionApi.getRegionsAround(player.blockPosition(), blockRadius)
                    .stream()
                    .map(r -> (IProtectedRegion)r)
                    .collect(Collectors.toList());
            try {
                // TODO: Build own RegionList Pagination
                int paginationSize = Services.REGION_CONFIG.getPaginationSize();
                RegionsInDimensionPagination childRegionPagination = new RegionsInDimensionPagination(dimRegionApi.getCache(), regionsAround, 0, paginationSize);
                MultiLineMessage.send(ctx.getSource(), childRegionPagination);
            } catch (InvalidPageNumberException e) {
                sendError(ctx.getSource(), e.getError());
                return -1;
            }
            return 0;
        } else {
            return -1;
        }
    }

    private static int showRegionsAroundPlayer(CommandContext<CommandSourceStack> ctx, DisplayType displayType, int blockRadius) throws CommandSyntaxException {
        ServerPlayer player = ctx.getSource().getPlayerOrException();
        VisualizationManager.showRegionsAround(player, blockRadius, displayType);

        Level level = player.getCommandSenderWorld();
        BlockPos playerPos = player.blockPosition();
        Optional<IDimensionRegionApi> maybeApi = RegionManager.get().getDimRegionApi(level.dimension());
        if (maybeApi.isPresent()) {
            var dimApi = maybeApi.get();
            List<IMarkableRegion> regionsAround = dimApi.getRegionsAround(playerPos, blockRadius);
            // TODO: build pagination list of regions visualized and prompt info links for them
        }
        return 0;
    }

    private static int hideRegions(CommandContext<CommandSourceStack> ctx) {
        return hideRegions(ctx, false);
    }

    private static int hideRegions(CommandContext<CommandSourceStack> ctx, boolean untracked) {
        VisualizationManager.hideAllRegions(ctx.getSource().getLevel(), untracked);
        return 0;
    }

    private static int hideRegionsAroundPlayer(CommandContext<CommandSourceStack> ctx, int blockRadius) throws CommandSyntaxException {
        ServerPlayer player = ctx.getSource().getPlayerOrException();
        VisualizationManager.hideRegionsAround(player, blockRadius);
        return 0;
    }
}
