package de.z0rdak.yawp.commands;

import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.api.core.IDimensionRegionApi;
import de.z0rdak.yawp.api.core.RegionManager;
import de.z0rdak.yawp.api.core.RegionVisualization;
import de.z0rdak.yawp.commands.arguments.region.RegionArgumentType;
import de.z0rdak.yawp.core.area.DisplayType;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.data.region.DimensionRegionCache;
import de.z0rdak.yawp.platform.Services;
import de.z0rdak.yawp.util.text.messages.multiline.MultiLineMessage;
import de.z0rdak.yawp.util.text.messages.pagination.InvalidPageNumberException;
import de.z0rdak.yawp.util.text.messages.pagination.RegionsInDimensionPagination;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.level.Level;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.api.MessageSender.sendError;
import static de.z0rdak.yawp.api.commands.CommandConstants.*;
import static de.z0rdak.yawp.commands.arguments.ArgumentUtil.*;


class PlayerCommands {

    private PlayerCommands() {
    }

    static LiteralArgumentBuilder<CommandSourceStack> build() {
        return literal(SHOW)
                .then(literal(HULL)
                        .then(Commands.argument(LOCAL.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> RegionArgumentType.region().listSuggestionsIn(ctx, builder, ctx.getSource().getLevel()))
                                .executes(ctx -> RegionCommands.showRegionArea(ctx, getRegionIn(ctx, ctx.getSource().getLevel()), DisplayType.HULL)))
                )
                .then(literal(FRAME)
                        .then(Commands.argument(LOCAL.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> RegionArgumentType.region().listSuggestionsIn(ctx, builder, ctx.getSource().getLevel()))
                                .executes(ctx -> RegionCommands.showRegionArea(ctx, getRegionIn(ctx, ctx.getSource().getLevel()), DisplayType.FRAME)))
                )
                .then(literal(HIDE)
                        .then(Commands.argument(LOCAL.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> RegionArgumentType.region().listSuggestionsIn(ctx, builder, ctx.getSource().getLevel()))
                                .executes(ctx -> RegionCommands.hideRegion(ctx, getRegionIn(ctx, ctx.getSource().getLevel()))))
                )
                .then(literal(HIDE_ALL).executes(PlayerCommands::hideRegions))
                .then(literal(HIDE_NEAR)
                        .executes(ctx -> hideRegionsAroundPlayer(ctx, 192))
                        .then(Commands.argument(RADIUS.toString(), IntegerArgumentType.integer(10, 800))
                                .executes(ctx -> hideRegionsAroundPlayer(ctx, IntegerArgumentType.getInteger(ctx, RADIUS.toString())))))
                .then(literal(SHOW_NEAR)
                        .then(literal(LIST)
                                .executes(ctx -> promptRegionsAroundPlayer(ctx, 192))
                                .then(Commands.argument(RADIUS.toString(), IntegerArgumentType.integer(10, 800))
                                        .executes(ctx -> promptRegionsAroundPlayer(ctx, IntegerArgumentType.getInteger(ctx, RADIUS.toString())))))
                        .then(literal(HULL)
                                .executes(ctx -> showRegionsAroundPlayer(ctx, DisplayType.HULL, 100))
                                .then(Commands.argument(RADIUS.toString(), IntegerArgumentType.integer(10, 800))
                                        .executes(ctx -> showRegionsAroundPlayer(ctx, DisplayType.HULL, IntegerArgumentType.getInteger(ctx, RADIUS.toString())))))
                        .then(literal(FRAME)
                                .executes(ctx -> showRegionsAroundPlayer(ctx, DisplayType.FRAME, 100))
                                .then(Commands.argument(RADIUS.toString(), IntegerArgumentType.integer(10, 800))
                                        .executes(ctx -> showRegionsAroundPlayer(ctx, DisplayType.FRAME, IntegerArgumentType.getInteger(ctx, RADIUS.toString())))))
                );
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
        Level level = ctx.getSource().getLevel();
        ServerPlayer player = ctx.getSource().getPlayerOrException();
        Optional<IDimensionRegionApi> maybeDimRegionApi = RegionManager.get().getDimRegionApi(level.dimension());
        if (maybeDimRegionApi.isPresent()) {
            IDimensionRegionApi dimRegionApi = maybeDimRegionApi.get();
            List<IMarkableRegion> regionsAround = dimRegionApi.getRegionsAround(player.blockPosition(), blockRadius);
            regionsAround.forEach(region -> {
                RegionVisualization.show(level, region, displayType);
            });
            // TODO: cmd feedback
            return 0;
        } else {
            return -1;
        }
    }

    private static int hideRegions(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        Level level = ctx.getSource().getLevel();
        ServerPlayer player = ctx.getSource().getPlayerOrException();
        Optional<IDimensionRegionApi> maybeDimRegionApi = RegionManager.get().getDimRegionApi(level.dimension());
        if (maybeDimRegionApi.isPresent()) {
            IDimensionRegionApi dimRegionApi = maybeDimRegionApi.get();
            List<IMarkableRegion> regionsAround = dimRegionApi.getAllLocalRegions().stream().toList();
            regionsAround.forEach(region -> RegionVisualization.hide(level, region));
            // TODO: cmd feedback
            return 0;
        } else {
            return -1;
        }
    }

    private static int hideRegionsAroundPlayer(CommandContext<CommandSourceStack> ctx, int blockRadius) throws CommandSyntaxException {
        Level level = ctx.getSource().getLevel();
        ServerPlayer player = ctx.getSource().getPlayerOrException();
        Optional<IDimensionRegionApi> maybeDimRegionApi = RegionManager.get().getDimRegionApi(level.dimension());
        if (maybeDimRegionApi.isPresent()) {
            IDimensionRegionApi dimRegionApi = maybeDimRegionApi.get();
            List<IMarkableRegion> regionsAround = dimRegionApi.getRegionsAround(player.blockPosition(), blockRadius);
            regionsAround.forEach(region -> RegionVisualization.hide(level, region));
            // TODO: cmd feedback
            return 0;
        } else {
            return -1;
        }
    }
}
