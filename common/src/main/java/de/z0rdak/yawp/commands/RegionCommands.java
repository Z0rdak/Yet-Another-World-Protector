package de.z0rdak.yawp.commands;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.api.core.RegionManager;
import de.z0rdak.yawp.api.visualization.VisualizationManager;
import de.z0rdak.yawp.api.events.region.RegionEvent;
import de.z0rdak.yawp.commands.arguments.ArgumentUtil;
import de.z0rdak.yawp.commands.arguments.region.AddRegionChildArgumentType;
import de.z0rdak.yawp.commands.arguments.region.RegionArgumentType;
import de.z0rdak.yawp.commands.arguments.region.RemoveRegionChildArgumentType;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.area.*;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.core.region.RegionType;
import de.z0rdak.yawp.data.region.DimensionRegionCache;
import de.z0rdak.yawp.data.region.RegionDataManager;
import de.z0rdak.yawp.platform.Services;
import de.z0rdak.yawp.util.ChatLinkBuilder;
import de.z0rdak.yawp.util.LocalRegions;
import de.z0rdak.yawp.util.text.messages.multiline.MultiLineMessage;
import de.z0rdak.yawp.util.text.messages.pagination.InvalidPageNumberException;
import de.z0rdak.yawp.util.text.messages.pagination.TeleportAnchorPagination;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.SharedSuggestionProvider;
import net.minecraft.commands.arguments.DimensionArgument;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.commands.arguments.ResourceLocationArgument;
import net.minecraft.commands.arguments.coordinates.BlockPosArgument;
import net.minecraft.core.BlockPos;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.*;
import net.minecraft.world.level.block.AirBlock;
import net.minecraft.world.level.block.Block;
import org.apache.commons.lang3.StringUtils;

import java.util.*;

import static de.z0rdak.yawp.api.MessageSender.sendError;
import static de.z0rdak.yawp.api.commands.CommandConstants.*;
import static de.z0rdak.yawp.commands.CommandUtil.*;
import static de.z0rdak.yawp.commands.arguments.ArgumentUtil.*;
import static de.z0rdak.yawp.constants.Constants.MAX_BUILD_LIMIT;
import static de.z0rdak.yawp.constants.Constants.MIN_BUILD_LIMIT;
import static de.z0rdak.yawp.util.ChatComponentBuilder.shortBlockPos;
import static de.z0rdak.yawp.util.ChatLinkBuilder.*;
import static de.z0rdak.yawp.api.MessageSender.sendCmdFeedback;


class RegionCommands {

    private RegionCommands() {
    }

    static LiteralArgumentBuilder<CommandSourceStack> build() {
        return literal(LOCAL)
                .then(Commands.argument(DIM.toString(), DimensionArgument.dimension())
                        .then(Commands.argument(LOCAL.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> RegionArgumentType.region().listSuggestions(ctx, builder))
                                .executes(ctx -> promptRegionInfo(ctx, getRegionArgument(ctx)))
                                .then(literal(INFO)
                                        .executes(ctx -> promptRegionInfo(ctx, getRegionArgument(ctx))))
                                .then(buildClearSubCommand(ArgumentUtil::getRegionArgument))
                                .then(buildAddSubCommand(ArgumentUtil::getRegionArgument))
                                .then(buildListSubCommand(ArgumentUtil::getRegionArgument))
                                .then(buildRemoveSubCommand(ArgumentUtil::getRegionArgument))
                                .then(buildCopySubCommand(ArgumentUtil::getRegionArgument))
                                .then(literal(DELETE)
                                        .executes(ctx -> DimensionCommands.attemptDeleteRegion(ctx, getDimCacheArgument(ctx), getRegionArgument(ctx)))
                                        .then(literal(FOR_SURE)
                                                .executes(ctx -> DimensionCommands.deleteRegion(ctx, getDimCacheArgument(ctx), getRegionArgument(ctx)))))
                                .then(literal(ADD).then(literal(CHILD)
                                                .then(Commands.argument(CHILD.toString(), StringArgumentType.word())
                                                        .suggests((ctx, builder) -> AddRegionChildArgumentType.potentialChildRegions().listSuggestions(ctx, builder))
                                                        .executes(ctx -> addChildren(ctx, getRegionArgument(ctx), getChildRegionArgument(ctx))))))
                                .then(literal(REMOVE).then(literal(CHILD)
                                                .then(Commands.argument(CHILD.toString(), StringArgumentType.word())
                                                        .suggests((ctx, builder) -> RemoveRegionChildArgumentType.childRegions().listSuggestions(ctx, builder))
                                                        .executes(ctx -> removeChildren(ctx, getDimCacheArgument(ctx), getRegionArgument(ctx), getChildRegionArgument(ctx))))))
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
                                        .then(literal(PRIORITY)
                                                .then(Commands.argument(PRIORITY.toString(), IntegerArgumentType.integer())
                                                        .executes(ctx -> setPriority(ctx, getRegionArgument(ctx), getPriorityArgument(ctx))))
                                                .then(literal(INC)
                                                        .then(Commands.argument(PRIORITY.toString(), IntegerArgumentType.integer())
                                                                .executes(ctx -> setPriority(ctx, getRegionArgument(ctx), getPriorityArgument(ctx), 1))))
                                                .then(literal(DEC)
                                                        .then(Commands.argument(PRIORITY.toString(), IntegerArgumentType.integer())
                                                                .executes(ctx -> setPriority(ctx, getRegionArgument(ctx), getPriorityArgument(ctx), -1))))))
                                .then(literal(AREA)
                                        .executes(ctx -> promptRegionAreaInfo(ctx, getRegionArgument(ctx)))
                                        .then(literal(SET)
                                                .then(Commands.literal(AreaType.CUBOID.areaType)
                                                        .then(Commands.argument(POS1.toString(), BlockPosArgument.blockPos())
                                                                .then(Commands.argument(POS2.toString(), BlockPosArgument.blockPos())
                                                                        .executes(ctx -> setCuboidArea(ctx, getRegionArgument(ctx), BlockPosArgument.getSpawnablePos(ctx, POS1.toString()), BlockPosArgument.getSpawnablePos(ctx, POS2.toString()))))))
                                                .then(Commands.literal(AreaType.SPHERE.areaType)
                                                        .then(Commands.argument(CENTER_POS.toString(), BlockPosArgument.blockPos())
                                                                .then(Commands.argument(RADIUS.toString(), IntegerArgumentType.integer(0))
                                                                        .executes(ctx -> setSphereArea(ctx, getRegionArgument(ctx), BlockPosArgument.getSpawnablePos(ctx, CENTER_POS.toString()), IntegerArgumentType.getInteger(ctx, RADIUS.toString()))))))
                                        )
                                        .then(literal(EXPAND)
                                                .then(Commands.literal(AreaType.CUBOID.areaType)
                                                        .executes(ctx -> expandCuboid(ctx, getRegionArgument(ctx), MIN_BUILD_LIMIT, MAX_BUILD_LIMIT))
                                                        .then(Commands.argument(Y_MIN.toString(), IntegerArgumentType.integer(MIN_BUILD_LIMIT, MAX_BUILD_LIMIT))
                                                                .then(Commands.argument(Y_MAX.toString(), IntegerArgumentType.integer(MIN_BUILD_LIMIT, MAX_BUILD_LIMIT))
                                                                        .executes(ctx -> expandCuboid(ctx, getRegionArgument(ctx), IntegerArgumentType.getInteger(ctx, Y_MIN.toString()), IntegerArgumentType.getInteger(ctx, Y_MAX.toString()))))))
                                                .then(Commands.literal(AreaType.SPHERE.areaType)
                                                        .executes(ctx -> expandSphere(ctx, getRegionArgument(ctx), 1))
                                                        .then(Commands.argument(EXPANSION.toString(), IntegerArgumentType.integer())
                                                               .executes(ctx -> expandSphere(ctx, getRegionArgument(ctx), IntegerArgumentType.getInteger(ctx, EXPANSION.toString())))))                                        )
                                )
                                .then(literal(LIST)
                                        .then(literal(TP_ANCHOR)
                                                .executes(ctx -> promptTeleportAnchorPagination(ctx, getRegionArgument(ctx), 0))
                                                .then(Commands.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                                        .executes(ctx -> promptTeleportAnchorPagination(ctx, getRegionArgument(ctx), getPageNoArgument(ctx)))
                                                )
                                        )
                                )
                                .then(literal(ADD)
                                        .then(literal(TP_ANCHOR)
                                                .then(Commands.argument(NAME.toString(), StringArgumentType.word())
                                                        .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(List.of("tpAnchor-name"), builder))
                                                        .then(Commands.argument(TP_ANCHOR.toString(), BlockPosArgument.blockPos())
                                                                .executes(ctx -> updateTeleportAnchor(ctx, getRegionArgument(ctx), getTeleportAnchorPosArgument(ctx), getTeleportAnchorNameArgument(ctx)))
                                                        )
                                                )
                                        )
                                )
                                .then(literal(REMOVE)
                                        .then(literal(TP_ANCHOR)
                                                .then(Commands.argument(NAME.toString(), StringArgumentType.word())
                                                        .executes(ctx -> removeTeleportAnchor(ctx, getRegionArgument(ctx), getTeleportAnchorNameArgument(ctx)))
                                                )
                                        )
                                )
                                .then(literal(TP_ANCHOR)
                                        .then(literal(RENAME)
                                                .then(Commands.argument(NAME.toString(), StringArgumentType.word())
                                                        .then(Commands.argument(RENAME.toString(), StringArgumentType.word())
                                                                .executes(ctx -> renameTeleportAnchor(ctx, getRegionArgument(ctx), getTeleportAnchorNameArgument(ctx), getNewTeleportAnchorNameArgument(ctx)))
                                                        )
                                                )
                                        )
                                        .then(literal(SET)
                                                .then(Commands.argument(NAME.toString(), StringArgumentType.word())
                                                        .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(List.of("tpAnchor-name"), builder))
                                                        .then(Commands.argument(TP_ANCHOR.toString(), BlockPosArgument.blockPos())
                                                                .executes(ctx -> updateTeleportAnchor(ctx, getRegionArgument(ctx), getTeleportAnchorPosArgument(ctx), getTeleportAnchorNameArgument(ctx)))
                                                        )
                                                )
                                        )
                                        .then(literal(HIDE)
                                                .then(Commands.argument(TP_ANCHOR.toString(), StringArgumentType.word())
                                                        .executes(ctx -> hideTpAnchor(ctx, getRegionArgument(ctx), StringArgumentType.getString(ctx, TP_ANCHOR.toString())))
                                                )
                                        )
                                        .then(literal(SHOW)
                                                .then(Commands.argument(TP_ANCHOR.toString(), StringArgumentType.word())
                                                        .executes(ctx -> showTpAnchor(ctx, getRegionArgument(ctx), StringArgumentType.getString(ctx, TP_ANCHOR.toString())))
                                                )
                                        )
                                        .then(literal(TELEPORT)
                                                .then(Commands.argument(TP_ANCHOR.toString(), StringArgumentType.word())
                                                        .executes(ctx -> teleport(ctx, getRegionArgument(ctx), StringArgumentType.getString(ctx, TP_ANCHOR.toString())))
                                                        .then(Commands.argument(PLAYER.toString(), EntityArgument.player())
                                                                .executes(ctx -> teleport(ctx, getRegionArgument(ctx), StringArgumentType.getString(ctx, TP_ANCHOR.toString()), getPlayerArgument(ctx)))
                                                        )
                                                )
                                        )
                                )
                                .then(literal(RENAME)
                                        .then(Commands.argument(NAME.toString(), StringArgumentType.word())
                                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(Collections.singletonList(getRegionArgument(ctx).getName()), builder))
                                                .executes(ctx -> renameRegion(ctx, getRegionArgument(ctx), getRegionNameArgument(ctx), getDimCacheArgument(ctx)))
                                        )
                                )
                                .then(literal(SHOW)
                                        .executes(ctx -> promptVisualizationOptions(ctx, getRegionArgument(ctx)))
                                        .then(literal(LOCAL)
                                            .executes(ctx -> showRegion(ctx, getRegionArgument(ctx), DisplayType.FRAME))
                                            .then(Commands.argument(STYLE.toString(), StringArgumentType.word())
                                                    .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(DisplayType.entries(), builder))
                                                    .executes(ctx -> showRegion(ctx, getRegionArgument(ctx), getDisplayTypeArgument(ctx)))
                                                    .then(Commands.argument(BLOCK.toString(), ResourceLocationArgument.id())
                                                            .executes(ctx -> showRegion(ctx, getRegionArgument(ctx),
                                                                    getDisplayTypeArgument(ctx),
                                                                    getDisplayBlockArgument(ctx)))
                                                            .then(Commands.argument(GLOW.toString(), BoolArgumentType.bool())
                                                                    .executes(ctx -> showRegion(ctx, getRegionArgument(ctx),
                                                                            getDisplayTypeArgument(ctx),
                                                                            getDisplayBlockArgument(ctx),
                                                                            getDisplayGlowArgument(ctx)))
                                                                    .then(Commands.argument(LIGHT_LEVEL.toString(), IntegerArgumentType.integer(0, 15))
                                                                            .executes(ctx -> showRegion(ctx, getRegionArgument(ctx),
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
                                                .executes(ctx -> showRegionHierarchy(ctx, getRegionArgument(ctx), DisplayType.FRAME, false))
                                                .then(Commands.argument(STYLE.toString(), StringArgumentType.word())
                                                        .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(DisplayType.entries(), builder))
                                                        .executes(ctx -> showRegionHierarchy(ctx, getRegionArgument(ctx), getDisplayTypeArgument(ctx), false))
                                                        .then(Commands.argument(RECURSIVE.toString(), BoolArgumentType.bool())
                                                                .executes(ctx -> showRegionHierarchy(ctx, getRegionArgument(ctx), getDisplayTypeArgument(ctx), BoolArgumentType.getBool(ctx, RECURSIVE.toString())))
                                                        )
                                                )
                                        )
                                        .then(literal(INTERSECTING)
                                                .executes(ctx -> showRegionsIntersecting(ctx, getRegionArgument(ctx), DisplayType.FRAME))
                                                .then(Commands.argument(STYLE.toString(), StringArgumentType.word())
                                                        .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(DisplayType.entries(), builder))
                                                        .executes(ctx -> showRegionsIntersecting(ctx, getRegionArgument(ctx), getDisplayTypeArgument(ctx)))
                                                )
                                        )
                                )
                                .then(literal(HIDE)
                                        .then(literal(LOCAL)
                                                .executes(ctx -> hideRegion(ctx, getRegionArgument(ctx), DisplayType.FRAME))
                                                .then(Commands.argument(STYLE.toString(), StringArgumentType.word())
                                                        .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(DisplayType.entries(), builder))
                                                        .executes(ctx -> hideRegion(ctx, getRegionArgument(ctx), getDisplayTypeArgument(ctx)))
                                                )
                                        )
                                        .then(literal(HIERARCHY)
                                                .executes(ctx -> hideRegionHierarchy(ctx, getRegionArgument(ctx), DisplayType.FRAME, false))
                                                .then(Commands.argument(STYLE.toString(), StringArgumentType.word())
                                                        .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(DisplayType.entries(), builder))
                                                        .executes(ctx -> hideRegionHierarchy(ctx, getRegionArgument(ctx), getDisplayTypeArgument(ctx), false))
                                                        .then(Commands.argument(RECURSIVE.toString(), BoolArgumentType.bool())
                                                                .executes(ctx -> hideRegionHierarchy(ctx, getRegionArgument(ctx), getDisplayTypeArgument(ctx), BoolArgumentType.getBool(ctx, RECURSIVE.toString())))
                                                        )
                                                )
                                        )
                                        .then(literal(INTERSECTING)
                                                .executes(ctx -> hideRegionsIntersecting(ctx, getRegionArgument(ctx), DisplayType.FRAME))
                                                .then(Commands.argument(STYLE.toString(), StringArgumentType.word())
                                                        .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(DisplayType.entries(), builder))
                                                        .executes(ctx -> hideRegionsIntersecting(ctx, getRegionArgument(ctx), getDisplayTypeArgument(ctx)))
                                                )
                                        )
                                )
                                .then(literal(DISPLAY)
                                        .executes(ctx -> promptDisplaySettings(ctx, getRegionArgument(ctx)))
                                        .then(literal(BLOCK)
                                                .then(Commands.argument(BLOCK.toString(), ResourceLocationArgument.id())
                                                        .executes(ctx -> setDisplayBlock(ctx, getRegionArgument(ctx), getDisplayBlockArgument(ctx)))
                                                )
                                        )
                                        .then(literal(GLOW)
                                                .then(Commands.argument(GLOW.toString(), BoolArgumentType.bool())
                                                        .executes(ctx -> setDisplayGlow(ctx, getRegionArgument(ctx), getDisplayGlowArgument(ctx)))
                                                )
                                        )
                                        .then(literal(LIGHT_LEVEL)
                                                .then(Commands.argument(LIGHT_LEVEL.toString(), IntegerArgumentType.integer(0, 15))
                                                         .executes(ctx -> setDisplayLightLevel(ctx, getRegionArgument(ctx), IntegerArgumentType.getInteger(ctx, LIGHT_LEVEL.toString())))
                                                )
                                        )
                                        .then(literal(RESET)
                                                .executes(ctx -> resetDisplaySettings(ctx, getRegionArgument(ctx)))
                                        )
                                )
                        )
                );
    }

    private static int setDisplayLightLevel(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, int lightLevel) {
        IMarkableArea area = region.getArea();
        area.getDisplay().setLightLevel(lightLevel);
        RegionManager.get().save();
        // TODO: Trigger update
        VisualizationManager.refreshDisplay(region);
        // TODO: I18n
        sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("Set light level for area display for region %s to '%s'", "Set display light level for %s to '%s'", buildRegionInfoLink(region), lightLevel));
        return 0;
    }

    public static int resetDisplaySettings(CommandContext<CommandSourceStack> ctx, IMarkableRegion region) {
        IMarkableArea area = region.getArea();
        area.getDisplay().setHasGlow(BlockDisplayProperties.DEFAULT_GLOW);
        area.getDisplay().setLightLevel(BlockDisplayProperties.DEFAULT_LIGHT_LEVEL);
        RegionManager.get().save();
        // TODO: Trigger update
        VisualizationManager.refreshDisplay(region);
        // TODO: I18n
        sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("Reset display settings for region %s", "Reset display settings for %s", buildRegionInfoLink(region)));
        return 0;
    }

    public static int setDisplayGlow(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, boolean hasGlow) {
        IMarkableArea area = region.getArea();
        BlockDisplayProperties display = area.getDisplay();
        if (display.hasGlow() != hasGlow) {
            display.setHasGlow(hasGlow);
            RegionManager.get().save();
            // TODO: Trigger update
            VisualizationManager.refreshDisplay(region);
            // TODO: I18n
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("Set display glow effect for region %s to '%s'", "Set display glow effect for %s to '%s'", buildRegionInfoLink(region), Boolean.toString(hasGlow)));
            return 0;
        }
        // else silently just do nothing :-)
        return 1;
    }


    public static int setDisplayBlock(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, ResourceLocation blockRl) {
        Block block = BuiltInRegistries.BLOCK.get(blockRl);
        if (block instanceof AirBlock) {
            // TODO: I18n
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("Not found", "Not found", buildRegionInfoLink(region), blockRl.toString()));
            return -1;
        }
        IMarkableArea area = region.getArea();
        area.getDisplay().setBlockRl(blockRl);
        RegionManager.get().save();
        // TODO: Trigger update
        VisualizationManager.refreshDisplay(region);
        // TODO: I18n
        sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("Set display block for region %s to '%s'", "Set display block for %s to '%s'", buildRegionInfoLink(region), blockRl.toString()));
        return 0;
    }

    private static int expandSphere(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, int expansion) {
        SphereArea expand = SphereArea.expand((SphereArea) region.getArea(), expansion);
        return updateArea(ctx, region, expand);
    }

    private static int setSphereArea(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, BlockPos center, int radius) {
        BlockPos newRadius = center.offset(0, radius, 0);
        return setSphereArea(ctx, region, center, newRadius);
    }

    private static int setSphereArea(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, BlockPos center, BlockPos radiusPos) {
        return updateArea(ctx, region, new SphereArea(center, radiusPos));
    }

    private static int setCuboidArea(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, BlockPos p1, BlockPos p2) {
        return updateArea(ctx, region, new CuboidArea(p1, p2));
    }

    private static int expandCuboid(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, int yMin, int yMax) {
        CuboidArea expand = CuboidArea.expand((CuboidArea) region.getArea(), yMin, yMax);
        return updateArea(ctx, region, expand);
    }

    private static int updateArea(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, IMarkableArea area) {
        try {
            AreaType prevAreaType = region.getArea().getAreaType();
            AreaType newAreaType = area.getAreaType();
            IProtectedRegion parent = region.getParent();
            // TODO: Implement a contains method for regions, with dimensional always returning true if dim is the same
            // IMarkableRegions would use the area contains method

            ServerPlayer player;
            try {
                player = ctx.getSource().getPlayerOrException();
            } catch (CommandSyntaxException e) {
                player = null;
            }
            RegionEvent.UpdateArea updateArea = new RegionEvent.UpdateArea(region, area, player);
            updateArea = Services.EVENT.post(updateArea);
            area = updateArea.markedArea();
            // Note: this check can be remove once the area types are all implemented, it's just here to catch any errors
            switch (newAreaType) {
                case CUBOID:
                case SPHERE:
                    if (parent.getRegionType() == RegionType.DIMENSION) {
                        int newPriority = LocalRegions.ensureHigherRegionPriorityFor(region, Services.REGION_CONFIG.getDefaultPriority());
                        Constants.LOGGER.info("New priority {} for region {}", newPriority, region.getName());
                    }
                    if (parent.getRegionType() == RegionType.LOCAL) {
                        IMarkableRegion localParent = (IMarkableRegion) parent;
                        switch (localParent.getArea().getAreaType()) {
                            case CUBOID:
                            case SPHERE:
                                if (localParent.getArea().containsOther(area)) {
                                    int newPriority = LocalRegions.ensureHigherRegionPriorityFor(region, localParent.getPriority() + 1);
                                    Constants.LOGGER.info("New priority {} for region {}", newPriority, region.getName());
                                } else {
                                    MutableComponent updateAreaFailMsg = Component.translatableWithFallback("cli.msg.info.region.area.area.update.fail.boundaries", "Parent region %s does not fully contain new are for region %s", buildRegionInfoLink(parent), buildRegionInfoLink(region));
                                    sendCmdFeedback(ctx.getSource(), updateAreaFailMsg);
                                    return 1;
                                }
                                break;
                            case CYLINDER:
                            case POLYGON_3D:
                            case PRISM:
                                throw new UnsupportedOperationException("Unsupported area type");
                        }
                    }
                    break;
                case CYLINDER:
                case POLYGON_3D:
                case PRISM:
                    throw new UnsupportedOperationException("Unsupported area type");
            }
            if (prevAreaType != newAreaType) {
                MutableComponent updateAreaFailMsg = Component.translatableWithFallback("cli.msg.info.region.area.update.type.change", "AreaType for %s changed from %s to %s", buildRegionInfoLink(region), prevAreaType, newAreaType);
                sendCmdFeedback(ctx.getSource(), updateAreaFailMsg);
            }
            region.setArea(area);
            RegionDataManager.save();
            MutableComponent updateAreaMsg = Component.translatableWithFallback("cli.msg.info.region.area.area.update", "Updated %s for %s", buildRegionAreaLink(region), buildRegionInfoLink(region));
            sendCmdFeedback(ctx.getSource(), updateAreaMsg);
            return 0;
        } catch (Exception ex) {
            Constants.LOGGER.error("Failed to update area: {}", ex.getMessage());
            return 1;
        }
    }

    private static int renameRegion(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, String regionName, DimensionRegionCache dimCache) {
        if (region.getName().equals(regionName)) {
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.dim.info.region.create.name.no-change", regionName));
            return 1;
        }
        int res = RegionDataManager.get().isValidRegionName(dimCache.getDimensionalRegion().getDim(), regionName);
        if (res == -1) {
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.dim.info.region.create.name.invalid", regionName));
            return res;
        }
        if (res == 1) {
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.dim.info.region.create.name.exists", "Dimension %s already contains region with name %s", dimCache.getDimensionalRegion().getName(), buildRegionInfoLink(dimCache.getRegion(regionName))));
            return res;
        }
        try {
            ServerPlayer player;
            try {
                player = ctx.getSource().getPlayerOrException();
            } catch (CommandSyntaxException e) {
                player = null;
            }
            
            RegionEvent.Rename renameRegion = new RegionEvent.Rename(region, region.getName(), regionName, player);
            if (Services.EVENT.post(renameRegion)) {
                return 1;
            }
            //if (RegionEvents.RENAME_REGION.invoker().renameRegion(renameRegion)) {
            //    return 0;
            //}
            String oldName = region.getName();
            dimCache.renameRegion(region, regionName);
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.dim.info.region.create.name.success", "Changed name of region %s from '%s' to '%s'", buildRegionInfoLink(region), oldName, regionName));
            RegionDataManager.save();
            return 0;
        } catch (IllegalArgumentException ex) {
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.dim.info.region.create.name.exists", "Dimension %s already contains region with name %s", dimCache.getDimensionalRegion().getName(), buildRegionInfoLink(dimCache.getRegion(regionName))));
            return 1;
        }
    }

    // TODO: Test removing child does not set priority correct with overlapping regions
    private static int removeChildren(CommandContext<CommandSourceStack> ctx, DimensionRegionCache dimCache, IProtectedRegion parent, IMarkableRegion child) {
        if (parent.hasChild(child)) {
            parent.removeChild(child);
            dimCache.getDimensionalRegion().addChild(child);
            LocalRegions.ensureLowerRegionPriorityFor(child, Services.REGION_CONFIG.getDefaultPriority());
            RegionDataManager.save();
            MutableComponent parentLink = buildRegionInfoLink(parent);
            MutableComponent notLongerChildLink = buildRegionInfoLink(child);
            MutableComponent dimensionalLink = buildRegionInfoLink(dimCache.getDimensionalRegion());
            MutableComponent undoLink = buildRegionActionUndoLink(ctx.getInput(), REMOVE, ADD);
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.children.remove", "Removed child '%s' from region %s", notLongerChildLink, parentLink).append(" ")
                    .append(undoLink));
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.parent.clear", "Reset default parent for %s back to %s", notLongerChildLink, dimensionalLink));
            return 0;
        }
        // should not happen, due to RemoveRegionChildArgumentType should only provide valid child regions
        return -1;
    }

    private static int addChildren(CommandContext<CommandSourceStack> ctx, IMarkableRegion parent, IMarkableRegion child) {
        boolean currentParentIsNotNullAndDimension = child.getParent() != null && child.getParent().getRegionType() == RegionType.DIMENSION;
        if (!parent.hasChild(child) && currentParentIsNotNullAndDimension) {
            if (!parent.getArea().containsOther(child.getArea())) { // does child fit into parent?
                sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.arg.region.owned.invalid.containment", "Region %s is not suitable as parent for %s (does not fully contain child region)", buildRegionInfoLink(parent), buildRegionInfoLink(child)));
                return -1;
            }
            child.getParent().removeChild(child);
            parent.addChild(child);
            LocalRegions.ensureHigherRegionPriorityFor(child, parent.getPriority() + 1);
            RegionDataManager.save();
            MutableComponent parentLink = buildRegionInfoLink(parent);
            MutableComponent childLink = buildRegionInfoLink(child);
            MutableComponent undoLink = buildRegionActionUndoLink(ctx.getInput(), ADD, REMOVE);
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.children.add", "Added child %s to region %s", childLink, parentLink).append(" ")
                    .append(undoLink));
            return 0;
        }
        // should not happen, due to AddRegionChildArgumentType should only provide valid child regions
        return -1;
    }

    private static int setPriority(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, int priority, int factor) {
        long newValue = (long) region.getPriority() + ((long) priority * factor);
        if (Integer.MAX_VALUE - newValue > 0) {
            return setPriority(ctx, region, (int) newValue);
        } else {
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.warn.region.state.priority.set.invalid", "Unable to change priority for region %s: %s is to high/low", buildRegionInfoLink(region), newValue));
            return -1;
        }
    }

    /**
     * Attempt to set new priority for the given region. <br>
     * Fails if region priority is used by an overlapping region at same hierarchy level.
     */
    private static int setPriority(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, int priority) {
        IProtectedRegion parent = region.getParent();
        if (parent instanceof IMarkableRegion) {
            int parentPriority = ((IMarkableRegion) parent).getPriority();
            if (parentPriority >= priority) {
                MutableComponent updatePriorityFailMsg = Component.translatableWithFallback("cli.msg.info.region.state.priority.set.fail.to-low", "Unable to set priority for region %s. The priority is not higher than its parents priority", buildRegionInfoLink(region));
                sendCmdFeedback(ctx.getSource(), updatePriorityFailMsg);
                return 1;
            }
        }
        boolean existRegionWithSamePriority = LocalRegions.hasAnyRegionWithSamePriority(region, priority);
        if (existRegionWithSamePriority) {
            MutableComponent updatePriorityFailMsg = Component.translatableWithFallback("cli.msg.info.region.state.priority.set.fail.same", "Unable to set priority for region %s. There is already another region with priority %s.", buildRegionInfoLink(region), priority);
            sendCmdFeedback(ctx.getSource(), updatePriorityFailMsg);
            return 1;
        } else {
            int oldPriority = region.getPriority();
            if (oldPriority != priority) {
                region.setPriority(priority);
                RegionDataManager.save();
                MutableComponent undoLink = buildRegionActionUndoLink(ctx.getInput(), String.valueOf(oldPriority), String.valueOf(priority));
                sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.state.priority.set.success", "Changed priority for region %s: %s -> %s",
                                buildRegionInfoLink(region), oldPriority, region.getPriority())
                        .append(" ")
                        .append(undoLink));
                return 0;
            } else {
                sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.state.priority.set.fail.no-change", "Unable to set priority for region %s. The priority is the same.", buildRegionInfoLink(region)));
                return 1;
            }
        }
    }

    public static int showRegion(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, DisplayType displayType) {
        return showRegion(ctx, region, displayType, region.getArea().getDisplay().blockRl());
    }

    public static int showRegion(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, DisplayType displayType, ResourceLocation blockRl) {
        return showRegion(ctx, region, displayType, blockRl, region.getArea().getDisplay().hasGlow());
    }

    public static int showRegion(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, DisplayType displayType, ResourceLocation blockRl, boolean glow) {
        return showRegion(ctx, region, displayType, blockRl, glow, region.getArea().getDisplay().lightLevel());
    }

    public static int showRegion(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, DisplayType displayType, ResourceLocation blockRl, boolean glow, int lightLevel) {
        BlockDisplayProperties displayProperties = new BlockDisplayProperties(blockRl, glow, lightLevel);
        VisualizationManager.show(region, displayType, displayProperties);
        // TODO: Feedback?
        return 0;
    }

    public static int hideRegion(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, DisplayType displayType) {
        VisualizationManager.hide(region, displayType);
        // TODO: Feedback?
        return 0;
    }

    public static int showRegionsIntersecting(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, DisplayType  displayType) {
        VisualizationManager.showIntersecting(region, displayType);
        return 0;
    }

    public static int showRegionHierarchy(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, DisplayType displayType, boolean recursive) {
        VisualizationManager.showHierarchy(region, displayType, recursive);
        return 0;
    }

    public static int hideRegionHierarchy(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, DisplayType displayType, boolean recursive) {
        VisualizationManager.hideHierarchy(region, displayType, recursive);
        return 0;
    }

    public static int hideRegionsIntersecting(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, DisplayType displayType) {
        VisualizationManager.hideIntersecting(region, displayType);
        return 0;
    }

    private static int promptDisplaySettings(CommandContext<CommandSourceStack> ctx, IMarkableRegion region) {
        MultiLineMessage.send(ctx.getSource(), MultiLineMessage.displaySettingsInfo(region));
        return 0;
    }

    private static int promptVisualizationOptions(CommandContext<CommandSourceStack> ctx, IMarkableRegion region) {
        MultiLineMessage.send(ctx.getSource(), MultiLineMessage.visualizationOptions(region));
        return 0;
    }

    private static int promptTeleportAnchorPagination(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, int pageNo) {
        try {
            int paginationSize = Services.REGION_CONFIG.getPaginationSize();
            TeleportAnchorPagination tpAnchorPagination = new TeleportAnchorPagination(region, pageNo, paginationSize);
            MultiLineMessage.send(ctx.getSource(), tpAnchorPagination);
        } catch (InvalidPageNumberException e) {
            sendError(ctx.getSource(), e.getError());
            return -1;
        }
        return 0;
    }

    private static int updateTeleportAnchor(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, BlockPos pos, String name) {
        RegionAnchors tpAnchors = region.getTpAnchors();
        var hasAnchor = tpAnchors.hasAnchor(name);
        if (!hasAnchor && !isValidName(name)) {
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.tp-anchor.fail-msg.invalid-name", "Teleport Anchor name is invalid. Must be alphanumeric and between 3 and 50 letters.", name, buildRegionInfoLink(region)));
            return -1;
        }
        if (!region.getArea().contains(pos)) {
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.tp-anchor.fail-msg.not-contained", "Teleport Anchor pos must be inside the region.", name, buildRegionInfoLink(region)));
            return -1;
        }
        if (tpAnchors.getTpAnchor(name).getPos().equals(pos)) {
            // they are the same
            return 0;
        }
        tpAnchors.addOrUpdate(name, pos);
        RegionManager.get().save();

        var anchor = tpAnchors.getTpAnchor(name);
        VisualizationManager.updateTpAnchor(region, anchor);
        var blockTpLink = TeleportAnchorPagination.buildTeleportToAnchorLink(region, anchor);
        if (hasAnchor) {
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.tp-anchor.updated.msg", "Updated position of '%s' to %s", name, blockTpLink));
        } else {
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.tp-anchor.added.msg", "Added new anchor '%s' at %s", name, blockTpLink));
        }
        return 0;
    }

    private static int removeTeleportAnchor(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, String name) {
        RegionAnchors tpAnchors = region.getTpAnchors();
        if (!tpAnchors.hasAnchor(name)) {
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.tp-anchor.fail-msg.not-existent", "Teleport anchor '%s' does not exist in %s", name, buildRegionInfoLink(region)));
            return -1;
        }
        TeleportAnchor anchor = tpAnchors.getTpAnchor(name);
        tpAnchors.removeTpAnchor(name);
        RegionManager.get().save();
        // TODO: Trigger update - if tpAnchor is currently visualized, it should be removed
        var blockTpLink = ChatLinkBuilder.buildDimensionalBlockTpLink(region.getDim(), anchor.getPos(), Component.literal(shortBlockPos(anchor.getPos())));
        sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.tp-anchor.removed.msg", "Removed teleport anchor '%s' (at %s ) from %s", name, blockTpLink, buildRegionInfoLink(region)));
        return 0;
    }

    public static boolean isValidName(String name) {
        return StringUtils.isAlphanumeric(name) && name.length() >= 4 && name.length() <= 50;
    }

    private static int renameTeleportAnchor(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, String name, String newName) {
        if (!isValidName(name) || !isValidName(newName)) {
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.tp-anchor.fail-msg.invalid-name", "Teleport Anchor name is invalid. Must be alphanumeric and between 3 and 50 letters.", name, buildRegionInfoLink(region)));
            return -1;
        }
        RegionAnchors tpAnchors = region.getTpAnchors();
        if (!tpAnchors.hasAnchor(name)) {
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.tp-anchor.fail-msg.not-existent", "Teleport anchor '%s' does not exist in %s", name, buildRegionInfoLink(region)));
            return -1;
        }
        if (tpAnchors.hasAnchor(newName)) {
            TeleportAnchor anchor = tpAnchors.getTpAnchor(newName);
            var blockTpLink = ChatLinkBuilder.buildDimensionalBlockTpLink(region.getDim(), anchor.getPos(), Component.literal(shortBlockPos(anchor.getPos())));
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.tp-anchor.fail-msg.already-present", "Teleport anchor '%s' %s is already defined in %s", name, blockTpLink, buildRegionInfoLink(region)));
            return 1;
        }
        tpAnchors.rename(name, newName);
        RegionManager.get().save();
        // TODO: Trigger update - if tpAnchor is currently visualized, it should be removed and displayed with new name
        sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.tp-anchor.renamed.msg", "Renamed teleport anchor '%s' to '%s'", name, newName));
        return 0;
    }

    /**
     * Prompt region area properties like teleport location and area.
     * == Area for [<region>]  ==
     * Location: [region] @ [X,Y,Z]
     * AreaType: Cuboid, Size: X=69, Y=10, Z=42
     * Marked Blocks: [X,Y,Z], ..., [X,Y,Z]
     * Actions: [set area] [set TP] [show area] [<=expand=>] [<=max=>]
     */
    private static int promptRegionAreaInfo(CommandContext<CommandSourceStack> ctx, IMarkableRegion region) {
        MultiLineMessage.send(ctx.getSource(), MultiLineMessage.areaInfo(region));
        return 0;
    }

    private static int teleport(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, String tpAnchorName) {
        if (!region.getTpAnchors().hasAnchor(tpAnchorName)) {
            // TODO
            sendCmdFeedback(ctx.getSource(), Component.literal("TODO"));
            return -1;
        }
        try {
            ServerPlayer self = ctx.getSource().getPlayerOrException();
            return teleport(ctx, region, tpAnchorName, self);
        } catch (CommandSyntaxException e) {
            Constants.LOGGER.warn("Unable to teleport command source to region. Can only be executed by a player");
            sendCmdFeedback(ctx.getSource(), Component.literal("TODO"));
            return -1;
        }
    }

    private static int teleport(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, String tpAnchorName, ServerPlayer playerToTeleport) {
        TeleportAnchor tpAnchor = region.getTpAnchors().getTpAnchor(tpAnchorName);
        BlockPos tpPos = tpAnchor.getPos();
        try {
            ServerPlayer player = ctx.getSource().getPlayerOrException();
            ServerLevel level = ctx.getSource().getServer().getLevel(region.getDim());
            if (level != null) {
                player.teleportTo(level, tpPos.getX(), tpPos.getY(), tpPos.getZ(), player.getYRot(), player.getXRot());
                return 0;
            } else {
                Constants.LOGGER.error("Error executing teleport command. Level is null.");
                sendCmdFeedback(ctx.getSource(), Component.literal("TODO"));
                return -1;
            }
        } catch (CommandSyntaxException e) {
            ServerLevel level = ctx.getSource().getServer().getLevel(region.getDim());
            if (level != null) {
                playerToTeleport.teleportTo(level, tpPos.getX(), tpPos.getY(), tpPos.getZ(), playerToTeleport.getYRot(), playerToTeleport.getXRot());
                return 0;
            }
            Constants.LOGGER.warn("Error executing teleport command.");
            sendCmdFeedback(ctx.getSource(), Component.literal("TODO"));
            return -1;
        }
    }

    private static int showTpAnchor(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, String tpAnchorName) {
        if (!region.getTpAnchors().hasAnchor(tpAnchorName)) {
            // TODO
            sendCmdFeedback(ctx.getSource(), Component.literal("TODO"));
            return -1;
        }
        TeleportAnchor tpAnchor = region.getTpAnchors().getTpAnchor(tpAnchorName);
        VisualizationManager.showTpAnchor(region, tpAnchor);
        return 0;
    }

    private static int hideTpAnchor(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, String tpAnchorName) {
        if (!region.getTpAnchors().hasAnchor(tpAnchorName)) {
            // TODO
            sendCmdFeedback(ctx.getSource(), Component.literal("TODO"));
            return -1;
        }
        TeleportAnchor tpAnchor = region.getTpAnchors().getTpAnchor(tpAnchorName);
        VisualizationManager.hideTpAnchor(region, tpAnchor);
        return 0;
    }
}
