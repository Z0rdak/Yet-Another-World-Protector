package de.z0rdak.yawp.commands;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.api.events.region.RegionEvent;
import de.z0rdak.yawp.commands.arguments.ArgumentUtil;
import de.z0rdak.yawp.commands.arguments.region.AddRegionChildArgumentType;
import de.z0rdak.yawp.commands.arguments.region.RegionArgumentType;
import de.z0rdak.yawp.commands.arguments.region.RemoveRegionChildArgumentType;
import de.z0rdak.yawp.config.server.RegionConfig;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.area.IMarkableArea;
import de.z0rdak.yawp.core.area.SphereArea;
import de.z0rdak.yawp.core.region.*;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.LocalRegions;
import net.minecraft.command.CommandSource;
import net.minecraft.command.Commands;
import net.minecraft.command.arguments.BlockPosArgument;
import net.minecraft.command.arguments.DimensionArgument;
import net.minecraft.command.arguments.EntityArgument;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.text.IFormattableTextComponent;
import net.minecraft.util.text.StringTextComponent;
import net.minecraft.util.text.TranslationTextComponent;
import net.minecraftforge.common.MinecraftForge;

import java.util.List;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.commands.arguments.ArgumentUtil.*;
import static de.z0rdak.yawp.util.MessageUtil.*;


public class RegionCommands {

    public final static int MIN_BUILD_LIMIT = 0;
    public final static int MAX_BUILD_LIMIT = 255;

    private RegionCommands() {
    }

    public static LiteralArgumentBuilder<CommandSource> build() {
        return literal(LOCAL)
                .then(Commands.argument(DIM.toString(), DimensionArgument.dimension())
                        .then(Commands.argument(LOCAL.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> RegionArgumentType.region().listSuggestions(ctx, builder))
                                .executes(ctx -> CommandUtil.promptRegionInfo(ctx, getRegionArgument(ctx)))
                                .then(literal(INFO)
                                        .executes(ctx -> CommandUtil.promptRegionInfo(ctx, getRegionArgument(ctx))))
                                .then(CommandUtil.buildClearSubCommand(ArgumentUtil::getRegionArgument))
                                .then(CommandUtil.buildAddSubCommand(ArgumentUtil::getRegionArgument))
                                .then(CommandUtil.buildListSubCommand(ArgumentUtil::getRegionArgument))
                                .then(CommandUtil.buildRemoveSubCommand(ArgumentUtil::getRegionArgument))
                                .then(CommandUtil.buildCopySubCommand(ArgumentUtil::getRegionArgument))
                                .then(literal(ADD)
                                        .then(literal(CHILD)
                                                .then(Commands.argument(CHILD.toString(), StringArgumentType.word())
                                                        .suggests((ctx, builder) -> AddRegionChildArgumentType.potentialChildRegions().listSuggestions(ctx, builder))
                                                        .executes(ctx -> addChildren(ctx, getRegionArgument(ctx), getChildRegionArgument(ctx))))))
                                .then(literal(REMOVE)
                                        .then(literal(CHILD)
                                                .then(Commands.argument(CHILD.toString(), StringArgumentType.word())
                                                        .suggests((ctx, builder) -> RemoveRegionChildArgumentType.childRegions().listSuggestions(ctx, builder))
                                                        .executes(ctx -> removeChildren(ctx, getDimCacheArgument(ctx), getRegionArgument(ctx), getChildRegionArgument(ctx))))))
                                .then(literal(LIST)
                                        .then(literal(CHILDREN)
                                                .executes(ctx -> promptRegionChildren(ctx, getRegionArgument(ctx), 0))
                                                .then(Commands.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                                        .executes(ctx -> promptRegionChildren(ctx, getRegionArgument(ctx), getPageNoArgument(ctx))))))
                                .then(literal(STATE)
                                        .executes(ctx -> promptLocalRegionState(ctx, getRegionArgument(ctx)))
                                        .then(literal(ALERT)
                                                .executes(ctx -> CommandUtil.setAlertState(ctx, getRegionArgument(ctx), !getRegionArgument(ctx).isMuted()))
                                                .then(Commands.argument(ALERT.toString(), BoolArgumentType.bool())
                                                        .executes(ctx -> CommandUtil.setAlertState(ctx, getRegionArgument(ctx), getAlertArgument(ctx)))))
                                        .then(literal(ENABLE)
                                                .executes(ctx -> CommandUtil.setActiveState(ctx, getRegionArgument(ctx), !getRegionArgument(ctx).isActive()))
                                                .then(Commands.argument(ENABLE.toString(), BoolArgumentType.bool())
                                                        .executes(ctx -> CommandUtil.setActiveState(ctx, getRegionArgument(ctx), getEnableArgument(ctx)))))
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
                                        .executes(ctx -> promptRegionAreaInfo(ctx.getSource(), getRegionArgument(ctx)))
                                        .then(literal(SET)
                                                .then(Commands.literal(AreaType.CUBOID.areaType)
                                                        .then(Commands.argument(POS1.toString(), BlockPosArgument.blockPos())
                                                                .then(Commands.argument(POS2.toString(), BlockPosArgument.blockPos())
                                                                        .executes(ctx -> setCuboidArea(ctx, getRegionArgument(ctx), BlockPosArgument.getOrLoadBlockPos(ctx, POS1.toString()), BlockPosArgument.getOrLoadBlockPos(ctx, POS2.toString()))))))
                                                .then(Commands.literal(AreaType.SPHERE.areaType)
                                                        .then(Commands.argument(CENTER_POS.toString(), BlockPosArgument.blockPos())
                                                                .then(Commands.argument(RADIUS_POS.toString(), BlockPosArgument.blockPos())
                                                                        .executes(ctx -> setSphereArea(ctx, getRegionArgument(ctx), BlockPosArgument.getOrLoadBlockPos(ctx, CENTER_POS.toString()), BlockPosArgument.getOrLoadBlockPos(ctx, RADIUS_POS.toString()))))))
                                                .then(Commands.literal(AreaType.SPHERE.areaType)
                                                        .then(Commands.argument(CENTER_POS.toString(), BlockPosArgument.blockPos())
                                                                .then(Commands.argument(RADIUS.toString(), IntegerArgumentType.integer(0))
                                                                        .executes(ctx -> setSphereArea(ctx, getRegionArgument(ctx), BlockPosArgument.getOrLoadBlockPos(ctx, CENTER_POS.toString()), IntegerArgumentType.getInteger(ctx, RADIUS.toString()))))))
                                        )
                                        .then(literal(EXPAND)
                                                .then(Commands.literal(AreaType.CUBOID.areaType)
                                                        .executes(ctx -> expandCuboid(ctx, getRegionArgument(ctx), MIN_BUILD_LIMIT, MAX_BUILD_LIMIT))
                                                        .then(Commands.argument(Y_MIN.toString(), IntegerArgumentType.integer(MIN_BUILD_LIMIT, MAX_BUILD_LIMIT))
                                                                .then(Commands.argument(Y_MAX.toString(), IntegerArgumentType.integer(MIN_BUILD_LIMIT, MAX_BUILD_LIMIT))
                                                                        .executes(ctx -> expandCuboid(ctx, getRegionArgument(ctx), IntegerArgumentType.getInteger(ctx, Y_MIN.toString()), IntegerArgumentType.getInteger(ctx, Y_MAX.toString())))))
                                                )
                                                .then(Commands.literal(AreaType.SPHERE.areaType)
                                                        .executes(ctx -> expandSphere(ctx, getRegionArgument(ctx), 1))
                                                        .then(Commands.argument(EXPANSION.toString(), IntegerArgumentType.integer())
                                                                .executes(ctx -> expandSphere(ctx, getRegionArgument(ctx), IntegerArgumentType.getInteger(ctx, EXPANSION.toString()))))
                                                )
                                        )
                                        .then(literal(TELEPORT)
                                                .then(Commands.literal(SET.toString())
                                                        .then(Commands.argument(TARGET.toString(), BlockPosArgument.blockPos())
                                                                .executes(ctx -> setTeleportPos(ctx, getRegionArgument(ctx), BlockPosArgument.getOrLoadBlockPos(ctx, TARGET.toString())))))))
                                        .then(literal(TELEPORT)
                                                .executes(ctx -> teleport(ctx, getRegionArgument(ctx)))
                                                .then(Commands.argument(PLAYER.toString(), EntityArgument.player())
                                                        .executes(ctx -> teleport(ctx, getRegionArgument(ctx), getPlayerArgument(ctx)))))
                                        .then(literal(RENAME)
                                                .then(Commands.argument(NAME.toString(), StringArgumentType.word())
                                                        .executes(ctx -> renameRegion(ctx, getRegionArgument(ctx), getRegionNameArgument(ctx), getDimCacheArgument(ctx)))))
                                // Idea: reset player, team, etc. with complete hierarchy
                                // Scenario: Keep region and children with flags but reset it for new player base
                        )
                );
    }

    private static int expandSphere(CommandContext<CommandSource> ctx, IMarkableRegion region, int expansion) {
        SphereArea expand = SphereArea.expand((SphereArea) region.getArea(), expansion);
        return updateArea(ctx, region, expand);
    }

    private static int setSphereArea(CommandContext<CommandSource> src, IMarkableRegion region, BlockPos center, int radius) {
        BlockPos newRadius = new BlockPos(radius, radius, radius).offset(0, radius, 0);
        return setSphereArea(src, region, center, newRadius);
    }

    private static int setSphereArea(CommandContext<CommandSource> src, IMarkableRegion region, BlockPos center, BlockPos radiusPos) {
        return updateArea(src, region, new SphereArea(center, radiusPos));
    }

    private static int setCuboidArea(CommandContext<CommandSource> src, IMarkableRegion region, BlockPos p1, BlockPos p2) {
        return updateArea(src, region, new CuboidArea(p1, p2));
    }

    private static int expandCuboid(CommandContext<CommandSource> ctx, IMarkableRegion region, int yMin, int yMax) {
        CuboidArea expand = CuboidArea.expand((CuboidArea) region.getArea(), yMin, yMax);
        return updateArea(ctx, region, expand);
    }

    private static int updateArea(CommandContext<CommandSource> src, IMarkableRegion region, IMarkableArea area) {
        try {
            AreaType prevAreaType = region.getArea().getAreaType();
            AreaType newAreaType = area.getAreaType();
            IProtectedRegion parent = region.getParent();
            // TODO: Implement a contains method for regions, with dimensional always returning true if dim is the same
            // IMarkableRegions would use the area contains method

            ServerPlayerEntity player;
            try {
                player = src.getSource().getPlayerOrException();
            } catch (CommandSyntaxException e) {
                player = null;
            }
            if (MinecraftForge.EVENT_BUS.post(new RegionEvent.UpdateArea(region, area, player))) {
                return 0;
            }

            // Note: this check can be remove once the area types are all implemented, it's just here to catch any errors
            switch (newAreaType) {
                case CUBOID:
                case SPHERE:
                    if (parent.getRegionType() == RegionType.DIMENSION) {
                        int newPriority = LocalRegions.ensureHigherRegionPriorityFor(region, RegionConfig.getDefaultPriority());
                        YetAnotherWorldProtector.LOGGER.info("New priority {} for region {}", newPriority, region.getName());
                    }
                    if (parent.getRegionType() == RegionType.LOCAL) {
                        IMarkableRegion localParent = (IMarkableRegion) parent;
                        switch (localParent.getArea().getAreaType()) {
                            case CUBOID:
                            case SPHERE:
                                if (localParent.getArea().containsOther(area)) {
                                    int newPriority = LocalRegions.ensureHigherRegionPriorityFor(region, localParent.getPriority() + 1);
                                    YetAnotherWorldProtector.LOGGER.info("New priority {} for region {}", newPriority, region.getName());
                                } else {
                                    IFormattableTextComponent updateAreaFailMsg = new TranslationTextComponent("cli.msg.info.region.area.area.update.fail.boundaries", buildRegionInfoLink(parent), buildRegionInfoLink(region));
                                    sendCmdFeedback(src.getSource(), updateAreaFailMsg);
                                    return 1;
                                }
                                break;
                            case CYLINDER:
                            case POLYGON_3D:
                            case PRISM:
                                throw new UnsupportedOperationException("Unsupported area type");
                        }
                    }
                case CYLINDER:
                case POLYGON_3D:
                case PRISM:
                    throw new UnsupportedOperationException("Unsupported area type");
            }
            if (prevAreaType != newAreaType) {
                IFormattableTextComponent updateAreaFailMsg = new TranslationTextComponent("cli.msg.info.region.area.update.type.change", buildRegionInfoLink(region), prevAreaType, newAreaType);
                sendCmdFeedback(src.getSource(), updateAreaFailMsg);
            }
            region.setArea(area);
            RegionDataManager.save();
            IFormattableTextComponent updateAreaMsg = new TranslationTextComponent("cli.msg.info.region.area.area.update", buildRegionAreaLink(region), buildRegionInfoLink(region));
            sendCmdFeedback(src.getSource(), updateAreaMsg);
            return 0;
        } catch (Exception ex) {
            YetAnotherWorldProtector.LOGGER.error("Failed to update area: {}", ex.getMessage());
            return 1;
        }
    }

    private static int renameRegion(CommandContext<CommandSource> src, IMarkableRegion region, String regionName, DimensionRegionCache dimCache) {
        int res = RegionDataManager.get().isValidRegionName(dimCache.getDimensionalRegion().getDim(), regionName);
        if (res == -1) {
            sendCmdFeedback(src.getSource(), new TranslationTextComponent("cli.msg.dim.info.region.create.name.invalid", regionName));
            return res;
        }
        if (res == 1) {
            sendCmdFeedback(src.getSource(), new TranslationTextComponent("cli.msg.dim.info.region.create.name.exists", dimCache.getDimensionalRegion().getName(), buildRegionInfoLink(dimCache.getRegion(regionName))));
            return res;
        }
        try {
            ServerPlayerEntity player;
            try {
                player = src.getSource().getPlayerOrException();
            } catch (CommandSyntaxException e) {
                player = null;
            }
            if (MinecraftForge.EVENT_BUS.post(new RegionEvent.RenameRegion(region, region.getName(), regionName, player))) {
                return 0;
            }
            // TODO: Test this
            dimCache.renameRegion(region, regionName);
            RegionDataManager.save();
            return 0;
        } catch (IllegalArgumentException ex) {
            sendCmdFeedback(src.getSource(), new TranslationTextComponent("cli.msg.dim.info.region.create.name.exists", dimCache.getDimensionalRegion().getName(), buildRegionInfoLink(dimCache.getRegion(regionName))));
            return 1;
        }
    }

    public static int removeChildren(CommandContext<CommandSource> src, DimensionRegionCache dimCache, IProtectedRegion parent, IProtectedRegion child) {
        if (parent.hasChild(child)) {
            // FIXME: Removing child does not set priority correct with overlapping regions
            dimCache.getDimensionalRegion().addChild(child); // this also removes the child from the local parent
            child.setIsActive(false);
            LocalRegions.ensureLowerRegionPriorityFor((CuboidRegion) child, RegionConfig.getDefaultPriority());
            RegionDataManager.save();
            IFormattableTextComponent parentLink = buildRegionInfoLink(parent);
            IFormattableTextComponent notLongerChildLink = buildRegionInfoLink(child);
            IFormattableTextComponent dimensionalLink = buildRegionInfoLink(dimCache.getDimensionalRegion());
            IFormattableTextComponent undoLink = buildRegionActionUndoLink(src.getInput(), REMOVE, ADD);
            sendCmdFeedback(src.getSource(), new TranslationTextComponent("cli.msg.info.region.children.remove", notLongerChildLink, parentLink).append(" ")
                    .append(undoLink));
            sendCmdFeedback(src.getSource(), new TranslationTextComponent("cli.msg.info.region.parent.clear", notLongerChildLink, dimensionalLink));
            return 0;
        }
        // should not happen, due to RemoveRegionChildArgumentType should only provide valid child regions
        return -1;
    }

    public static int addChildren(CommandContext<CommandSource> src, IMarkableRegion parent, IMarkableRegion child) {
        if (!parent.hasChild(child) && child.getParent() != null && child.getParent() instanceof DimensionalRegion) {
            parent.addChild(child);
            LocalRegions.ensureHigherRegionPriorityFor(child, parent.getPriority() + 1);
            RegionDataManager.save();
            IFormattableTextComponent parentLink = buildRegionInfoLink(parent);
            IFormattableTextComponent childLink = buildRegionInfoLink(child);
            IFormattableTextComponent undoLink = buildRegionActionUndoLink(src.getInput(), ADD, REMOVE);
            sendCmdFeedback(src.getSource(), new TranslationTextComponent("cli.msg.info.region.children.add", childLink, parentLink).append(" ")
                    .append(undoLink));
            return 0;
        }
        // should not happen, due to AddRegionChildArgumentType should only provide valid child regions
        return -1;
    }

    public static int setPriority(CommandContext<CommandSource> src, IMarkableRegion region, int priority, int factor) {
        long newValue = (long) region.getPriority() + ((long) priority * factor);
        if (Integer.MAX_VALUE - newValue > 0) {
            return setPriority(src, region, (int) newValue);
        } else {
            sendCmdFeedback(src.getSource(), new TranslationTextComponent("cli.msg.warn.region.state.priority.set.invalid", buildRegionInfoLink(region), newValue));
            return -1;
        }
    }

    /**
     * Attempt to set new priority for the given region. <br>
     * Fails if region priority is used by an overlapping region at same hierarchy level.
     */
    public static int setPriority(CommandContext<CommandSource> src, IMarkableRegion region, int priority) {
        IProtectedRegion parent = region.getParent();
        if (parent instanceof IMarkableRegion) {
            int parentPriority = ((IMarkableRegion) parent).getPriority();
            if (parentPriority >= priority) {
                IFormattableTextComponent updatePriorityFailMsg = new TranslationTextComponent("cli.msg.info.region.state.priority.set.fail.to-low", buildRegionInfoLink(region));
                sendCmdFeedback(src.getSource(), updatePriorityFailMsg);
                return 1;
            }
        }
        CuboidRegion cuboidRegion = (CuboidRegion) region;
        boolean existRegionWithSamePriority = LocalRegions.hasAnyRegionWithSamePriority(cuboidRegion, priority);
        if (existRegionWithSamePriority) {
            IFormattableTextComponent updatePriorityFailMsg = new TranslationTextComponent("cli.msg.info.region.state.priority.set.fail.same", buildRegionInfoLink(region), priority);
            sendCmdFeedback(src.getSource(), updatePriorityFailMsg);
            return 1;
        } else {
            int oldPriority = region.getPriority();
            if (oldPriority != priority) {
                region.setPriority(priority);
                RegionDataManager.save();
                IFormattableTextComponent undoLink = buildRegionActionUndoLink(src.getInput(), String.valueOf(oldPriority), String.valueOf(priority));
                sendCmdFeedback(src.getSource(), new TranslationTextComponent("cli.msg.info.region.state.priority.set.success",
                        buildRegionInfoLink(region), oldPriority, region.getPriority())
                        .append(" ")
                        .append(undoLink));
                return 0;
            } else {
                sendCmdFeedback(src.getSource(), new TranslationTextComponent("cli.msg.info.region.state.priority.set.fail.no-change", buildRegionInfoLink(region)));
                return 1;
            }
        }
    }

    private static int promptRegionChildren(CommandContext<CommandSource> ctx, IProtectedRegion region, int pageNo) {
        List<IMarkableRegion> children = region.getChildren().values().stream().map(r -> (IMarkableRegion) r).collect(Collectors.toList());
        IFormattableTextComponent childRegionList = new StringTextComponent("");
        if (children.isEmpty()) {
            IFormattableTextComponent noChildrenText = new TranslationTextComponent("cli.msg.info.region.children.empty", buildRegionInfoLink(region));
            childRegionList.append(noChildrenText);
            sendCmdFeedback(ctx.getSource(), childRegionList);
        }
        List<IFormattableTextComponent> regionPagination = buildPaginationComponents(
                buildRegionChildrenHeader(region),
                buildCommandStr(LOCAL.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), CHILDREN.toString()),
                buildRemoveRegionEntries(region, children),
                pageNo,
                new StringTextComponent(" - ").append(buildRegionAddChildrenLink(region)));
        regionPagination.forEach(line -> sendCmdFeedback(ctx.getSource(), line));
        return 0;
    }

    /**
     * Prompt the common region state and the local region priority info
     * Priority: n [#][+5][-5]
     */
    private static int promptLocalRegionState(CommandContext<CommandSource> ctx, IMarkableRegion region) {
        CommandUtil.promptRegionState(ctx, region);
        sendCmdFeedback(ctx.getSource(), buildInfoComponent("cli.msg.info.region.state.priority", buildRegionPriorityComponent(region)));
        return 0;
    }

    /**
     * Prompt region area properties like teleport location and area.
     * == Area for [<region>]  ==
     * Location: [dimInfo] @ [tpCoordinates]
     * Area: Cuboid, Size: X=69, Y=10, Z=42 [<=expand=>] [<=max=>]
     * Marked Blocks: [X,Y,Z], ..., [X,Y,Z] [Set] [Show]
     * TP-Anchor: [X,Y,Z] [Set]
     */
    private static int promptRegionAreaInfo(CommandSource src, IMarkableRegion region) {
        sendCmdFeedback(src, buildHeader(new TranslationTextComponent("cli.msg.info.header.for", buildRegionAreaLink(region), buildRegionInfoLink(region))));
        sendCmdFeedback(src, buildInfoComponent("cli.msg.info.region.area.location", buildDimensionTeleportLink(region)));
        sendCmdFeedback(src, buildInfoComponent("cli.msg.info.region.area.area", buildRegionAreaDetailComponent(region)));
        sendCmdFeedback(src, buildInfoComponent("cli.msg.info.region.area.marked", buildMarkedBlocksAreaComponent(region)));
        sendCmdFeedback(src, buildInfoComponent("cli.msg.info.region.area.tp", buildRegionAreaTpComponent(region)));
        return 0;
    }

    private static int teleport(CommandContext<CommandSource> ctx, IMarkableRegion region) {
        try {
            ServerPlayerEntity player = ctx.getSource().getPlayerOrException();
            return teleport(ctx, region, player);
        } catch (CommandSyntaxException e) {
            YetAnotherWorldProtector.LOGGER.warn("Unable to teleport command source to region. Most likely not a player");
            return -1;
        }
    }

    private static int teleport(CommandContext<CommandSource> ctx, IMarkableRegion region, PlayerEntity player) {
        try {
            String teleportCmd = buildRegionTpCmd(region, player.getScoreboardName());
            ctx.getSource().getServer().getCommands().getDispatcher().execute(teleportCmd, ctx.getSource());
            YetAnotherWorldProtector.LOGGER.warn("Trying to execute TP: '{}'", teleportCmd);
            return 0;
        } catch (CommandSyntaxException e) {
            YetAnotherWorldProtector.LOGGER.warn("Error executing teleport command.");
            return -1;
        }
    }

    private static int setTeleportPos(CommandContext<CommandSource> src, IMarkableRegion region, BlockPos target) {
        if (!region.getTpTarget().equals(target)) {
            region.setTpTarget(target);
            RegionDataManager.save();
            IFormattableTextComponent newTpTargetLink = buildDimensionalBlockTpLink(region.getDim(), target);
            sendCmdFeedback(src.getSource(), new TranslationTextComponent("cli.msg.info.region.area.location.teleport.set", buildRegionInfoLink(region), newTpTargetLink));
            return 0;
        }
        return 1;
    }
}
