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
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.core.region.RegionType;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.LocalRegions;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.SharedSuggestionProvider;
import net.minecraft.commands.arguments.DimensionArgument;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.commands.arguments.coordinates.BlockPosArgument;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;

import java.util.Collections;

import static de.z0rdak.yawp.api.events.region.RegionEvents.post;
import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.commands.arguments.ArgumentUtil.*;
import static de.z0rdak.yawp.util.ChatComponentBuilder.*;
import static de.z0rdak.yawp.util.MessageSender.sendCmdFeedback;


public class RegionCommands {

    public final static int MIN_BUILD_LIMIT = -64;
    public final static int MAX_BUILD_LIMIT = 320;

    private RegionCommands() {
    }

    public static LiteralArgumentBuilder<CommandSourceStack> build() {
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
                                .then(literal(STATE)
                                        .executes(ctx -> promptLocalRegionState(ctx, getRegionArgument(ctx)))
                                        .then(literal(ALERT)
                                                .executes(ctx -> CommandUtil.setAlertState(ctx, getRegionArgument(ctx), getRegionArgument(ctx).isMuted()))
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
                                                                        .executes(ctx -> setCuboidArea(ctx, getRegionArgument(ctx), BlockPosArgument.getLoadedBlockPos(ctx, POS1.toString()), BlockPosArgument.getLoadedBlockPos(ctx, POS2.toString()))))))
                                                .then(Commands.literal(AreaType.SPHERE.areaType)
                                                        .then(Commands.argument(CENTER_POS.toString(), BlockPosArgument.blockPos())
                                                                .then(Commands.argument(RADIUS_POS.toString(), BlockPosArgument.blockPos())
                                                                        .executes(ctx -> setSphereArea(ctx, getRegionArgument(ctx), BlockPosArgument.getLoadedBlockPos(ctx, CENTER_POS.toString()), BlockPosArgument.getLoadedBlockPos(ctx, RADIUS_POS.toString()))))))
                                                .then(Commands.literal(AreaType.SPHERE.areaType)
                                                        .then(Commands.argument(CENTER_POS.toString(), BlockPosArgument.blockPos())
                                                                .then(Commands.argument(RADIUS.toString(), IntegerArgumentType.integer(0))
                                                                        .executes(ctx -> setSphereArea(ctx, getRegionArgument(ctx), BlockPosArgument.getLoadedBlockPos(ctx, CENTER_POS.toString()), IntegerArgumentType.getInteger(ctx, RADIUS.toString()))))))
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
                                                                .executes(ctx -> expandSphere(ctx, getRegionArgument(ctx), IntegerArgumentType.getInteger(ctx, EXPANSION.toString())))))
                                        )
                                        .then(literal(TELEPORT)
                                                .then(Commands.literal(SET.toString())
                                                        .then(Commands.argument(TARGET.toString(), BlockPosArgument.blockPos())
                                                                .executes(ctx -> setTeleportPos(ctx, getRegionArgument(ctx), BlockPosArgument.getLoadedBlockPos(ctx, TARGET.toString())))))
                                        )
                                        .then(literal(TELEPORT)
                                                .executes(ctx -> teleport(ctx, getRegionArgument(ctx)))
                                                .then(Commands.argument(PLAYER.toString(), EntityArgument.player())
                                                        .executes(ctx -> teleport(ctx, getRegionArgument(ctx), getPlayerArgument(ctx)))))
                                ).then(literal(RENAME)
                                        .then(Commands.argument(NAME.toString(), StringArgumentType.word())
                                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(Collections.singletonList(getRegionArgument(ctx).getName()), builder))
                                                .executes(ctx -> renameRegion(ctx, getRegionArgument(ctx), getRegionNameArgument(ctx), getDimCacheArgument(ctx))))
                                )
                        )
                );
    }

    private static int expandSphere(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, int expansion) {
        SphereArea expand = SphereArea.expand((SphereArea) region.getArea(), expansion);
        return updateArea(ctx, region, expand);
    }

    private static int setSphereArea(CommandContext<CommandSourceStack> src, IMarkableRegion region, BlockPos center, int radius) {
        BlockPos newRadius = center.offset(0, radius, 0);
        return setSphereArea(src, region, center, newRadius);
    }

    private static int setSphereArea(CommandContext<CommandSourceStack> src, IMarkableRegion region, BlockPos center, BlockPos radiusPos) {
        return updateArea(src, region, new SphereArea(center, radiusPos));
    }

    private static int setCuboidArea(CommandContext<CommandSourceStack> src, IMarkableRegion region, BlockPos p1, BlockPos p2) {
        return updateArea(src, region, new CuboidArea(p1, p2));
    }

    private static int expandCuboid(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, int yMin, int yMax) {
        CuboidArea expand = CuboidArea.expand((CuboidArea) region.getArea(), yMin, yMax);
        return updateArea(ctx, region, expand);
    }

    private static int updateArea(CommandContext<CommandSourceStack> src, IMarkableRegion region, IMarkableArea area) {
        try {
            AreaType prevAreaType = region.getArea().getAreaType();
            AreaType newAreaType = area.getAreaType();
            IProtectedRegion parent = region.getParent();
            // TODO: Implement a contains method for regions, with dimensional always returning true if dim is the same
            // IMarkableRegions would use the area contains method

            ServerPlayer player;
            try {
                player = src.getSource().getPlayerOrException();
            } catch (CommandSyntaxException e) {
                player = null;
            }
            if (post(new RegionEvent.UpdateArea(region, area, player))) {
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
                                    MutableComponent updateAreaFailMsg = new TranslatableComponent("cli.msg.info.region.area.area.update.fail.boundaries", buildRegionInfoLink(parent), buildRegionInfoLink(region));
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
                    break;
                case CYLINDER:
                case POLYGON_3D:
                case PRISM:
                    throw new UnsupportedOperationException("Unsupported area type");
            }
            if (prevAreaType != newAreaType) {
                MutableComponent updateAreaFailMsg = new TranslatableComponent("cli.msg.info.region.area.update.type.change", buildRegionInfoLink(region), prevAreaType, newAreaType);
                sendCmdFeedback(src.getSource(), updateAreaFailMsg);
            }
            region.setArea(area);
            RegionDataManager.save();
            MutableComponent updateAreaMsg = new TranslatableComponent("cli.msg.info.region.area.area.update", buildRegionAreaLink(region), buildRegionInfoLink(region));
            sendCmdFeedback(src.getSource(), updateAreaMsg);
            return 0;
        } catch (Exception ex) {
            YetAnotherWorldProtector.LOGGER.error("Failed to update area: {}", ex.getMessage());
            return 1;
        }
    }

    private static int renameRegion(CommandContext<CommandSourceStack> src, IMarkableRegion region, String regionName, DimensionRegionCache dimCache) {
        if (region.getName().equals(regionName)) {
            sendCmdFeedback(src.getSource(), new TranslatableComponent("cli.msg.dim.info.region.create.name.no-change", regionName));
            return 1;
        }
        int res = RegionDataManager.get().isValidRegionName(dimCache.getDimensionalRegion().getDim(), regionName);
        if (res == -1) {
            sendCmdFeedback(src.getSource(), new TranslatableComponent("cli.msg.dim.info.region.create.name.invalid", regionName));
            return res;
        }
        if (res == 1) {
            sendCmdFeedback(src.getSource(), new TranslatableComponent("cli.msg.dim.info.region.create.name.exists", dimCache.getDimensionalRegion().getName(), buildRegionInfoLink(dimCache.getRegion(regionName))));
            return res;
        }
        try {
            ServerPlayer player;
            try {
                player = src.getSource().getPlayerOrException();
            } catch (CommandSyntaxException e) {
                player = null;
            }
            if (post(new RegionEvent.RenameRegion(region, region.getName(), regionName, player))) {
                return 0;
            }
            String oldName = region.getName();
            dimCache.renameRegion(region, regionName);
            sendCmdFeedback(src.getSource(), new TranslatableComponent("cli.msg.dim.info.region.create.name.success", buildRegionInfoLink(region), oldName, regionName));
            RegionDataManager.save();
            return 0;
        } catch (IllegalArgumentException ex) {
            sendCmdFeedback(src.getSource(), new TranslatableComponent("cli.msg.dim.info.region.create.name.exists", dimCache.getDimensionalRegion().getName(), buildRegionInfoLink(dimCache.getRegion(regionName))));
            return 1;
        }
    }

    // TODO: Test removing child does not set priority correct with overlapping regions
    public static int removeChildren(CommandContext<CommandSourceStack> src, DimensionRegionCache dimCache, IProtectedRegion parent, IMarkableRegion child) {
        if (parent.hasChild(child)) {
            parent.removeChild(child);
            dimCache.getDimensionalRegion().addChild(child);
            LocalRegions.ensureLowerRegionPriorityFor(child, RegionConfig.getDefaultPriority());
            RegionDataManager.save();
            MutableComponent parentLink = buildRegionInfoLink(parent);
            MutableComponent notLongerChildLink = buildRegionInfoLink(child);
            MutableComponent dimensionalLink = buildRegionInfoLink(dimCache.getDimensionalRegion());
            MutableComponent undoLink = buildRegionActionUndoLink(src.getInput(), REMOVE, ADD);
            sendCmdFeedback(src.getSource(), new TranslatableComponent("cli.msg.info.region.children.remove", notLongerChildLink, parentLink).append(" ")
                    .append(undoLink));
            sendCmdFeedback(src.getSource(), new TranslatableComponent("cli.msg.info.region.parent.clear", notLongerChildLink, dimensionalLink));
            return 0;
        }
        // should not happen, due to RemoveRegionChildArgumentType should only provide valid child regions
        return -1;
    }

    public static int addChildren(CommandContext<CommandSourceStack> src, IMarkableRegion parent, IMarkableRegion child) {
        boolean parentIsNotNullAndDimension = child.getParent() != null && child.getParent().getRegionType() == RegionType.DIMENSION;
        if (!parent.hasChild(child) && parentIsNotNullAndDimension) {
            child.getParent().removeChild(child);
            parent.addChild(child);
            LocalRegions.ensureHigherRegionPriorityFor(child, parent.getPriority() + 1);
            RegionDataManager.save();
            MutableComponent parentLink = buildRegionInfoLink(parent);
            MutableComponent childLink = buildRegionInfoLink(child);
            MutableComponent undoLink = buildRegionActionUndoLink(src.getInput(), ADD, REMOVE);
            sendCmdFeedback(src.getSource(), new TranslatableComponent("cli.msg.info.region.children.add", childLink, parentLink).append(" ")
                    .append(undoLink));
            return 0;
        }
        // should not happen, due to AddRegionChildArgumentType should only provide valid child regions
        return -1;
    }

    public static int setPriority(CommandContext<CommandSourceStack> src, IMarkableRegion region, int priority, int factor) {
        long newValue = (long) region.getPriority() + ((long) priority * factor);
        if (Integer.MAX_VALUE - newValue > 0) {
            return setPriority(src, region, (int) newValue);
        } else {
            sendCmdFeedback(src.getSource(), new TranslatableComponent("cli.msg.warn.region.state.priority.set.invalid", buildRegionInfoLink(region), newValue));
            return -1;
        }
    }

    /**
     * Attempt to set new priority for the given region. <br>
     * Fails if region priority is used by an overlapping region at same hierarchy level.
     */
    public static int setPriority(CommandContext<CommandSourceStack> src, IMarkableRegion region, int priority) {
        IProtectedRegion parent = region.getParent();
        if (parent instanceof IMarkableRegion) {
            int parentPriority = ((IMarkableRegion) parent).getPriority();
            if (parentPriority >= priority) {
                MutableComponent updatePriorityFailMsg = new TranslatableComponent("cli.msg.info.region.state.priority.set.fail.to-low", buildRegionInfoLink(region));
                sendCmdFeedback(src.getSource(), updatePriorityFailMsg);
                return 1;
            }
        }
        boolean existRegionWithSamePriority = LocalRegions.hasAnyRegionWithSamePriority(region, priority);
        if (existRegionWithSamePriority) {
            MutableComponent updatePriorityFailMsg = new TranslatableComponent("cli.msg.info.region.state.priority.set.fail.same", buildRegionInfoLink(region), priority);
            sendCmdFeedback(src.getSource(), updatePriorityFailMsg);
            return 1;
        } else {
            int oldPriority = region.getPriority();
            if (oldPriority != priority) {
                region.setPriority(priority);
                RegionDataManager.save();
                MutableComponent undoLink = buildRegionActionUndoLink(src.getInput(), String.valueOf(oldPriority), String.valueOf(priority));
                sendCmdFeedback(src.getSource(), new TranslatableComponent("cli.msg.info.region.state.priority.set.success",
                        buildRegionInfoLink(region), oldPriority, region.getPriority())
                        .append(" ")
                        .append(undoLink));
                return 0;
            } else {
                sendCmdFeedback(src.getSource(), new TranslatableComponent("cli.msg.info.region.state.priority.set.fail.no-change", buildRegionInfoLink(region)));
                return 1;
            }
        }
    }

    /**
     * Prompt the common region state and the local region priority info
     * Priority: n [#][+5][-5]
     */
    private static int promptLocalRegionState(CommandContext<CommandSourceStack> ctx, IMarkableRegion region) {
        CommandUtil.promptRegionState(ctx, region);
        sendCmdFeedback(ctx.getSource(), buildInfoComponent("cli.msg.info.region.state.priority", buildRegionPriorityComponent(region)));
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
    private static int promptRegionAreaInfo(CommandSourceStack src, IMarkableRegion region) {
        sendCmdFeedback(src, buildHeader(new TranslatableComponent("cli.msg.info.header.of", buildRegionAreaLink(region), buildRegionInfoLink(region))));
        sendCmdFeedback(src, buildInfoComponent("cli.msg.info.region.area.location", buildRegionTeleportLink(region, null)));
        sendCmdFeedback(src, buildInfoComponent("cli.msg.info.region.area.area", buildRegionAreaDetailComponent(region)));
        sendCmdFeedback(src, buildInfoComponent("cli.msg.info.region.area.marked", buildAreaMarkedBlocksTpLinks(region)));
        sendCmdFeedback(src, buildInfoComponent("cli.msg.info.region.area.actions", buildRegionAreaActionLinks(region)));
        return 0;
    }

    private static int teleport(CommandContext<CommandSourceStack> ctx, IMarkableRegion region) {
        try {
            ServerPlayer player = ctx.getSource().getPlayerOrException();
            return teleport(ctx, region, player);
        } catch (CommandSyntaxException e) {
            YetAnotherWorldProtector.LOGGER.warn("Unable to teleport command source to region. Most likely not a player");
            return -1;
        }
    }

    private static int teleport(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, ServerPlayer player) {
        try {
            ServerPlayer playerEntity = ctx.getSource().getPlayerOrException();
            ServerLevel level = ctx.getSource().getServer().getLevel(region.getDim());
            if (level != null) {
                playerEntity.teleportTo(level, region.getTpTarget().getX(), region.getTpTarget().getY(), region.getTpTarget().getZ(), playerEntity.yRotO, playerEntity.xRotO);
                return 0;
            } else {
                YetAnotherWorldProtector.LOGGER.error("Error executing teleport command. Level is null.");
                return -1;
            }
        } catch (CommandSyntaxException e) {
            ServerLevel level = ctx.getSource().getServer().getLevel(region.getDim());
            if (level != null) {
                player.teleportTo(level, region.getTpTarget().getX(), region.getTpTarget().getY(), region.getTpTarget().getZ(), player.yRotO, player.xRotO);
                return 0;
            }
            YetAnotherWorldProtector.LOGGER.warn("Error executing teleport command.");
            return -1;
        }
    }

    private static int setTeleportPos(CommandContext<CommandSourceStack> src, IMarkableRegion region, BlockPos target) {
        if (!region.getTpTarget().equals(target)) {
            region.setTpTarget(target);
            RegionDataManager.save();
            MutableComponent newTpTargetLink = buildDimensionalBlockTpLink(region.getDim(), target);
            sendCmdFeedback(src.getSource(), new TranslatableComponent("cli.msg.info.region.area.tp.set.msg", buildRegionInfoLink(region), newTpTargetLink));
            return 0;
        }
        return 1;
    }
}
