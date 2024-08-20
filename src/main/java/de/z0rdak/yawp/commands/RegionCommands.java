package de.z0rdak.yawp.commands;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.api.events.region.RegionEvent;
import de.z0rdak.yawp.api.events.region.RegionEvents;
import de.z0rdak.yawp.commands.arguments.ArgumentUtil;
import de.z0rdak.yawp.commands.arguments.region.AddRegionChildArgumentType;
import de.z0rdak.yawp.commands.arguments.region.RegionArgumentType;
import de.z0rdak.yawp.commands.arguments.region.RemoveRegionChildArgumentType;
import de.z0rdak.yawp.config.server.RegionConfig;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.area.IMarkableArea;
import de.z0rdak.yawp.core.area.SphereArea;
import de.z0rdak.yawp.core.region.CuboidRegion;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.core.region.RegionType;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.LocalRegions;
import net.minecraft.command.CommandSource;
import net.minecraft.command.argument.BlockPosArgumentType;
import net.minecraft.command.argument.DimensionArgumentType;
import net.minecraft.command.argument.EntityArgumentType;
import net.minecraft.server.command.CommandManager;
import net.minecraft.server.command.ServerCommandSource;
import net.minecraft.server.network.ServerPlayerEntity;
import net.minecraft.server.world.ServerWorld;
import net.minecraft.text.MutableText;
import net.minecraft.text.Text;
import net.minecraft.util.math.BlockPos;

import java.util.Collections;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.commands.arguments.ArgumentUtil.*;
import static de.z0rdak.yawp.util.ChatComponentBuilder.*;
import static de.z0rdak.yawp.util.MessageSender.sendCmdFeedback;


public class RegionCommands {

    public final static int MIN_BUILD_LIMIT = -64;
    public final static int MAX_BUILD_LIMIT = 320;

    private RegionCommands() {
    }

    public static LiteralArgumentBuilder<ServerCommandSource> build() {
        return literal(LOCAL)
                .then(CommandManager.argument(DIM.toString(), DimensionArgumentType.dimension())
                        .then(CommandManager.argument(LOCAL.toString(), StringArgumentType.word())
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
                                                .then(CommandManager.argument(CHILD.toString(), StringArgumentType.word())
                                                        .suggests((ctx, builder) -> AddRegionChildArgumentType.potentialChildRegions().listSuggestions(ctx, builder))
                                                        .executes(ctx -> addChildren(ctx, getRegionArgument(ctx), getChildRegionArgument(ctx))))))
                                .then(literal(REMOVE)
                                        .then(literal(CHILD)
                                                .then(CommandManager.argument(CHILD.toString(), StringArgumentType.word())
                                                        .suggests((ctx, builder) -> RemoveRegionChildArgumentType.childRegions().listSuggestions(ctx, builder))
                                                        .executes(ctx -> removeChildren(ctx, getDimCacheArgument(ctx), getRegionArgument(ctx), getChildRegionArgument(ctx))))))
                                .then(literal(STATE)
                                        .executes(ctx -> promptLocalRegionState(ctx, getRegionArgument(ctx)))
                                        .then(literal(ALERT)
                                                .executes(ctx -> CommandUtil.setAlertState(ctx, getRegionArgument(ctx), getRegionArgument(ctx).isMuted()))
                                                .then(CommandManager.argument(ALERT.toString(), BoolArgumentType.bool())
                                                        .executes(ctx -> CommandUtil.setAlertState(ctx, getRegionArgument(ctx), getAlertArgument(ctx)))))
                                        .then(literal(ENABLE)
                                                .executes(ctx -> CommandUtil.setActiveState(ctx, getRegionArgument(ctx), !getRegionArgument(ctx).isActive()))
                                                .then(CommandManager.argument(ENABLE.toString(), BoolArgumentType.bool())
                                                        .executes(ctx -> CommandUtil.setActiveState(ctx, getRegionArgument(ctx), getEnableArgument(ctx)))))
                                        .then(literal(PRIORITY)
                                                .then(CommandManager.argument(PRIORITY.toString(), IntegerArgumentType.integer())
                                                        .executes(ctx -> setPriority(ctx, getRegionArgument(ctx), getPriorityArgument(ctx))))
                                                .then(literal(INC)
                                                        .then(CommandManager.argument(PRIORITY.toString(), IntegerArgumentType.integer())
                                                                .executes(ctx -> setPriority(ctx, getRegionArgument(ctx), getPriorityArgument(ctx), 1))))
                                                .then(literal(DEC)
                                                        .then(CommandManager.argument(PRIORITY.toString(), IntegerArgumentType.integer())
                                                                .executes(ctx -> setPriority(ctx, getRegionArgument(ctx), getPriorityArgument(ctx), -1))))))
                                .then(literal(AREA)
                                        .executes(ctx -> promptRegionAreaInfo(ctx.getSource(), getRegionArgument(ctx)))
                                        .then(literal(SET)
                                                .then(CommandManager.literal(AreaType.CUBOID.areaType)
                                                        .then(CommandManager.argument(POS1.toString(), BlockPosArgumentType.blockPos())
                                                                .then(CommandManager.argument(POS2.toString(), BlockPosArgumentType.blockPos())
                                                                        .executes(ctx -> setCuboidArea(ctx, getRegionArgument(ctx), BlockPosArgumentType.getLoadedBlockPos(ctx, POS1.toString()), BlockPosArgumentType.getLoadedBlockPos(ctx, POS2.toString()))))))
                                                .then(CommandManager.literal(AreaType.SPHERE.areaType)
                                                        .then(CommandManager.argument(CENTER_POS.toString(), BlockPosArgumentType.blockPos())
                                                                .then(CommandManager.argument(RADIUS_POS.toString(), BlockPosArgumentType.blockPos())
                                                                        .executes(ctx -> setSphereArea(ctx, getRegionArgument(ctx), BlockPosArgumentType.getLoadedBlockPos(ctx, CENTER_POS.toString()), BlockPosArgumentType.getLoadedBlockPos(ctx, RADIUS_POS.toString()))))))
                                                .then(CommandManager.literal(AreaType.SPHERE.areaType)
                                                        .then(CommandManager.argument(CENTER_POS.toString(), BlockPosArgumentType.blockPos())
                                                                .then(CommandManager.argument(RADIUS.toString(), IntegerArgumentType.integer(0))
                                                                        .executes(ctx -> setSphereArea(ctx, getRegionArgument(ctx), BlockPosArgumentType.getLoadedBlockPos(ctx, CENTER_POS.toString()), IntegerArgumentType.getInteger(ctx, RADIUS.toString()))))))
                                        )
                                        .then(literal(EXPAND)
                                                .then(CommandManager.literal(AreaType.CUBOID.areaType)
                                                        .executes(ctx -> expandCuboid(ctx, getRegionArgument(ctx), MIN_BUILD_LIMIT, MAX_BUILD_LIMIT))
                                                        .then(CommandManager.argument(Y_MIN.toString(), IntegerArgumentType.integer(MIN_BUILD_LIMIT, MAX_BUILD_LIMIT))
                                                                .then(CommandManager.argument(Y_MAX.toString(), IntegerArgumentType.integer(MIN_BUILD_LIMIT, MAX_BUILD_LIMIT))
                                                                        .executes(ctx -> expandCuboid(ctx, getRegionArgument(ctx), IntegerArgumentType.getInteger(ctx, Y_MIN.toString()), IntegerArgumentType.getInteger(ctx, Y_MAX.toString()))))))
                                                .then(CommandManager.literal(AreaType.SPHERE.areaType)
                                                        .executes(ctx -> expandSphere(ctx, getRegionArgument(ctx), 1))
                                                        .then(CommandManager.argument(EXPANSION.toString(), IntegerArgumentType.integer())
                                                                .executes(ctx -> expandSphere(ctx, getRegionArgument(ctx), IntegerArgumentType.getInteger(ctx, EXPANSION.toString())))))
                                        )
                                        .then(literal(TELEPORT)
                                                .then(CommandManager.literal(SET.toString())
                                                        .then(CommandManager.argument(TARGET.toString(), BlockPosArgumentType.blockPos())
                                                                .executes(ctx -> setTeleportPos(ctx, getRegionArgument(ctx), BlockPosArgumentType.getLoadedBlockPos(ctx, TARGET.toString())))))
                                        )
                                        .then(literal(TELEPORT)
                                                .executes(ctx -> teleport(ctx, getRegionArgument(ctx)))
                                                .then(CommandManager.argument(PLAYER.toString(), EntityArgumentType.player())
                                                        .executes(ctx -> teleport(ctx, getRegionArgument(ctx), getPlayerArgument(ctx)))))
                                ).then(literal(RENAME)
                                        .then(CommandManager.argument(NAME.toString(), StringArgumentType.word())
                                                .suggests((ctx, builder) -> CommandSource.suggestMatching(Collections.singletonList(getRegionArgument(ctx).getName()), builder))
                                                .executes(ctx -> renameRegion(ctx, getRegionArgument(ctx), getRegionNameArgument(ctx), getDimCacheArgument(ctx))))
                                )
                        )
                );
    }

    private static int expandSphere(CommandContext<ServerCommandSource> ctx, IMarkableRegion region, int expansion) {
        SphereArea expand = SphereArea.expand((SphereArea) region.getArea(), expansion);
        return updateArea(ctx, region, expand);
    }

    private static int setSphereArea(CommandContext<ServerCommandSource> ctx, IMarkableRegion region, BlockPos center, int radius) {
        BlockPos newRadius = center.add(0, radius, 0);
        return setSphereArea(ctx, region, center, newRadius);
    }

    private static int setSphereArea(CommandContext<ServerCommandSource> ctx, IMarkableRegion region, BlockPos center, BlockPos radiusPos) {
        return updateArea(ctx, region, new SphereArea(center, radiusPos));
    }

    private static int setCuboidArea(CommandContext<ServerCommandSource> ctx, IMarkableRegion region, BlockPos p1, BlockPos p2) {
        return updateArea(ctx, region, new CuboidArea(p1, p2));
    }

    private static int expandCuboid(CommandContext<ServerCommandSource> ctx, IMarkableRegion region, int yMin, int yMax) {
        CuboidArea expand = CuboidArea.expand((CuboidArea) region.getArea(), yMin, yMax);
        return updateArea(ctx, region, expand);
    }

    private static int updateArea(CommandContext<ServerCommandSource> ctx, IMarkableRegion region, IMarkableArea area) {
        try {
            AreaType prevAreaType = region.getArea().getAreaType();
            AreaType newAreaType = area.getAreaType();
            IProtectedRegion parent = region.getParent();
            // TODO: Implement a contains method for regions, with dimensional always returning true if dim is the same
            // IMarkableRegions would use the area contains method

            ServerPlayerEntity player;
            try {
                player = ctx.getSource().getPlayerOrThrow();
            } catch (CommandSyntaxException e) {
                player = null;
            }
            if (RegionEvents.UPDATE_AREA.invoker().updateArea(new RegionEvent.UpdateArea(region, area, player))) {
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
                                    MutableText updateAreaFailMsg = Text.translatableWithFallback("cli.msg.info.region.area.area.update.fail.boundaries", "Parent region %s does not fully contain new are for region %s", buildRegionInfoLink(parent), buildRegionInfoLink(region));
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
                MutableText updateAreaFailMsg = Text.translatableWithFallback("cli.msg.info.region.area.update.type.change", "AreaType for %s changed from %s to %s", buildRegionInfoLink(region), prevAreaType, newAreaType);
                sendCmdFeedback(ctx.getSource(), updateAreaFailMsg);
            }
            region.setArea(area);
            RegionDataManager.save();
            MutableText updateAreaMsg = Text.translatableWithFallback("cli.msg.info.region.area.area.update", "Updated %s for region %s", buildRegionAreaLink(region), buildRegionInfoLink(region));
            sendCmdFeedback(ctx.getSource(), updateAreaMsg);
            return 0;
        } catch (Exception ex) {
            YetAnotherWorldProtector.LOGGER.error("Failed to update area: {}", ex.getMessage());
            return 1;
        }
    }

    private static int renameRegion(CommandContext<ServerCommandSource> ctx, IMarkableRegion region, String regionName, DimensionRegionCache dimCache) {
        if (region.getName().equals(regionName)) {
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.dim.info.region.create.name.no-change", regionName));
            return 1;
        }
        int res = RegionDataManager.get().isValidRegionName(dimCache.getDimensionalRegion().getDim(), regionName);
        if (res == -1) {
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.dim.info.region.create.name.invalid", regionName));
            return res;
        }
        if (res == 1) {
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.dim.info.region.create.name.exists", "Dimension %s already contains region with name %s", dimCache.getDimensionalRegion().getName(), buildRegionInfoLink(dimCache.getRegion(regionName))));
            return res;
        }
        try {
            ServerPlayerEntity player;
            try {
                player = ctx.getSource().getPlayerOrThrow();
            } catch (CommandSyntaxException e) {
                player = null;
            }
            if (RegionEvents.RENAME_REGION.invoker().renameRegion(new RegionEvent.RenameRegion(region, region.getName(), regionName, player))) {
                return 0;
            }
            String oldName = region.getName();
            dimCache.renameRegion(region, regionName);
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.dim.info.region.create.name.success", "Changed name of region %s from '%s' to '%s'", buildRegionInfoLink(region), oldName, regionName));
            RegionDataManager.save();
            return 0;
        } catch (IllegalArgumentException ex) {
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.dim.info.region.create.name.exists", "Dimension %s already contains region with name %s", dimCache.getDimensionalRegion().getName(), buildRegionInfoLink(dimCache.getRegion(regionName))));
            return 1;
        }
    }

    // TODO: Test removing child does not set priority correct with overlapping regions
    public static int removeChildren(CommandContext<ServerCommandSource> ctx, DimensionRegionCache dimCache, IProtectedRegion parent, IProtectedRegion child) {
        if (parent.hasChild(child)) {
            parent.removeChild(child);
            dimCache.getDimensionalRegion().addChild(child);
            LocalRegions.ensureLowerRegionPriorityFor((CuboidRegion) child, RegionConfig.getDefaultPriority());
            RegionDataManager.save();
            MutableText parentLink = buildRegionInfoLink(parent);
            MutableText notLongerChildLink = buildRegionInfoLink(child);
            MutableText dimensionalLink = buildRegionInfoLink(dimCache.getDimensionalRegion());
            MutableText undoLink = buildRegionActionUndoLink(ctx.getInput(), REMOVE, ADD);
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.info.region.children.remove", "Removed child '%s' from region %s", notLongerChildLink, parentLink).append(" ")
                    .append(undoLink));
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.info.region.parent.clear", "Reset default parent for %s back to %s", notLongerChildLink, dimensionalLink));
            return 0;
        }
        // should not happen, due to RemoveRegionChildArgumentType should only provide valid child regions
        return -1;
    }

    public static int addChildren(CommandContext<ServerCommandSource> ctx, IMarkableRegion parent, IMarkableRegion child) {
        boolean parentIsNotNullAndDimension = child.getParent() != null && child.getParent().getRegionType() == RegionType.DIMENSION;
        if (!parent.hasChild(child) && parentIsNotNullAndDimension) {
            child.getParent().removeChild(child);
            parent.addChild(child);
            LocalRegions.ensureHigherRegionPriorityFor(child, parent.getPriority() + 1);
            RegionDataManager.save();
            MutableText parentLink = buildRegionInfoLink(parent);
            MutableText childLink = buildRegionInfoLink(child);
            MutableText undoLink = buildRegionActionUndoLink(ctx.getInput(), ADD, REMOVE);
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.info.region.children.add", "Added child %s to region %s", childLink, parentLink).append(" ")
                    .append(undoLink));
            return 0;
        }
        // should not happen, due to AddRegionChildArgumentType should only provide valid child regions
        return -1;
    }

    public static int setPriority(CommandContext<ServerCommandSource> ctx, IMarkableRegion region, int priority, int factor) {
        long newValue = (long) region.getPriority() + ((long) priority * factor);
        if (Integer.MAX_VALUE - newValue > 0) {
            return setPriority(ctx, region, (int) newValue);
        } else {
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.warn.region.state.priority.set.invalid", "Unable to change priority for region %s: %s is to high/low", buildRegionInfoLink(region), newValue));
            return -1;
        }
    }

    /**
     * Attempt to set new priority for the given region. <br>
     * Fails if region priority is used by an overlapping region at same hierarchy level.
     */
    public static int setPriority(CommandContext<ServerCommandSource> ctx, IMarkableRegion region, int priority) {
        IProtectedRegion parent = region.getParent();
        if (parent instanceof IMarkableRegion) {
            int parentPriority = ((IMarkableRegion) parent).getPriority();
            if (parentPriority >= priority) {
                MutableText updatePriorityFailMsg = Text.translatableWithFallback("cli.msg.info.region.state.priority.set.fail.to-low", "Unable to set priority for region %s. The priority is not higher than its parents priority", buildRegionInfoLink(region));
                sendCmdFeedback(ctx.getSource(), updatePriorityFailMsg);
                return 1;
            }
        }
        CuboidRegion cuboidRegion = (CuboidRegion) region;
        boolean existRegionWithSamePriority = LocalRegions.hasAnyRegionWithSamePriority(cuboidRegion, priority);
        if (existRegionWithSamePriority) {
            MutableText updatePriorityFailMsg = Text.translatableWithFallback("cli.msg.info.region.state.priority.set.fail.same", "Unable to set priority for region %s. There is already another region with priority %s.", buildRegionInfoLink(region), priority);
            sendCmdFeedback(ctx.getSource(), updatePriorityFailMsg);
            return 1;
        } else {
            int oldPriority = region.getPriority();
            if (oldPriority != priority) {
                region.setPriority(priority);
                RegionDataManager.save();
                MutableText undoLink = buildRegionActionUndoLink(ctx.getInput(), String.valueOf(oldPriority), String.valueOf(priority));
                sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.info.region.state.priority.set.success", "Changed priority for region %s: %s -> %s",
                                buildRegionInfoLink(region), oldPriority, region.getPriority())
                        .append(" ")
                        .append(undoLink));
                return 0;
            } else {
                sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.info.region.state.priority.set.fail.no-change", "Unable to set priority for region %s. The priority is the same.", buildRegionInfoLink(region)));
                return 1;
            }
        }
    }

    /**
     * Prompt the common region state and the local region priority info
     * Priority: n [#][+5][-5]
     */
    private static int promptLocalRegionState(CommandContext<ServerCommandSource> ctx, IMarkableRegion region) {
        CommandUtil.promptRegionState(ctx, region);
        sendCmdFeedback(ctx.getSource(), buildInfoComponent("cli.msg.info.region.state.priority", "Priority", buildRegionPriorityComponent(region)));
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
    private static int promptRegionAreaInfo(ServerCommandSource src, IMarkableRegion region) {
        sendCmdFeedback(src, buildHeader(Text.translatableWithFallback("cli.msg.info.header.of", "== %s of %s ==", buildRegionAreaLink(region), buildRegionInfoLink(region))));
        sendCmdFeedback(src, buildInfoComponent("cli.msg.info.region.area.location", "Location", buildRegionTeleportLink(region, null)));
        sendCmdFeedback(src, buildInfoComponent("cli.msg.info.region.area.area", "Area", buildRegionAreaDetailComponent(region)));
        sendCmdFeedback(src, buildInfoComponent("cli.msg.info.region.area.marked", "Marked Blocks", buildAreaMarkedBlocksTpLinks(region)));
        sendCmdFeedback(src, buildInfoComponent("cli.msg.info.region.area.actions", "Actions", buildRegionAreaActionLinks(region)));
        return 0;
    }

    private static int teleport(CommandContext<ServerCommandSource> ctx, IMarkableRegion region) {
        try {
            ServerPlayerEntity player = ctx.getSource().getPlayerOrThrow();
            return teleport(ctx, region, player);
        } catch (CommandSyntaxException e) {
            YetAnotherWorldProtector.LOGGER.warn("Unable to teleport command source to region. Most likely not a player");
            return -1;
        }
    }

    private static int teleport(CommandContext<ServerCommandSource> ctx, IMarkableRegion region, ServerPlayerEntity player) {
        try {
            ServerPlayerEntity playerEntity = ctx.getSource().getPlayerOrThrow();
            ServerWorld level = ctx.getSource().getServer().getWorld(region.getDim());
            if (level != null) {
                playerEntity.teleport(level, region.getTpTarget().getX(), region.getTpTarget().getY(), region.getTpTarget().getZ(), playerEntity.getYaw(), playerEntity.getPitch());
                return 0;
            } else {
                YetAnotherWorldProtector.LOGGER.error("Error executing teleport command. Level is null.");
                return -1;
            }
        } catch (CommandSyntaxException e) {
            ServerWorld level = ctx.getSource().getServer().getWorld(region.getDim());
            if (level != null) {
                player.teleport(level, region.getTpTarget().getX(), region.getTpTarget().getY(), region.getTpTarget().getZ(), player.getYaw(), player.getPitch());
                return 0;
            }
            YetAnotherWorldProtector.LOGGER.warn("Error executing teleport command.");
            return -1;
        }
    }

    private static int setTeleportPos(CommandContext<ServerCommandSource> ctx, IMarkableRegion region, BlockPos target) {
        if (!region.getTpTarget().equals(target)) {
            region.setTpTarget(target);
            RegionDataManager.save();
            MutableText newTpTargetLink = buildDimensionalBlockTpLink(region.getDim(), target);
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.info.region.area.tp.set.msg", "Set new teleport anchor for %s to %s", buildRegionInfoLink(region), newTpTargetLink));
            return 0;
        }
        return 1;
    }
}
