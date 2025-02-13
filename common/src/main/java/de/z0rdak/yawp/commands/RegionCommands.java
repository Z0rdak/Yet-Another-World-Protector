package de.z0rdak.yawp.commands;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.api.events.region.RegionEvent;
import de.z0rdak.yawp.commands.arguments.ArgumentUtil;
import de.z0rdak.yawp.commands.arguments.region.AddRegionChildArgumentType;
import de.z0rdak.yawp.commands.arguments.region.RegionArgumentType;
import de.z0rdak.yawp.commands.arguments.region.RemoveRegionChildArgumentType;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.area.IMarkableArea;
import de.z0rdak.yawp.core.area.SphereArea;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.core.region.RegionType;
import de.z0rdak.yawp.data.region.DimensionRegionCache;
import de.z0rdak.yawp.data.region.RegionDataManager;
import de.z0rdak.yawp.platform.Services;
import de.z0rdak.yawp.util.LocalRegions;
import de.z0rdak.yawp.util.text.messages.multiline.MultiLineMessage;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.SharedSuggestionProvider;
import net.minecraft.commands.arguments.DimensionArgument;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.commands.arguments.coordinates.BlockPosArgument;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.RelativeMovement;

import java.util.Collections;

import static de.z0rdak.yawp.api.commands.CommandConstants.*;
import static de.z0rdak.yawp.commands.CommandUtil.*;
import static de.z0rdak.yawp.commands.arguments.ArgumentUtil.*;
import static de.z0rdak.yawp.constants.Constants.MAX_BUILD_LIMIT;
import static de.z0rdak.yawp.constants.Constants.MIN_BUILD_LIMIT;
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
                                                                .then(Commands.argument(RADIUS_POS.toString(), BlockPosArgument.blockPos())
                                                                        .executes(ctx -> setSphereArea(ctx, getRegionArgument(ctx), BlockPosArgument.getSpawnablePos(ctx, CENTER_POS.toString()), BlockPosArgument.getSpawnablePos(ctx, RADIUS_POS.toString()))))))
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
                                                                .executes(ctx -> expandSphere(ctx, getRegionArgument(ctx), IntegerArgumentType.getInteger(ctx, EXPANSION.toString())))))
                                        )
                                        .then(literal(TELEPORT)
                                                .then(Commands.literal(SET.toString())
                                                        .then(Commands.argument(TARGET.toString(), BlockPosArgument.blockPos())
                                                                .executes(ctx -> setTeleportPos(ctx, getRegionArgument(ctx), BlockPosArgument.getSpawnablePos(ctx, TARGET.toString())))))
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
            MutableComponent updateAreaMsg = Component.translatableWithFallback("cli.msg.info.region.area.area.update", "Updated %s for region %s", buildRegionAreaLink(region), buildRegionInfoLink(region));
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
        boolean parentIsNotNullAndDimension = child.getParent() != null && child.getParent().getRegionType() == RegionType.DIMENSION;
        if (!parent.hasChild(child) && parentIsNotNullAndDimension) {
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

    private static int teleport(CommandContext<CommandSourceStack> ctx, IMarkableRegion region) {
        try {
            ServerPlayer player = ctx.getSource().getPlayerOrException();
            return teleport(ctx, region, player);
        } catch (CommandSyntaxException e) {
            Constants.LOGGER.warn("Unable to teleport command source to region. Most likely not a player");
            return -1;
        }
    }

    private static int teleport(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, ServerPlayer playerToTeleport) {
        try {
            ServerPlayer player = ctx.getSource().getPlayerOrException();
            ServerLevel level = ctx.getSource().getServer().getLevel(region.getDim());
            if (level != null) {
                player.teleportTo(level, region.getTpTarget().getX(), region.getTpTarget().getY(), region.getTpTarget().getZ(), RelativeMovement.ROTATION, player.getYRot(), player.getXRot());
                return 0;
            } else {
                Constants.LOGGER.error("Error executing teleport command. Level is null.");
                return -1;
            }
        } catch (CommandSyntaxException e) {
            ServerLevel level = ctx.getSource().getServer().getLevel(region.getDim());
            if (level != null) {
                playerToTeleport.teleportTo(level, region.getTpTarget().getX(), region.getTpTarget().getY(), region.getTpTarget().getZ(), RelativeMovement.ROTATION, playerToTeleport.getYRot(), playerToTeleport.getXRot());
                return 0;
            }
            Constants.LOGGER.warn("Error executing teleport command.");
            return -1;
        }
    }

    private static int setTeleportPos(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, BlockPos target) {
        if (!region.getTpTarget().equals(target)) {
            region.setTpTarget(target);
            RegionDataManager.save();
            MutableComponent newTpTargetLink = buildDimensionalBlockTpLink(region.getDim(), target);
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.area.tp.set.msg", "Set new teleport anchor for %s to %s", buildRegionInfoLink(region), newTpTargetLink));
            return 0;
        }
        return 1;
    }
}
