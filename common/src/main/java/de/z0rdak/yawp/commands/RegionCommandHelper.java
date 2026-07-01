package de.z0rdak.yawp.commands;

import com.mojang.brigadier.Command;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.api.FlagRegister;
import de.z0rdak.yawp.api.core.ILevelRegionApi;
import de.z0rdak.yawp.api.core.RegionManager;
import de.z0rdak.yawp.api.core.region.hierarchy.HierarchyValidationResult;
import de.z0rdak.yawp.api.core.region.hierarchy.RegionHierarchy;
import de.z0rdak.yawp.api.events.flag.FlagEvent;
import de.z0rdak.yawp.api.events.region.RegionEvent;
import de.z0rdak.yawp.api.visualization.VisualizationManager;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.area.*;
import de.z0rdak.yawp.core.area.anchors.RegionAnchors;
import de.z0rdak.yawp.core.area.anchors.TeleportAnchor;
import de.z0rdak.yawp.core.area.visuals.BlockDisplayProperties;
import de.z0rdak.yawp.core.area.visuals.DisplayType;
import de.z0rdak.yawp.core.flag.BooleanFlag;
import de.z0rdak.yawp.core.flag.FlagMessage;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.region.*;
import de.z0rdak.yawp.data.region.LevelData;
import de.z0rdak.yawp.platform.Services;
import de.z0rdak.yawp.util.ChatLinkBuilder;
import de.z0rdak.yawp.util.LocalRegions;
import de.z0rdak.yawp.util.text.Messages;
import de.z0rdak.yawp.util.text.messages.multiline.MultiLineMessage;
import de.z0rdak.yawp.util.text.messages.pagination.InvalidPageNumberException;
import de.z0rdak.yawp.util.text.messages.pagination.TeleportAnchorPagination;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Holder;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.resources.Identifier;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Relative;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.block.AirBlock;
import net.minecraft.world.level.block.Block;

import java.util.*;

import static de.z0rdak.yawp.api.MessageSender.sendCmdFeedback;
import static de.z0rdak.yawp.api.MessageSender.sendError;
import static de.z0rdak.yawp.api.commands.CommandConstants.*;
import static de.z0rdak.yawp.commands.arguments.ArgumentUtil.*;
import static de.z0rdak.yawp.util.ChatComponentBuilder.shortBlockPos;
import static de.z0rdak.yawp.util.ChatComponentBuilder.shortBlockPosBracketed;
import static de.z0rdak.yawp.util.ChatLinkBuilder.*;


final class RegionCommandHelper {

    private RegionCommandHelper() {
    }


    public static int resetGlobalRegion(CommandContext<CommandSourceStack> ctx) {
        RegionManager.get().resetGlobal();
        sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.global.reset", "Successfully reset global region", buildRegionInfoLink(getGlobalRegion())));
        return 0;
    }

    public static int nukeDisplayEntities(CommandContext<CommandSourceStack> ctx, ServerLevel level) {
        var entityAmount = VisualizationManager.nukeDisplayEntities(level);
        sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.dim.nuke-display-entities", "Removed all (%s) visualization entities '%s'", entityAmount, level.dimension().identifier().toString()));
        return 0;
    }

    public static int setActiveStateForAllLocal(CommandContext<CommandSourceStack> ctx, LevelData levelData, boolean enable) {
        if (levelData != null) {
            levelData.getLocalList().forEach(region -> region.setIsActive(enable));
            if (enable)
                sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.state.enable.all.set.on.value",
                        "Activates alert for all local regions of %s", ChatLinkBuilder.buildRegionInfoLink(levelData.getDim())));
            else
                sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.state.enable.all.set.off.value",
                        "Deactivated all local regions of %s", ChatLinkBuilder.buildRegionInfoLink(levelData.getDim())));
            RegionManager.get().save(levelData.getDim());
            return 0;
        } else {
            return 1;
        }
    }

    public static int setAlertStateForAllLocal(CommandContext<CommandSourceStack> ctx, LevelData levelData, boolean mute) {
        if (levelData != null) {
            levelData.getLocalList().forEach(region -> region.setIsMuted(mute));
            if (mute)
                sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.state.alert.all.set.on.value",
                        "Activated alert for all local regions of %s", ChatLinkBuilder.buildRegionInfoLink(levelData.getDim())));
            else
                sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.state.alert.all.set.off.value",
                        "Deactivated alert for all local regions of %s", ChatLinkBuilder.buildRegionInfoLink(levelData.getDim())));
            RegionManager.get().save(levelData.getDim());
            return 0;
        } else {
            return 1;
        }
    }


    public static int createRegion(CommandContext<CommandSourceStack> ctx, String regionName, LevelData levelData, IMarkableRegion region, IProtectedRegion parent) {
        int res = levelData.isValidRegionName(regionName);
        if (res == -1) {
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.dim.info.region.create.name.invalid", "Invalid region name supplied: '%s'", regionName));
            return res;
        }
        if (res == 1) {
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.dim.info.region.create.name.exists", "Dimension %s already contains region with name %s", levelData.getDim().getName(), ChatLinkBuilder.buildRegionInfoLink(levelData.getLocal(regionName))));
            return res;
        }
        ServerPlayer player;
        try {
            player = ctx.getSource().getPlayerOrException();
        } catch (CommandSyntaxException e) {
            player = null;
        }

        var regionCreated = new RegionEvent.Create(region, player);
        if (Services.REGION_EVENT_DISPATCHER.post(regionCreated)) {
            return 1;
        }

        Services.REGION_CONFIG.getDefaultFlags().stream()
                .map(FlagRegister::byId)
                .forEach(flag -> region.addFlag(new BooleanFlag(flag)));
        Optional<ILevelRegionApi> dimRegionApi = RegionManager.get().getDimRegionApi(parent.getDim());
        if (dimRegionApi.isPresent()) {
            var api = dimRegionApi.get();
            boolean added = api.addLocalRegion(region);
            if (added) {
                // LocalRegions.ensureHigherRegionPriorityFor(region, Services.REGION_CONFIG.getDefaultPriority());
                RegionManager.get().save(levelData.getDim());
                sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.dim.info.region.create.success", "Successfully created region %s (parent: %s)", ChatLinkBuilder.buildRegionInfoLink(region), ChatLinkBuilder.buildRegionInfoLink(parent)));
                return Command.SINGLE_SUCCESS;
            }
        }
        // TODO error
        return -1;
    }

    public static int createCuboidRegion(CommandContext<CommandSourceStack> ctx, Identifier levelId, String regionName, BlockPos pos1, BlockPos pos2) {
        // TODO Validate levelId
        var dimCache = RegionManager.get().getLevelRegionData(levelId);
        if (dimCache.isPresent()) {
            return createCuboidRegion(ctx, regionName, dimCache.get(), pos1, pos2);
        }
        Constants.LOGGER.error("Error getting dimension cache for {}", ctx.getSource().getLevel().dimension().identifier().toString());
        return -1;
    }

    public static int createSphereRegion(CommandContext<CommandSourceStack> ctx, Identifier levelId, String regionName, BlockPos centerPos, int radius) {
        // TODO validate levelId
        var dimCache = RegionManager.get().getLevelRegionData(levelId);
        if (dimCache.isPresent()) {
            return createSphereRegion(ctx, regionName, dimCache.get(), centerPos, radius);
        }
        Constants.LOGGER.error("Error getting dimension cache for {}", ctx.getSource().getLevel().dimension().identifier().toString());
        return -1;
    }

    public static int createCuboidRegion(CommandContext<CommandSourceStack> ctx, String regionName, BlockPos pos1, BlockPos pos2) {
        Player player = null;
        try {
            player = ctx.getSource().getPlayerOrException();
        } catch (CommandSyntaxException e) {
            sendError(ctx.getSource(), Component.translatableWithFallback("", ""));
            return 1;
        }
        var dimCache = RegionManager.get().getLevelRegionData(player.level().dimension());
        if (dimCache.isPresent()) {
            return createCuboidRegion(ctx, regionName, dimCache.get(), pos1, pos2);
        }
        Constants.LOGGER.error("Error getting dimension cache for {}", ctx.getSource().getLevel().dimension().identifier().toString());
        return -1;
    }

    public static int createSphereRegion(CommandContext<CommandSourceStack> ctx, String regionName, BlockPos centerPos, int radius) {
        Player player = null;
        try {
            player = ctx.getSource().getPlayerOrException();
        } catch (CommandSyntaxException e) {
            sendError(ctx.getSource(), Component.translatableWithFallback("", ""));
            return 1;
        }
        var dimCache = RegionManager.get().getLevelRegionData(player.level().dimension());
        if (dimCache.isPresent()) {
            return createSphereRegion(ctx, regionName, dimCache.get(), centerPos, radius);
        }
        Constants.LOGGER.error("Error getting dimension cache for {}", ctx.getSource().getLevel().dimension().identifier().toString());
        return -1;
    }

    public static int createCuboidRegion(CommandContext<CommandSourceStack> ctx, String regionName, LevelData levelData, BlockPos pos1, BlockPos pos2) {
        var uuid = UUID.randomUUID();
        var parentUuid = LevelData.levelUuid(levelData.getDimKey().identifier());
        IMarkableRegion region = new MarkedRegion(regionName, uuid, parentUuid, new CuboidArea(pos1, pos2), levelData.getDimKey());
        IProtectedRegion parent = levelData.getDim();
        return createRegion(ctx, regionName, levelData, region, parent);
    }

    public static int createSphereRegion(CommandContext<CommandSourceStack> ctx, String regionName, LevelData levelData, BlockPos centerPos, int radius) {
        var uuid = UUID.randomUUID();
        var parentUuid = LevelData.levelUuid(levelData.getDimKey().identifier());
        var region = new MarkedRegion(regionName, uuid, parentUuid, new SphereArea(centerPos, radius), levelData.getDimKey());
        IProtectedRegion parent = levelData.getDim();
        return createRegion(ctx, regionName, levelData, region, parent);
    }


    public static int attemptDeleteRegions(CommandContext<CommandSourceStack> ctx, LevelData dimCache) {
        int amount = dimCache.getLocalNames().size();
        MutableComponent removeAllRegionsLink = ChatLinkBuilder.buildRemoveAllRegionsLink(dimCache);
        sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.dim.region.remove.all.attempt", "Attempt to remove all (%s) regions from dimension %s. Confirm removal by clicking here %s",
                amount, ChatLinkBuilder.buildRegionInfoLink(dimCache.getDim()), removeAllRegionsLink));
        return 0;
    }

    public static int attemptDeleteRegion(CommandContext<CommandSourceStack> ctx, LevelData levelData, IMarkableRegion region) {
        if (levelData.hasLocal(region.getName())) {
            MutableComponent removeRegionLink = ChatLinkBuilder.buildRemoveRegionLink(region);
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.dim.region.remove.attempt", "Attempt to remove region %s from %s. Confirm by clicking here %s",
                    ChatLinkBuilder.buildRegionInfoLink(region), ChatLinkBuilder.buildRegionInfoLink(levelData.getDim()), removeRegionLink));
            return 0;
        }
        return 1;
    }


    public static int deleteRegion(CommandContext<CommandSourceStack> ctx, IMarkableRegion region) {
        var dimCache = RegionManager.get().getLevelRegionData(region.getDim());
        if (dimCache.isPresent()) {
            return deleteRegion(ctx, dimCache.get(), region);
        }
        Constants.LOGGER.error("Error getting dimension cache for region {} in {}", region.getName(), region.getDim().identifier().toString());
        return -1;
    }
    public static int deleteRegion(CommandContext<CommandSourceStack> ctx, LevelData levelData, IMarkableRegion region) {
        ServerPlayer player;
        try {
            player = ctx.getSource().getPlayerOrException();
        } catch (CommandSyntaxException e) {
            player = null;
        }
        if (Services.REGION_EVENT_DISPATCHER.post(new RegionEvent.Remove(region, player))) {
            return 1;
        }
        if (levelData.hasLocal(region.getName())) {
            if (!region.getChildren().isEmpty()) {
                sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.dim.region.remove.fail.hasChildren", "Region %s can't be deleted because it has child regions.", ChatLinkBuilder.buildRegionInfoLink(region)));
                return -1;
            }
            RegionType parentType = region.getParent().getRegionType();
            if (parentType == RegionType.DIMENSION) {
                levelData.removeLocal(region);
                RegionManager.get().save(levelData.getDim());
                sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.dim.region.remove.confirm", "Removed region '%s' from %s", region.getName(), ChatLinkBuilder.buildRegionInfoLink(levelData.getDim())));
                return 0;
            }
            if (parentType == RegionType.LOCAL) {
                sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.dim.region.remove.fail.hasParent", "Region %s can't be deleted because it has a Local Regions as parent.", ChatLinkBuilder.buildRegionInfoLink(region)));
                return 1;
            }
        }
        return 1;
    }

    public static int deleteRegions(CommandContext<CommandSourceStack> ctx, LevelData levelData) {
        int amount = levelData.regionCount();
        levelData.clearLocals();
        RegionManager.get().save(levelData.getDim());
        sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.dim.region.remove.all.confirm", "Removed %s regions from dimension %s", amount, ChatLinkBuilder.buildRegionInfoLink(levelData.getDim())));
        return 0;
    }

    /**
     * Reset groups (players) and state for all local regions in the dimension.<br>
     * This keeps region hierarchy and flags intact. <br>
     * Scenario: You want to keep the local region layout and hierarchy but want to reset players.<br>
     */
    public static int resetLocalRegions(CommandContext<CommandSourceStack> ctx, LevelData levelData) {
        levelData.getLocalList().forEach(region -> {
            region.resetGroups();
            region.setIsActive(true);
            region.setIsMuted(false);
        });
        RegionManager.get().save(levelData.getDim());
        sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.dim.reset.all.confirm", "Successfully reset all local regions in %s", ChatLinkBuilder.buildRegionInfoLink(levelData.getDim())));
        return 0;
    }

    /**
     * Reset groups (players) and state for the dimensional region.<br>
     * This keeps region hierarchy and flags intact.<br>
     */
    public static int resetDimRegion(CommandContext<CommandSourceStack> ctx, LevelData dimCache) {
        DimensionalRegion dimRegion = dimCache.getDim();
        dimRegion.resetGroups();
        dimRegion.setIsActive(true);
        dimRegion.setIsMuted(false);
        dimRegion.getFlags().clear();
        RegionManager.get().save(dimRegion);
        sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.dim.reset.confirm", "Successfully reset dimensional region %s", ChatLinkBuilder.buildRegionInfoLink(dimRegion)));
        return 0;
    }

    public static int setDisplayLightLevel(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, int lightLevel) {
        IMarkableArea area = region.getArea();
        area.getDisplay().setLightLevel(lightLevel);
        RegionManager.get().save(region);
        // TODO: Trigger update event instead
        VisualizationManager.refreshDisplay(region);
        // TODO: I18n
        sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("Set light level for area display for region %s to '%s'", "Set display light level for %s to '%s'", buildRegionInfoLink(region), lightLevel));
        return 0;
    }

    public static int resetDisplaySettings(CommandContext<CommandSourceStack> ctx, IMarkableRegion region) {
        IMarkableArea area = region.getArea();
        area.getDisplay().setHasGlow(BlockDisplayProperties.DEFAULT_GLOW);
        area.getDisplay().setLightLevel(BlockDisplayProperties.DEFAULT_LIGHT_LEVEL);
        RegionManager.get().save(region);
        // TODO: Trigger update event instead
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
            RegionManager.get().save(region);
            // TODO: Trigger update event instead
            VisualizationManager.refreshDisplay(region);
            // TODO: I18n
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("Set display glow effect for region %s to '%s'", "Set display glow effect for %s to '%s'", buildRegionInfoLink(region), Boolean.toString(hasGlow)));
            return 0;
        }
        // else silently just do nothing :-)
        return 1;
    }


    public static int setDisplayBlock(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, Identifier blockRl) {
        Optional<Holder.Reference<Block>> block = BuiltInRegistries.BLOCK.get(blockRl);
        if (block.isPresent() && block.get().value() instanceof AirBlock) {
            // TODO: I18n
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("Not found", "Not found", buildRegionInfoLink(region), blockRl.toString()));
            return -1;
        }
        IMarkableArea area = region.getArea();
        area.getDisplay().setBlockRl(blockRl);
        RegionManager.get().save(region);
        // TODO: Trigger update
        VisualizationManager.refreshDisplay(region);
        // TODO: I18n
        sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("Set display block for region %s to '%s'", "Set display block for %s to '%s'", buildRegionInfoLink(region), blockRl.toString()));
        return 0;
    }

    public static int expandSphere(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, int expansion) {
        SphereArea expand = SphereArea.expand((SphereArea) region.getArea(), expansion);
        return updateArea(ctx, region, expand);
    }

    public static int setSphereArea(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, BlockPos center, int radius) {
        BlockPos newRadius = center.offset(0, radius, 0);
        return setSphereArea(ctx, region, center, newRadius);
    }

    public static int setSphereArea(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, BlockPos center, BlockPos radiusPos) {
        return updateArea(ctx, region, new SphereArea(center, radiusPos));
    }

    public static int setCuboidArea(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, BlockPos p1, BlockPos p2) {
        return updateArea(ctx, region, new CuboidArea(p1, p2));
    }

    public static int expandCuboid(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, int yMin, int yMax) {
        CuboidArea expand = CuboidArea.expand((CuboidArea) region.getArea(), yMin, yMax);
        return updateArea(ctx, region, expand);
    }

    public static int updateArea(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, IMarkableArea area) {
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
            Services.REGION_EVENT_DISPATCHER.post(updateArea);
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
                        }
                    }
                    break;
            }
            if (prevAreaType != newAreaType) {
                MutableComponent updateAreaFailMsg = Component.translatableWithFallback("cli.msg.info.region.area.update.type.change", "AreaType for %s changed from %s to %s", buildRegionInfoLink(region), prevAreaType, newAreaType);
                sendCmdFeedback(ctx.getSource(), updateAreaFailMsg);
            }
            region.setArea(area);
            RegionManager.get().save(region);

            // TODO: Use event to update visualization. But I am currently to lazy to add event handlers for each modloader platform
            VisualizationManager.updateRegionDisplay(region);

            MutableComponent updateAreaMsg = Component.translatableWithFallback("cli.msg.info.region.area.area.update", "Updated %s for %s", buildRegionAreaLink(region), buildRegionInfoLink(region));
            sendCmdFeedback(ctx.getSource(), updateAreaMsg);
            return 0;
        } catch (Exception ex) {
            Constants.LOGGER.error("Failed to update area: {}", ex.getMessage());
            return 1;
        }
    }

    public static int renameRegion(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, String regionName, LevelData levelData) {
        if (region.getName().equals(regionName)) {
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.dim.info.region.create.name.no-change", regionName));
            return 1;
        }
        int res = levelData.isValidRegionName(regionName);
        if (res == -1) {
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.dim.info.region.create.name.invalid", regionName));
            return res;
        }
        if (res == 1) {
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.dim.info.region.create.name.exists", "Dimension %s already contains region with name %s", levelData.getDim().getName(), buildRegionInfoLink(levelData.getLocal(regionName))));
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
            if (Services.REGION_EVENT_DISPATCHER.post(renameRegion)) {
                return 1;
            }
            //if (RegionEvents.RENAME_REGION.invoker().renameRegion(renameRegion)) {
            //    return 0;
            //}
            String oldName = region.getName();
            levelData.renameLocal(region, regionName);
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.dim.info.region.create.name.success", "Changed name of region %s from '%s' to '%s'", buildRegionInfoLink(region), oldName, regionName));
            RegionManager.get().save(region);
            return 0;
        } catch (IllegalArgumentException ex) {
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.dim.info.region.create.name.exists", "Dimension %s already contains region with name %s", levelData.getDim().getName(), buildRegionInfoLink(levelData.getLocal(regionName))));
            return 1;
        }
    }

    // TODO: Test removing child does not set priority correct with overlapping regions
    public static int removeChildren(CommandContext<CommandSourceStack> ctx, LevelData dimCache, IProtectedRegion parent, IMarkableRegion child) {
        if (parent.hasChild(child)) {
            RegionHierarchy.removeParent(child);

            // LocalRegions.ensureLowerRegionPriorityFor(child, Services.REGION_CONFIG.getDefaultPriority());
            RegionManager.get().save(parent.getDim());
            MutableComponent parentLink = buildRegionInfoLink(parent);
            MutableComponent notLongerChildLink = buildRegionInfoLink(child);
            MutableComponent dimensionalLink = buildRegionInfoLink(dimCache.getDim());
            MutableComponent undoLink = buildRegionActionUndoLink(ctx.getInput(), REMOVE, ADD);
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.children.remove", "Removed child '%s' from region %s", notLongerChildLink, parentLink).append(" ")
                    .append(undoLink));
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.parent.clear", "Reset default parent for %s back to %s", notLongerChildLink, dimensionalLink));
            return 0;
        }
        // should not happen, due to RemoveRegionChildArgumentType should only provide valid child regions
        return -1;
    }


    public static int addChildren(CommandContext<CommandSourceStack> ctx, IMarkableRegion parent, IMarkableRegion child) {
        if (!RegionHierarchy.validateParent(child, parent).valid()) {
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.arg.region.owned.invalid.containment", "Region %s is not suitable as parent for %s (does not fully contain child region)", buildRegionInfoLink(parent), buildRegionInfoLink(child)));
            return -1;
        }
        RegionHierarchy.setParent(child, parent);
        // LocalRegions.ensureHigherRegionPriorityFor(child, parent.getPriority() + 1);
        RegionManager.get().save(parent.getDim());
        MutableComponent parentLink = buildRegionInfoLink(parent);
        MutableComponent childLink = buildRegionInfoLink(child);
        MutableComponent undoLink = buildRegionActionUndoLink(ctx.getInput(), ADD, REMOVE);
        sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.children.add", "Added child %s to region %s", childLink, parentLink).append(" ")
                .append(undoLink));
        return Command.SINGLE_SUCCESS;
    }

    public static int setPriority(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, int priority, int factor) {
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
    public static int setPriority(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, int priority) {
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
                RegionManager.get().save(region);
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

    public static int showRegion(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, DisplayType displayType, Identifier blockRl) {
        return showRegion(ctx, region, displayType, blockRl, region.getArea().getDisplay().hasGlow());
    }

    public static int showRegion(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, DisplayType displayType, Identifier blockRl, boolean glow) {
        return showRegion(ctx, region, displayType, blockRl, glow, region.getArea().getDisplay().lightLevel());
    }

    public static int showRegion(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, DisplayType displayType, Identifier blockRl, boolean glow, int lightLevel) {
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

    public static int promptDisplaySettings(CommandContext<CommandSourceStack> ctx, IMarkableRegion region) {
        MultiLineMessage.send(ctx.getSource(), MultiLineMessage.displaySettingsInfo(region));
        return 0;
    }

    public static int promptVisualizationOptions(CommandContext<CommandSourceStack> ctx, IMarkableRegion region) {
        MultiLineMessage.send(ctx.getSource(), MultiLineMessage.visualizationOptions(region));
        return 0;
    }

    public static int promptTeleportAnchorPagination(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, int pageNo) {
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

    public static int updateTeleportAnchor(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, BlockPos pos, String name) {
        RegionAnchors tpAnchors = region.getTpAnchors();
        var hasAnchor = tpAnchors.hasAnchor(name);
        if (!hasAnchor && !isValidName(name)) {
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.tp-anchor.fail-msg.invalid-name", "Teleport Anchor name is invalid. Must be alphanumeric and between 3 and 50 letters.", name, buildRegionInfoLink(region)));
            return -1;
        }
        if (!region.getArea().contains(pos)) {
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.tp-anchor.fail-msg.not-contained", "Region %s doesn't contain %s - invalid position for teleport anchor supplied.", buildRegionInfoLink(region), shortBlockPosBracketed(pos), name));
            return -1;
        }
        if (tpAnchors.hasAnchorWithPos(pos)) {
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.tp-anchor.fail-msg.duplicate-pos", "Region %s already contains a Teleport Anchor with the same pos (%s).", buildRegionInfoLink(region), shortBlockPosBracketed(pos)));
            return -1;
        }
        if (tpAnchors.hasAnchor(name, pos)) {
            // they are the same
            return 0;
        }
        tpAnchors.addOrUpdate(name, pos);
        RegionManager.get().save(region);

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

    public static int removeTeleportAnchor(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, String name) {
        RegionAnchors tpAnchors = region.getTpAnchors();
        if (!tpAnchors.hasAnchor(name)) {
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.tp-anchor.fail-msg.not-existent", "Teleport anchor '%s' does not exist in %s", name, buildRegionInfoLink(region)));
            return -1;
        }
        TeleportAnchor anchor = tpAnchors.getTpAnchor(name);
        tpAnchors.removeTpAnchor(name);
        RegionManager.get().save(region);
        // TODO: Trigger update - if tpAnchor is currently visualized, it should be removed
        var blockTpLink = ChatLinkBuilder.buildDimensionalBlockTpLink(region.getDim(), anchor.getPos(), Component.literal(shortBlockPos(anchor.getPos())));
        sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.tp-anchor.removed.msg", "Removed teleport anchor '%s' (at %s ) from %s", name, blockTpLink, buildRegionInfoLink(region)));
        return 0;
    }

    public static boolean isValidName(String name) {
        return name != null
                && name.length() >= 4
                && name.length() <= 50
                && name.matches("^[a-zA-Z0-9][a-zA-Z0-9_-]*$");
    }

    public static int renameTeleportAnchor(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, String name, String newName) {
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
        RegionManager.get().save(region);
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
    public static int promptRegionAreaInfo(CommandContext<CommandSourceStack> ctx, IMarkableRegion region) {
        MultiLineMessage.send(ctx.getSource(), MultiLineMessage.areaInfo(region));
        return 0;
    }

    public static int teleport(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, String tpAnchorName) {
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

    public static int teleport(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, String tpAnchorName, ServerPlayer playerToTeleport) {
        TeleportAnchor tpAnchor = region.getTpAnchors().getTpAnchor(tpAnchorName);
        BlockPos tpPos = tpAnchor.getPos();
        try {
            ServerPlayer player = ctx.getSource().getPlayerOrException();
            ServerLevel level = ctx.getSource().getServer().getLevel(region.getDim());
            if (level != null) {
                player.teleportTo(level, tpPos.getX(), tpPos.getY(), tpPos.getZ(), Relative.ROTATION, player.getYRot(), player.getXRot(), true);
                return 0;
            } else {
                Constants.LOGGER.error("Error executing teleport command. Level is null.");
                sendCmdFeedback(ctx.getSource(), Component.literal("TODO"));
                return -1;
            }
        } catch (CommandSyntaxException e) {
            ServerLevel level = ctx.getSource().getServer().getLevel(region.getDim());
            if (level != null) {
                playerToTeleport.teleportTo(level, tpPos.getX(), tpPos.getY(), tpPos.getZ(), Relative.ROTATION, playerToTeleport.getYRot(), playerToTeleport.getXRot(), true);
                return 0;
            }
            Constants.LOGGER.warn("Error executing teleport command.");
            sendCmdFeedback(ctx.getSource(), Component.literal("TODO"));
            return -1;
        }
    }

    public static int showTpAnchor(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, String tpAnchorName) {
        if (!region.getTpAnchors().hasAnchor(tpAnchorName)) {
            // TODO
            sendCmdFeedback(ctx.getSource(), Component.literal("TODO"));
            return -1;
        }
        TeleportAnchor tpAnchor = region.getTpAnchors().getTpAnchor(tpAnchorName);
        VisualizationManager.showTpAnchor(region, tpAnchor);
        return 0;
    }

    public static int hideTpAnchor(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, String tpAnchorName) {
        if (!region.getTpAnchors().hasAnchor(tpAnchorName)) {
            // TODO
            sendCmdFeedback(ctx.getSource(), Component.literal("TODO"));
            return -1;
        }
        TeleportAnchor tpAnchor = region.getTpAnchors().getTpAnchor(tpAnchorName);
        VisualizationManager.hideTpAnchor(region, tpAnchor);
        return 0;
    }

    public static List<String> flagMsgExamples() {
        final int amountOfExamples = 10;
        List<String> examples = new ArrayList<>(amountOfExamples);
        for (int i = 0; i < amountOfExamples; i++) {
            examples.add(Component.translatableWithFallback("cli.flag.msg.text.example." + i, "<Your flag message here>").getString());
        }
        return examples;
    }

    public static int promptFlagInfo(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, IFlag flag) {
        if (flag == null) return 1;
        MultiLineMessage.send(ctx.getSource(), MultiLineMessage.flagDetail(region, flag));
        return 0;
    }

    public static int setFlagMuteState(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, IFlag regionFlag) {
        if (regionFlag == null) return 1;
        if (region.containsFlag(regionFlag.getName())) {
            IFlag flag = region.getFlag(regionFlag.getName());
            return setFlagMuteState(ctx, region, flag, !flag.getFlagMsg().isMuted());
        } else {
            MutableComponent hint = Component.translatableWithFallback("cli.msg.info.region.flag.add-hint", "Add flag by clicking: %s", buildSuggestAddFlagLink(region));
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.flag.not-present", "Region %s does not contain flag '%s'. %",
                    buildRegionInfoLink(region), regionFlag.getName(), hint));
            return 1;
        }
    }

    public static int setFlagMuteState(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, IFlag flag, boolean setMuted) {
        if (flag == null) return 1;
        flag.getFlagMsg().mute(setMuted);
        String muteState = flag.getFlagMsg().isMuted() ? "on" : "off";
        MutableComponent infoMsg = Component.translatableWithFallback("cli.flag.msg.mute.success.text", "Set mute state of %s to: '%s'",
                buildFlagInfoLink(region, flag), muteState);
        MutableComponent undoLink = buildRegionActionUndoLink(ctx.getInput(), String.valueOf(!setMuted), String.valueOf(setMuted));
        MutableComponent msg = Messages.substitutable("%s %s", infoMsg, undoLink);
        sendCmdFeedback(ctx.getSource(), msg);
        RegionManager.get().save(region);
        return 0;

    }

    public static int setRegionFlagMsg(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, IFlag flag, String flagMsgStr) {
        if (flag == null) return 1;
        String oldFlagMsg = flag.getFlagMsg().msg();

        ServerPlayer player;
        try {
            player = ctx.getSource().getPlayerOrException();
        } catch (CommandSyntaxException e) {
            player = null;
        }

        FlagEvent.UpdateFlagMessage editMsgEvent = new FlagEvent.UpdateFlagMessage(player, region, flag, flagMsgStr);
        Services.FLAG_EVENT_DISPATCHER.post(editMsgEvent);

        FlagMessage flagMsg = new FlagMessage(flagMsgStr, flag.getFlagMsg().isMuted());
        flag.setFlagMsg(flagMsg);
        MutableComponent infoMsg = Component.translatableWithFallback("cli.flag.msg.msg.success.text", "Set message of %s to: '%s'",
                buildFlagInfoLink(region, flag), flagMsgStr);
        MutableComponent undoLink = buildRegionActionUndoLink(ctx.getInput(), flagMsgStr, oldFlagMsg);
        MutableComponent msg = Messages.substitutable("%s %s", infoMsg, undoLink);
        sendCmdFeedback(ctx.getSource(), msg);
        RegionManager.get().save(region);
        return 0;
    }

    public static int setFlagState(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, IFlag regionFlag) {
        if (regionFlag == null) return 1;
        if (region.containsFlag(regionFlag.getName())) {
            IFlag flag = region.getFlag(regionFlag.getName());
            if (flag.getState() == FlagState.ALLOWED || flag.getState() == FlagState.DENIED) {
                return setFlagState(ctx, region, regionFlag, FlagState.invert(flag.getState()));
            }
            if (flag.getState() == FlagState.DISABLED) {
                return setFlagState(ctx, region, regionFlag, FlagState.DENIED);
            }
            return setFlagState(ctx, region, regionFlag, flag.getState());
        } else {
            MutableComponent hint = Component.translatableWithFallback("cli.msg.info.region.flag.add-hint", "Add flag by clicking: %s", buildSuggestAddFlagLink(region));
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.flag.not-present", "Region %s does not contain flag '%s'. %",
                    buildRegionInfoLink(region), regionFlag.getName(), hint));
            return 1;
        }
    }

    public static int setFlagState(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, IFlag flag, FlagState flagState) {
        if (flag == null) return 1;
        FlagState oldState = flag.getState();
        flag.setState(flagState);
        MutableComponent undoLink = buildRegionActionUndoLink(ctx.getInput(), flagState.name, oldState.name);
        MutableComponent infoMsg = Component.translatableWithFallback("cli.flag.state.success.text", "Set flag state of %s to: '%s'",
                buildFlagInfoLink(region, flag), flag.getState().name);
        MutableComponent msg = Messages.substitutable("%s %s", infoMsg, undoLink);
        sendCmdFeedback(ctx.getSource(), msg);
        RegionManager.get().save(region);
        return 0;

    }

    public static int setOverride(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, IFlag regionFlag) {
        if (regionFlag == null) return 1;
        if (region.containsFlag(regionFlag.getName())) {
            IFlag flag = region.getFlag(regionFlag.getName());
            return setOverride(ctx, region, flag, !flag.doesOverride());
        } else {
            MutableComponent hint = Component.translatableWithFallback("cli.msg.info.region.flag.add-hint", "Add flag by clicking: %s", buildSuggestAddFlagLink(region));
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.flag.not-present", "Region %s does not contain flag '%s'. %",
                    buildRegionInfoLink(region), regionFlag.getName(), hint));
            return 1;
        }
    }

    public static int setOverride(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, IFlag flag, boolean override) {
        if (flag == null) return 1;
        flag.setOverride(override);
        String overrideState = flag.doesOverride() ? "on" : "off";
        MutableComponent infoMsg = Component.translatableWithFallback("cli.flag.override.success.text", "Set flag override for %s to %s",
                buildFlagInfoLink(region, flag), overrideState);
        MutableComponent undoLink = buildRegionActionUndoLink(ctx.getInput(), String.valueOf(!override), String.valueOf(override));
        MutableComponent msg = Messages.substitutable("%s %s", infoMsg, undoLink);
        sendCmdFeedback(ctx.getSource(), msg);
        RegionManager.get().save(region);
        return 0;
    }

    public static int showChildren(CommandContext<CommandSourceStack> ctx, IProtectedRegion region) {
        var children = region.getChildren().values();
        if (children.isEmpty()) {
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("", "%s has no child regions.", buildRegionInfoLink(region)));
            return Command.SINGLE_SUCCESS;
        }

        MutableComponent msg = Component.literal("Children of ")
                .append(buildRegionInfoLink(region))
                .append(": ");
        boolean first = true;
        for (IProtectedRegion child : children) {
            if (!first) {
                msg.append(", ");
            }
            msg.append(buildRegionInfoLink(child));
            first = false;
        }
        sendCmdFeedback(ctx.getSource(), msg);
        return Command.SINGLE_SUCCESS;
    }

    public static int showParent(CommandContext<CommandSourceStack> ctx, IProtectedRegion region) {
        IProtectedRegion parent = region.getParent();
        if (parent == null || parent == region) {
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("", "%s has no parent.", buildRegionInfoLink(region)));
            return Command.SINGLE_SUCCESS;
        }
        sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("", "Parent of %s: %s", buildRegionInfoLink(region), buildRegionInfoLink(parent)));
        return Command.SINGLE_SUCCESS;
    }

    public static int showHierarchy(CommandContext<CommandSourceStack> ctx, IProtectedRegion region) {
        MutableComponent msg = Component.empty();
        appendHierarchy(msg, region, "", true);
        sendCmdFeedback(ctx.getSource(), msg);
        return Command.SINGLE_SUCCESS;
    }

    private static final String PIPE = "│  ";
    private static final String TEE = "├─ ";
    private static final String ELBOW = "└─ ";

    public static int showHierarchyTest(CommandContext<CommandSourceStack> ctx, IProtectedRegion region) {
        MutableComponent out = Component.literal("");
        out.append(buildRegionInfoLink(region));
        List<IProtectedRegion> children = new ArrayList<>(region.getChildren().values());
        // optional: stable output
        children.sort(Comparator.comparing(IProtectedRegion::getName));
        for (int i = 0; i < children.size(); i++) {
            boolean last = (i == children.size() - 1);
            appendTree(out, children.get(i), "", last);
        }
        sendCmdFeedback(ctx.getSource(), out);
        return Command.SINGLE_SUCCESS;
    }

    private static void appendTree(MutableComponent out, IProtectedRegion node, String prefix, boolean isLast) {
        out.append("\n")
                .append(prefix)
                .append(isLast ? ELBOW : TEE)
                .append(buildRegionInfoLink(node));
        List<IProtectedRegion> children = new ArrayList<>(node.getChildren().values());
        children.sort(Comparator.comparing(IProtectedRegion::getName));
        for (int i = 0; i < children.size(); i++) {
            boolean last = (i == children.size() - 1);
            appendTree(out, children.get(i), prefix + (isLast ? "   " : PIPE), last);
        }
    }

    private static void appendHierarchy(MutableComponent out, IProtectedRegion region, String indent, boolean root) {
        if (!root) {
            out.append("\n");
        }
        out.append(indent).append(buildRegionInfoLink(region));
        var children = new ArrayList<>(region.getChildren().values());
        for (int i = 0; i < children.size(); i++) {
            boolean last = i == children.size() - 1;
            appendHierarchy(out, children.get(i), indent + (last ? "   " : "│  "), false);
        }
    }


    public static int showPath(CommandContext<CommandSourceStack> ctx, IProtectedRegion region) {
        List<IProtectedRegion> path = RegionHierarchy.pathToRoot(region);
        Collections.reverse(path);
        MutableComponent msg = Component.literal("Path: ");
        for (int i = 0; i < path.size(); i++) {
            if (i > 0) {
                msg.append(" -> ");
            }
            msg.append(buildRegionInfoLink(path.get(i)));
        }
        sendCmdFeedback(ctx.getSource(), msg);
        return Command.SINGLE_SUCCESS;
    }

    public static int detachParent(CommandContext<CommandSourceStack> ctx, IProtectedRegion child) {
        IProtectedRegion oldParent = child.getParent();
        try {
            RegionHierarchy.detach(child);
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("", "Detached %s from %s.", buildRegionInfoLink(child), buildRegionInfoLink(oldParent)));
            RegionManager.get().save(child);
            return Command.SINGLE_SUCCESS;
        } catch (IllegalArgumentException ex) {
            sendError(ctx.getSource(), Component.literal(ex.getMessage()));
            return 0;
        }
    }

    public static int attachChild(CommandContext<CommandSourceStack> ctx, IProtectedRegion parent, IProtectedRegion child) {
        try {
            RegionHierarchy.attachChild(parent, child);
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("", "Attached child %s to %s.", buildRegionInfoLink(child), buildRegionInfoLink(parent)));
            RegionManager.get().save(child);
            return Command.SINGLE_SUCCESS;
        } catch (IllegalArgumentException ex) {
            sendError(ctx.getSource(), Component.literal(ex.getMessage()));
            return 0;
        }
    }

    public static int detachChild(CommandContext<CommandSourceStack> ctx, IProtectedRegion parent, IProtectedRegion child) {
        try {
            RegionHierarchy.detachChild(parent, child);
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("", "Detached child %s from %s.", buildRegionInfoLink(child), buildRegionInfoLink(parent)));
            RegionManager.get().save(child);
            return Command.SINGLE_SUCCESS;
        } catch (IllegalArgumentException ex) {
            sendError(ctx.getSource(), Component.literal(ex.getMessage()));
            return 0;
        }
    }


    public static int attachParent(CommandContext<CommandSourceStack> ctx, IProtectedRegion child, IProtectedRegion parent) {
        try {
            RegionHierarchy.attach(child, parent);
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("", "Attached %s to parent %s.", buildRegionInfoLink(child), buildRegionInfoLink(parent)));
            RegionManager.get().save(child);
            return Command.SINGLE_SUCCESS;
        } catch (IllegalArgumentException ex) {
            sendError(ctx.getSource(), Component.literal(ex.getMessage()));
            return 0;
        }
    }

    public static int addSubRegion(CommandContext<CommandSourceStack> ctx, IProtectedRegion parent, IProtectedRegion child) {
        HierarchyValidationResult result = RegionHierarchy.validateParent(child, parent);
        if (!result.valid()) {
            sendError(ctx.getSource(), Component.translatableWithFallback("","Cannot add region %s", result.reason()));
            return 0;
        }
        RegionHierarchy.setParent(child, parent);
        sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("","Added region %s to %s", buildRegionInfoLink(child), buildRegionInfoLink(parent) ));
        return Command.SINGLE_SUCCESS;
    }
}
