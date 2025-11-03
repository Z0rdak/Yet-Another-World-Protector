package de.z0rdak.yawp.commands;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.api.commands.CommandConstants;
import de.z0rdak.yawp.api.core.RegionManager;
import de.z0rdak.yawp.api.events.region.RegionEvent;
import de.z0rdak.yawp.commands.arguments.region.ContainingOwnedRegionArgumentType;
import de.z0rdak.yawp.commands.arguments.region.RegionArgumentType;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.area.SphereArea;
import de.z0rdak.yawp.core.flag.BooleanFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.*;
import de.z0rdak.yawp.data.region.LevelRegionData;
import de.z0rdak.yawp.platform.Services;
import de.z0rdak.yawp.util.ChatLinkBuilder;
import de.z0rdak.yawp.util.LocalRegions;
import de.z0rdak.yawp.util.text.messages.multiline.MultiLineMessage;
import de.z0rdak.yawp.util.text.messages.pagination.InvalidPageNumberException;
import de.z0rdak.yawp.util.text.messages.pagination.RegionsInDimensionPagination;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.SharedSuggestionProvider;
import net.minecraft.commands.arguments.DimensionArgument;
import net.minecraft.commands.arguments.coordinates.BlockPosArgument;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Display;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.level.entity.EntityTypeTest;
import org.jetbrains.annotations.Nullable;

import java.util.*;

import static de.z0rdak.yawp.api.commands.CommandConstants.*;
import static de.z0rdak.yawp.api.visualization.VisualizationManager.REGION_BLOCK_DISPLAY_TAG;
import static de.z0rdak.yawp.api.visualization.VisualizationManager.REGION_TEXT_DISPLAY_TAG;
import static de.z0rdak.yawp.commands.CommandUtil.*;
import static de.z0rdak.yawp.commands.arguments.ArgumentUtil.*;
import static de.z0rdak.yawp.api.MessageSender.sendCmdFeedback;
import static de.z0rdak.yawp.api.MessageSender.sendError;

class DimensionCommands {

    private DimensionCommands() {
    }

    private static List<String> getRegionNameSuggestions() {
        String examples = Component.translatableWithFallback("cli.region.name.examples", "newRegion,spawn,home,town,arena").getString();
        return Arrays.asList(examples.split(","));
    }

    static String getRandomExample() {
        List<String> regionNameSuggestions = getRegionNameSuggestions();
        return regionNameSuggestions.get(new Random().nextInt(regionNameSuggestions.size()));
    }

    static LiteralArgumentBuilder<CommandSourceStack> build() {
        return literal(DIM)
                /* /wp dimension <dim> list region */
                .then(Commands.argument(DIM.toString(), DimensionArgument.dimension())
                        /* /wp dimension <dim> [info] */
                        .executes(ctx -> CommandUtil.promptRegionInfo(ctx, getLevelDataArgument(ctx).getDim()))
                        .then(literal(INFO)
                                .executes(ctx -> CommandUtil.promptRegionInfo(ctx, getLevelDataArgument(ctx).getDim())))
                        .then(buildListSubCommand((ctx) -> getLevelDataArgument(ctx).getDim()))
                        .then(buildClearSubCommand((ctx) -> getLevelDataArgument(ctx).getDim()))
                        .then(buildAddSubCommand((ctx) -> getLevelDataArgument(ctx).getDim()))
                        .then(buildRemoveSubCommand((ctx) -> getLevelDataArgument(ctx).getDim()))
                        .then(buildCopySubCommand((ctx) -> getLevelDataArgument(ctx).getDim()))
                        .then(literal(NUKE_DISPLAY_ENTITIES)
                                .executes(ctx -> nukeDisplayEntities(ctx, ctx.getSource().getLevel()))
                                .then(Commands.argument(DIM.toString(), DimensionArgument.dimension())
                                        .executes(ctx -> nukeDisplayEntities(ctx, DimensionArgument.getDimension(ctx, DIM.toString())))))
                        .then(literal(LIST)
                                .then(literal(LOCAL)
                                        .executes(ctx -> promptDimensionRegionList(ctx, getLevelDataArgument(ctx), 0))
                                        .then(Commands.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                                .executes(ctx -> promptDimensionRegionList(ctx, getLevelDataArgument(ctx), getPageNoArgument(ctx)))))
                        )
                        .then(literal(DELETE)
                                .then(Commands.argument(CommandConstants.LOCAL.toString(), StringArgumentType.word())
                                        .suggests((ctx, builder) -> RegionArgumentType.region().listSuggestions(ctx, builder))
                                        .executes(ctx -> attemptDeleteRegion(ctx, getLevelDataArgument(ctx), getRegionArgument(ctx)))
                                        .then(literal(FOR_SURE)
                                                .executes(ctx -> deleteRegion(ctx, getLevelDataArgument(ctx), getRegionArgument(ctx))))))
                        .then(literal(DELETE_ALL)
                                .then(literal(REGIONS)
                                        .executes(ctx -> attemptDeleteRegions(ctx, getLevelDataArgument(ctx)))
                                        .then(literal(FOREVER)
                                                .then(literal(SERIOUSLY)
                                                        .executes(ctx -> deleteRegions(ctx, getLevelDataArgument(ctx)))))))
                        /* /wp dimension <dim> activate */
                        .then(literal(STATE)
                                .executes(ctx -> CommandUtil.promptRegionState(ctx, getLevelDataArgument(ctx).getDim()))
                                .then(literal(ALERT)
                                        .executes(ctx -> CommandUtil.setAlertState(ctx, getLevelDataArgument(ctx).getDim(), (getLevelDataArgument(ctx).getDim().isMuted())))
                                        .then(Commands.argument(ALERT.toString(), BoolArgumentType.bool())
                                                .executes(ctx -> CommandUtil.setAlertState(ctx, getLevelDataArgument(ctx).getDim(), getAlertArgument(ctx))))
                                )
                                .then(literal(ALERT_LOCAL)
                                        .then(Commands.argument(ALERT.toString(), BoolArgumentType.bool())
                                                .executes(ctx -> setAlertStateForAllLocal(ctx, getLevelDataArgument(ctx), getAlertArgument(ctx))))
                                )
                                .then(literal(ENABLE)
                                        .executes(ctx -> CommandUtil.setActiveState(ctx, getLevelDataArgument(ctx).getDim(), !getLevelDataArgument(ctx).getDim().isActive()))
                                        .then(Commands.argument(ENABLE.toString(), BoolArgumentType.bool())
                                                .executes(ctx -> CommandUtil.setActiveState(ctx, getLevelDataArgument(ctx).getDim(), getEnableArgument(ctx))))
                                )
                                .then(literal(ENABLE_LOCAL)
                                        .then(Commands.argument(ENABLE.toString(), BoolArgumentType.bool())
                                                .executes(ctx -> setActiveStateForAllLocal(ctx, getLevelDataArgument(ctx), getEnableArgument(ctx))))
                                )
                        )
                        .then(literal(CREATE)
                                .then(literal(CommandConstants.LOCAL)
                                        .then(Commands.argument(CommandConstants.NAME.toString(), StringArgumentType.word())
                                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(Collections.singletonList(getRandomExample()), builder))
                                                .then(Commands.literal(AreaType.CUBOID.areaType)
                                                        .then(Commands.argument(POS1.toString(), BlockPosArgument.blockPos())
                                                                .then(Commands.argument(POS2.toString(), BlockPosArgument.blockPos())
                                                                        .executes(ctx -> createCuboidRegion(ctx, getRegionNameArgument(ctx), getLevelDataArgument(ctx),
                                                                                BlockPosArgument.getSpawnablePos(ctx, POS1.toString()),
                                                                                BlockPosArgument.getSpawnablePos(ctx, POS2.toString()), null))
                                                                        .then(Commands.argument(CommandConstants.PARENT.toString(), StringArgumentType.word())
                                                                                .suggests((ctx, builder) -> ContainingOwnedRegionArgumentType.owningRegions().listSuggestions(ctx, builder))
                                                                                .executes(ctx -> createCuboidRegion(ctx, getRegionNameArgument(ctx), getLevelDataArgument(ctx),
                                                                                        BlockPosArgument.getSpawnablePos(ctx, POS1.toString()),
                                                                                        BlockPosArgument.getSpawnablePos(ctx, POS2.toString()), getContainingOwnedRegionArgument(ctx))))))
                                                )
                                                .then(Commands.literal(AreaType.SPHERE.areaType)
                                                        .then(Commands.argument(CENTER_POS.toString(), BlockPosArgument.blockPos())
                                                                .then(Commands.argument(RADIUS.toString(), IntegerArgumentType.integer(0))
                                                                        .executes(ctx -> createSphereRegion(ctx, getRegionNameArgument(ctx), getLevelDataArgument(ctx),
                                                                                BlockPosArgument.getSpawnablePos(ctx, CENTER_POS.toString()),
                                                                                IntegerArgumentType.getInteger(ctx, RADIUS.toString()), null))
                                                                        .then(Commands.argument(CommandConstants.PARENT.toString(), StringArgumentType.word())
                                                                                .suggests((ctx, builder) -> ContainingOwnedRegionArgumentType.owningRegions().listSuggestions(ctx, builder))
                                                                                .executes(ctx -> createSphereRegion(ctx, getRegionNameArgument(ctx), getLevelDataArgument(ctx),
                                                                                        BlockPosArgument.getSpawnablePos(ctx, CENTER_POS.toString()),
                                                                                        IntegerArgumentType.getInteger(ctx, RADIUS.toString()), getContainingOwnedRegionArgument(ctx))))))
                                                )
                                        )
                                )
                        )
                        .then(literal(RESET)
                                .then(literal(DIM)
                                        .executes(ctx -> resetDimRegion(ctx, getLevelDataArgument(ctx))))
                                .then(literal(REGIONS)
                                        .executes(ctx -> resetLocalRegions(ctx, getLevelDataArgument(ctx))))
                        )
                );
    }

    private static int nukeDisplayEntities(CommandContext<CommandSourceStack> ctx, ServerLevel level) {
        var entities = level.getEntities(EntityTypeTest.forClass(Display.class), (entity) -> {
            boolean containsTextTag = entity.entityTags().contains(REGION_TEXT_DISPLAY_TAG.toString());
            boolean containsBlockTag = entity.entityTags().contains(REGION_BLOCK_DISPLAY_TAG.toString());
            return containsTextTag || containsBlockTag;
        });
        var entityAmount = entities.size();
        entities.forEach(e -> e.remove(Entity.RemovalReason.DISCARDED));
        // TODO: I18n
        sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("Nuked all (%s) yawp entities in '%s'", "Nuked all (%s) yawp entities in '%s'", entityAmount, level.dimension().identifier().toString()));
        return 0;
    }


    private static int setActiveStateForAllLocal(CommandContext<CommandSourceStack> ctx, LevelRegionData dimCache, boolean enable) {
        if (dimCache != null) {
            dimCache.getLocalList().forEach(region -> region.setIsActive(enable));
            if (enable)
                sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.state.enable.all.set.on.value",
                        "Activates alert for all local regions of %s", ChatLinkBuilder.buildRegionInfoLink(dimCache.getDim())));
            else
                sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.state.enable.all.set.off.value",
                        "Deactivated all local regions of %s", ChatLinkBuilder.buildRegionInfoLink(dimCache.getDim())));
            RegionManager.get().save();
            return 0;
        } else {
            return 1;
        }
    }

    private static int setAlertStateForAllLocal(CommandContext<CommandSourceStack> ctx, LevelRegionData dimCache, boolean mute) {
        if (dimCache != null) {
            dimCache.getLocalList().forEach(region -> region.setIsMuted(mute));
            if (mute)
                sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.state.alert.all.set.on.value",
                        "Activated alert for all local regions of %s", ChatLinkBuilder.buildRegionInfoLink(dimCache.getDim())));
            else
                sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.state.alert.all.set.off.value",
                        "Deactivated alert for all local regions of %s", ChatLinkBuilder.buildRegionInfoLink(dimCache.getDim())));
            RegionManager.get().save();
            return 0;
        } else {
            return 1;
        }
    }

    /**
     * Reset groups (players and teams) and state for all local regions in the dimension.<br>
     * This keeps region hierarchy and flags intact. <br>
     * Scenario: You want to keep the local region layout and hierarchy but want to reset players and teams.<br>
     */
    private static int resetLocalRegions(CommandContext<CommandSourceStack> ctx, LevelRegionData dimCache) {
        dimCache.getLocalList().forEach(region -> {
            region.resetGroups();
            region.setIsActive(true);
            region.setIsMuted(false);
        });
        RegionManager.get().save();
        sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.dim.reset.all.confirm", "Successfully reset all local regions in %s", ChatLinkBuilder.buildRegionInfoLink(dimCache.getDim())));
        return 0;
    }

    /**
     * Reset groups (players and teams) and state for the dimensional region.<br>
     * This keeps region hierarchy and flags intact.<br>
     */
    private static int resetDimRegion(CommandContext<CommandSourceStack> ctx, LevelRegionData dimCache) {
        DimensionalRegion dimRegion = dimCache.getDim();
        dimRegion.resetGroups();
        dimRegion.setIsActive(true);
        dimRegion.setIsMuted(false);
        dimRegion.getFlags().clear();
        RegionManager.get().save();
        sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.dim.reset.confirm", "Successfully reset dimensional region %s", ChatLinkBuilder.buildRegionInfoLink(dimRegion)));
        return 0;
    }

    private static int createRegion(CommandContext<CommandSourceStack> ctx, String regionName, LevelRegionData levelData, IMarkableRegion region, IProtectedRegion parent) {
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
        Constants.LOGGER.info("1 before post created region {}", regionName);
        if (Services.REGION_EVENT_DISPATCHER.post(regionCreated)) {
            Constants.LOGGER.info("LAst? {}", regionName);
            return 1;
        }

        Services.REGION_CONFIG.getDefaultFlags().stream()
                .map(RegionFlag::fromId)
                .forEach(flag -> region.addFlag(new BooleanFlag(flag)));
        levelData.addLocal(parent, region);
        LocalRegions.ensureHigherRegionPriorityFor(region, Services.REGION_CONFIG.getDefaultPriority());
        RegionManager.get().save();
        sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.dim.info.region.create.success", "Successfully created region %s (parent: %s)", ChatLinkBuilder.buildRegionInfoLink(region), ChatLinkBuilder.buildRegionInfoLink(parent)));
        return 0;
    }

    public static int createCuboidRegion(CommandContext<CommandSourceStack> ctx, String regionName, BlockPos pos1, BlockPos pos2, @Nullable IProtectedRegion parentRegion) {
        var dimCache = RegionManager.get().getLevelRegionData(ctx.getSource().getLevel().dimension());
        if (dimCache.isPresent()) {
            return createCuboidRegion(ctx, regionName, dimCache.get(), pos1, pos2, parentRegion);
        }
        Constants.LOGGER.error("Error getting dimension cache for {}", ctx.getSource().getLevel().dimension().identifier().toString());
        return -1;
    }

    public static int createSphereRegion(CommandContext<CommandSourceStack> ctx, String regionName, BlockPos centerPos, int radius, @Nullable IProtectedRegion parentRegion) {
        var dimCache = RegionManager.get().getLevelRegionData(ctx.getSource().getLevel().dimension());
        if (dimCache.isPresent()) {
            return createSphereRegion(ctx, regionName, dimCache.get(), centerPos, radius, parentRegion);
        }
        Constants.LOGGER.error("Error getting dimension cache for {}", ctx.getSource().getLevel().dimension().identifier().toString());
        return -1;
    }

    public static int createCuboidRegion(CommandContext<CommandSourceStack> ctx, String regionName, LevelRegionData levelData, BlockPos pos1, BlockPos pos2, @Nullable IProtectedRegion parentRegion) {
        CuboidRegion region = new CuboidRegion(regionName, new CuboidArea(pos1, pos2), null, levelData.getDimKey());
        IProtectedRegion parent = parentRegion == null ? levelData.getDim() : parentRegion;
        return createRegion(ctx, regionName, levelData, region, parent);
    }

    private static int createSphereRegion(CommandContext<CommandSourceStack> ctx, String regionName, LevelRegionData levelData, BlockPos centerPos, BlockPos radiusPos, @Nullable IProtectedRegion parentRegion) {
        SphereRegion region = new SphereRegion(regionName, new SphereArea(centerPos, radiusPos), null, levelData.getDimKey());
        IProtectedRegion parent = parentRegion == null ? levelData.getDim() : parentRegion;
        return createRegion(ctx, regionName, levelData, region, parent);
    }

    public static int createSphereRegion(CommandContext<CommandSourceStack> ctx, String regionName, LevelRegionData levelData, BlockPos centerPos, int radius, @Nullable IProtectedRegion parentRegion) {
        SphereRegion region = new SphereRegion(regionName, new SphereArea(centerPos, radius), null, levelData.getDimKey());
        IProtectedRegion parent = parentRegion == null ? levelData.getDim() : parentRegion;
        return createRegion(ctx, regionName, levelData, region, parent);
    }

    public static int attemptDeleteRegion(CommandContext<CommandSourceStack> ctx, LevelRegionData levelData, IMarkableRegion region) {
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

    public static int deleteRegion(CommandContext<CommandSourceStack> ctx, LevelRegionData levelData, IMarkableRegion region) {
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
                RegionManager.get().save();
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

    private static int attemptDeleteRegions(CommandContext<CommandSourceStack> ctx, LevelRegionData dimCache) {
        int amount = dimCache.getLocalNames().size();
        MutableComponent removeAllRegionsLink = ChatLinkBuilder.buildRemoveAllRegionsLink(dimCache);
        sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.dim.region.remove.all.attempt", "Attempt to remove all (%s) regions from dimension %s. Confirm removal by clicking here %s",
                amount, ChatLinkBuilder.buildRegionInfoLink(dimCache.getDim()), removeAllRegionsLink));
        return 0;
    }

    private static int deleteRegions(CommandContext<CommandSourceStack> ctx, LevelRegionData levelData) {
        int amount = levelData.regionCount();
        levelData.clearLocals();
        RegionManager.get().save();
        sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.dim.region.remove.all.confirm", "Removed %s regions from dimension %s", amount, ChatLinkBuilder.buildRegionInfoLink(levelData.getDim())));
        return 0;
    }

    /**
     * Note: Could, together with promptChildRegionList, be refactored to a single method
     */
    private static int promptDimensionRegionList(CommandContext<CommandSourceStack> ctx, LevelRegionData levelData, int pageNo) {
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
}