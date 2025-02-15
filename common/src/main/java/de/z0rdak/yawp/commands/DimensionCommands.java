package de.z0rdak.yawp.commands;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.api.commands.CommandConstants;
import de.z0rdak.yawp.api.events.region.RegionEvent;
import de.z0rdak.yawp.commands.arguments.region.ContainingOwnedRegionArgumentType;
import de.z0rdak.yawp.commands.arguments.region.RegionArgumentType;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.area.SphereArea;
import de.z0rdak.yawp.core.region.*;
import de.z0rdak.yawp.data.region.DimensionRegionCache;
import de.z0rdak.yawp.data.region.RegionDataManager;
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
import net.minecraft.server.level.ServerPlayer;
import org.jetbrains.annotations.Nullable;

import java.util.*;

import static de.z0rdak.yawp.api.commands.CommandConstants.*;
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
                        .executes(ctx -> CommandUtil.promptRegionInfo(ctx, getDimCacheArgument(ctx).getDimensionalRegion()))
                        .then(literal(INFO)
                                .executes(ctx -> CommandUtil.promptRegionInfo(ctx, getDimCacheArgument(ctx).getDimensionalRegion())))
                        .then(buildListSubCommand((ctx) -> getDimCacheArgument(ctx).getDimensionalRegion()))
                        .then(buildClearSubCommand((ctx) -> getDimCacheArgument(ctx).getDimensionalRegion()))
                        .then(buildAddSubCommand((ctx) -> getDimCacheArgument(ctx).getDimensionalRegion()))
                        .then(buildRemoveSubCommand((ctx) -> getDimCacheArgument(ctx).getDimensionalRegion()))
                        .then(buildCopySubCommand((ctx) -> getDimCacheArgument(ctx).getDimensionalRegion()))
                        .then(literal(LIST)
                                .then(literal(LOCAL)
                                        .executes(ctx -> promptDimensionRegionList(ctx, getDimCacheArgument(ctx), 0))
                                        .then(Commands.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                                .executes(ctx -> promptDimensionRegionList(ctx, getDimCacheArgument(ctx), getPageNoArgument(ctx)))))
                        )
                        .then(literal(DELETE)
                                .then(Commands.argument(CommandConstants.LOCAL.toString(), StringArgumentType.word())
                                        .suggests((ctx, builder) -> RegionArgumentType.region().listSuggestions(ctx, builder))
                                        .executes(ctx -> attemptDeleteRegion(ctx, getDimCacheArgument(ctx), getRegionArgument(ctx)))
                                        .then(literal(FOR_SURE)
                                                .executes(ctx -> deleteRegion(ctx, getDimCacheArgument(ctx), getRegionArgument(ctx))))))
                        .then(literal(DELETE_ALL)
                                .then(literal(REGIONS)
                                        .executes(ctx -> attemptDeleteRegions(ctx, getDimCacheArgument(ctx)))
                                        .then(literal(FOREVER)
                                                .then(literal(SERIOUSLY)
                                                        .executes(ctx -> deleteRegions(ctx, getDimCacheArgument(ctx)))))))
                        /* /wp dimension <dim> activate */
                        .then(literal(STATE)
                                .executes(ctx -> CommandUtil.promptRegionState(ctx, getDimCacheArgument(ctx).getDimensionalRegion()))
                                .then(literal(ALERT)
                                        .executes(ctx -> CommandUtil.setAlertState(ctx, getDimCacheArgument(ctx).getDimensionalRegion(), (getDimCacheArgument(ctx).getDimensionalRegion().isMuted())))
                                        .then(Commands.argument(ALERT.toString(), BoolArgumentType.bool())
                                                .executes(ctx -> CommandUtil.setAlertState(ctx, getDimCacheArgument(ctx).getDimensionalRegion(), getAlertArgument(ctx))))
                                )
                                .then(literal(ALERT_LOCAL)
                                        .then(Commands.argument(ALERT.toString(), BoolArgumentType.bool())
                                                .executes(ctx -> setAlertStateForAllLocal(ctx, getDimCacheArgument(ctx), getAlertArgument(ctx))))
                                )
                                .then(literal(ENABLE)
                                        .executes(ctx -> CommandUtil.setActiveState(ctx, getDimCacheArgument(ctx).getDimensionalRegion(), !getDimCacheArgument(ctx).getDimensionalRegion().isActive()))
                                        .then(Commands.argument(ENABLE.toString(), BoolArgumentType.bool())
                                                .executes(ctx -> CommandUtil.setActiveState(ctx, getDimCacheArgument(ctx).getDimensionalRegion(), getEnableArgument(ctx))))
                                )
                                .then(literal(ENABLE_LOCAL)
                                        .then(Commands.argument(ENABLE.toString(), BoolArgumentType.bool())
                                                .executes(ctx -> setActiveStateForAllLocal(ctx, getDimCacheArgument(ctx), getEnableArgument(ctx))))
                                )
                        )
                        .then(literal(CREATE)
                                .then(literal(CommandConstants.LOCAL)
                                        .then(Commands.argument(CommandConstants.NAME.toString(), StringArgumentType.word())
                                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(Collections.singletonList(getRandomExample()), builder))
                                                //.then(Commands.argument(AREA.toString(), StringArgumentType.word())
                                                //        .suggests((ctx, builder) -> AreaArgumentType.areaType().listSuggestions(ctx, builder))
                                                //        .executes(ctx -> createRegion(ctx.getSource(), getRegionNameArgument(ctx), getDimCacheArgument(ctx), getAreaTypeArgument(ctx))))
                                                .then(Commands.literal(AreaType.CUBOID.areaType)
                                                        .then(Commands.argument(POS1.toString(), BlockPosArgument.blockPos())
                                                                .then(Commands.argument(POS2.toString(), BlockPosArgument.blockPos())
                                                                        .executes(ctx -> createCuboidRegion(ctx, getRegionNameArgument(ctx), getDimCacheArgument(ctx),
                                                                                BlockPosArgument.getSpawnablePos(ctx, POS1.toString()),
                                                                                BlockPosArgument.getSpawnablePos(ctx, POS2.toString()), null))
                                                                        .then(Commands.argument(CommandConstants.PARENT.toString(), StringArgumentType.word())
                                                                                .suggests((ctx, builder) -> ContainingOwnedRegionArgumentType.owningRegions().listSuggestions(ctx, builder))
                                                                                .executes(ctx -> createCuboidRegion(ctx, getRegionNameArgument(ctx), getDimCacheArgument(ctx),
                                                                                        BlockPosArgument.getSpawnablePos(ctx, POS1.toString()),
                                                                                        BlockPosArgument.getSpawnablePos(ctx, POS2.toString()), getContainingOwnedRegionArgument(ctx))))))
                                                )
                                                /* Note: Disabled for now because in conflicts with the 2nd option where only the radius is provided.
                                                .then(Commands.literal(AreaType.SPHERE.areaType)
                                                        .then(Commands.argument(CENTER_POS.toString(), BlockPosArgument.blockPos())
                                                                .then(Commands.argument(RADIUS_POS.toString(), BlockPosArgument.blockPos())
                                                                        .executes(ctx -> createSphereRegion(ctx, getRegionNameArgument(ctx), getDimCacheArgument(ctx),
                                                                                BlockPosArgument.getSpawnablePos(ctx, CENTER_POS.toString()),
                                                                                BlockPosArgument.getSpawnablePos(ctx, RADIUS_POS.toString()), null))
                                                                        .then(Commands.argument(CommandConstants.PARENT.toString(), StringArgumentType.word())
                                                                                .suggests((ctx, builder) -> ContainingOwnedRegionArgumentType.owningRegions().listSuggestions(ctx, builder))
                                                                                .executes(ctx -> createSphereRegion(ctx, getRegionNameArgument(ctx), getDimCacheArgument(ctx),
                                                                                        BlockPosArgument.getSpawnablePos(ctx, CENTER_POS.toString()),
                                                                                        BlockPosArgument.getSpawnablePos(ctx, RADIUS_POS.toString()), getContainingOwnedRegionArgument(ctx))))))
                                                )
                                                */
                                                .then(Commands.literal(AreaType.SPHERE.areaType)
                                                        .then(Commands.argument(CENTER_POS.toString(), BlockPosArgument.blockPos())
                                                                .then(Commands.argument(RADIUS.toString(), IntegerArgumentType.integer(0))
                                                                        .executes(ctx -> createSphereRegion(ctx, getRegionNameArgument(ctx), getDimCacheArgument(ctx),
                                                                                BlockPosArgument.getSpawnablePos(ctx, CENTER_POS.toString()),
                                                                                IntegerArgumentType.getInteger(ctx, RADIUS.toString()), null))
                                                                        .then(Commands.argument(CommandConstants.PARENT.toString(), StringArgumentType.word())
                                                                                .suggests((ctx, builder) -> ContainingOwnedRegionArgumentType.owningRegions().listSuggestions(ctx, builder))
                                                                                .executes(ctx -> createSphereRegion(ctx, getRegionNameArgument(ctx), getDimCacheArgument(ctx),
                                                                                        BlockPosArgument.getSpawnablePos(ctx, CENTER_POS.toString()),
                                                                                        IntegerArgumentType.getInteger(ctx, RADIUS.toString()), getContainingOwnedRegionArgument(ctx))))))
                                                )
                                        )
                                )
                        )
                        .then(literal(RESET)
                                .then(literal(DIM)
                                        .executes(ctx -> resetDimRegion(ctx, getDimCacheArgument(ctx))))
                                .then(literal(REGIONS)
                                        .executes(ctx -> resetLocalRegions(ctx, getDimCacheArgument(ctx))))
                        )
                );
    }


    private static int setActiveStateForAllLocal(CommandContext<CommandSourceStack> ctx, DimensionRegionCache dimCache, boolean enable) {
        if (dimCache != null) {
            dimCache.getRegionsInDimension().values().forEach(region -> region.setIsActive(enable));
            if (enable)
                sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.state.enable.all.set.on.value",
                        "Activates alert for all local regions of %s", ChatLinkBuilder.buildRegionInfoLink(dimCache.getDimensionalRegion())));
            else
                sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.state.enable.all.set.off.value",
                        "Deactivated all local regions of %s", ChatLinkBuilder.buildRegionInfoLink(dimCache.getDimensionalRegion())));
            RegionDataManager.save();
            return 0;
        } else {
            return 1;
        }
    }

    private static int setAlertStateForAllLocal(CommandContext<CommandSourceStack> ctx, DimensionRegionCache dimCache, boolean mute) {
        if (dimCache != null) {
            dimCache.getRegionsInDimension().values().forEach(region -> region.setIsMuted(mute));
            if (mute)
                sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.state.alert.all.set.on.value",
                        "Activated alert for all local regions of %s", ChatLinkBuilder.buildRegionInfoLink(dimCache.getDimensionalRegion())));
            else
                sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.state.alert.all.set.off.value",
                        "Deactivated alert for all local regions of %s", ChatLinkBuilder.buildRegionInfoLink(dimCache.getDimensionalRegion())));
            RegionDataManager.save();
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
    private static int resetLocalRegions(CommandContext<CommandSourceStack> ctx, DimensionRegionCache dimCache) {
        dimCache.getRegionsInDimension().values().forEach(region -> {
            region.resetGroups();
            region.setIsActive(true);
            region.setIsMuted(false);
        });
        RegionDataManager.save();
        sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.dim.reset.all.confirm", "Successfully reset all local regions in %s", ChatLinkBuilder.buildRegionInfoLink(dimCache.getDimensionalRegion())));
        return 0;
    }

    /**
     * Reset groups (players and teams) and state for the dimensional region.<br>
     * This keeps region hierarchy and flags intact.<br>
     */
    private static int resetDimRegion(CommandContext<CommandSourceStack> ctx, DimensionRegionCache dimCache) {
        DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
        dimRegion.resetGroups();
        dimRegion.setIsActive(true);
        dimRegion.setIsMuted(false);
        dimRegion.getFlags().clear();
        RegionDataManager.save();
        sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.dim.reset.confirm", "Successfully reset dimensional region %s", ChatLinkBuilder.buildRegionInfoLink(dimRegion)));
        return 0;
    }

    private static int createRegion(CommandContext<CommandSourceStack> ctx, String regionName, DimensionRegionCache dimCache, IMarkableRegion region, IProtectedRegion parent) {
        int res = RegionDataManager.get().isValidRegionName(dimCache.getDimensionalRegion().getDim(), regionName);
        if (res == -1) {
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.dim.info.region.create.name.invalid", "Invalid region name supplied: '%s'", regionName));
            return res;
        }
        if (res == 1) {
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.dim.info.region.create.name.exists", "Dimension %s already contains region with name %s", dimCache.getDimensionalRegion().getName(), ChatLinkBuilder.buildRegionInfoLink(dimCache.getRegion(regionName))));
            return res;
        }
        ServerPlayer player;
        try {
            player = ctx.getSource().getPlayerOrException();
        } catch (CommandSyntaxException e) {
            player = null;
        }
        if (Services.EVENT.post(new RegionEvent.Create(region, player))) {
            return 1;
        }

        RegionDataManager.addFlags(Services.REGION_CONFIG.getDefaultFlags(), region);
        dimCache.addRegion(parent, region);
        LocalRegions.ensureHigherRegionPriorityFor(region, Services.REGION_CONFIG.getDefaultPriority());
        RegionDataManager.save();
        sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.dim.info.region.create.success", "Successfully created region %s (with parent %s)", ChatLinkBuilder.buildRegionInfoLink(region), ChatLinkBuilder.buildRegionInfoLink(parent)));
        return 0;
    }

    private static int createCuboidRegion(CommandContext<CommandSourceStack> ctx, String regionName, DimensionRegionCache dimCache, BlockPos pos1, BlockPos pos2, @Nullable IProtectedRegion parentRegion) {
        CuboidRegion region = new CuboidRegion(regionName, new CuboidArea(pos1, pos2), null, dimCache.dimensionKey());
        IProtectedRegion parent = parentRegion == null ? dimCache.getDimensionalRegion() : parentRegion;
        return createRegion(ctx, regionName, dimCache, region, parent);
    }

    private static int createSphereRegion(CommandContext<CommandSourceStack> ctx, String regionName, DimensionRegionCache dimCache, BlockPos centerPos, BlockPos radiusPos, @Nullable IProtectedRegion parentRegion) {
        SphereRegion region = new SphereRegion(regionName, new SphereArea(centerPos, radiusPos), null, dimCache.dimensionKey());
        IProtectedRegion parent = parentRegion == null ? dimCache.getDimensionalRegion() : parentRegion;
        return createRegion(ctx, regionName, dimCache, region, parent);
    }

    private static int createSphereRegion(CommandContext<CommandSourceStack> ctx, String regionName, DimensionRegionCache dimCache, BlockPos centerPos, int radius, @Nullable IProtectedRegion parentRegion) {
        SphereRegion region = new SphereRegion(regionName, new SphereArea(centerPos, radius), null, dimCache.dimensionKey());
        IProtectedRegion parent = parentRegion == null ? dimCache.getDimensionalRegion() : parentRegion;
        return createRegion(ctx, regionName, dimCache, region, parent);
    }

    public static int attemptDeleteRegion(CommandContext<CommandSourceStack> ctx, DimensionRegionCache dim, IMarkableRegion region) {
        if (dim.contains(region.getName())) {
            MutableComponent removeRegionLink = ChatLinkBuilder.buildRemoveRegionLink(region);
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.dim.region.remove.attempt", "Attempt to remove region %s from %s. Confirm by clicking here %s",
                    ChatLinkBuilder.buildRegionInfoLink(region), ChatLinkBuilder.buildRegionInfoLink(dim.getDimensionalRegion()), removeRegionLink));
            return 0;
        }
        return 1;
    }

    public static int deleteRegion(CommandContext<CommandSourceStack> ctx, DimensionRegionCache dim, IMarkableRegion region) {
        ServerPlayer player;
        try {
            player = ctx.getSource().getPlayerOrException();
        } catch (CommandSyntaxException e) {
            player = null;
        }
        if (Services.EVENT.post(new RegionEvent.Remove(region, player))) {
            return 1;
        }
        if (dim.contains(region.getName())) {
            if (!region.getChildren().isEmpty()) {
                sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.dim.region.remove.fail.hasChildren", "Region %s can't be deleted because it has child regions.", ChatLinkBuilder.buildRegionInfoLink(region)));
                return -1;
            }
            RegionType parentType = region.getParent().getRegionType();
            if (parentType == RegionType.DIMENSION) {
                dim.removeRegion(region);
                RegionDataManager.save();
                sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.dim.region.remove.confirm", "Removed region '%s' from %s", region.getName(), ChatLinkBuilder.buildRegionInfoLink(dim.getDimensionalRegion())));
                return 0;
            }
            if (parentType == RegionType.LOCAL) {
                sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.dim.region.remove.fail.hasParent", "Region %s can't be deleted because it has a Local Regions as parent.", ChatLinkBuilder.buildRegionInfoLink(region)));
                return 1;
            }
        }
        return 1;
    }

    private static int attemptDeleteRegions(CommandContext<CommandSourceStack> ctx, DimensionRegionCache dimCache) {
        int amount = dimCache.getRegionsInDimension().size();
        MutableComponent removeAllRegionsLink = ChatLinkBuilder.buildRemoveAllRegionsLink(dimCache);
        sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.dim.region.remove.all.attempt", "Attempt to remove all (%s) regions from dimension %s. Confirm removal by clicking here %s",
                amount, ChatLinkBuilder.buildRegionInfoLink(dimCache.getDimensionalRegion()), removeAllRegionsLink));
        return 0;
    }

    private static int deleteRegions(CommandContext<CommandSourceStack> ctx, DimensionRegionCache dimCache) {
        int amount = dimCache.getRegionsInDimension().size();
        dimCache.clearRegions();
        RegionDataManager.save();
        sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.dim.region.remove.all.confirm", "Removed %s regions from dimension %s", amount, ChatLinkBuilder.buildRegionInfoLink(dimCache.getDimensionalRegion())));
        return 0;
    }

    /**
     * Note: Could, together with promptChildRegionList, be refactored to a single method
     */
    private static int promptDimensionRegionList(CommandContext<CommandSourceStack> ctx, DimensionRegionCache dimCache, int pageNo) {
        List<IProtectedRegion> regionsForDim = dimCache.getAllLocal().stream()
                .map(region -> (IProtectedRegion) region)
                .sorted(Comparator.comparing(IProtectedRegion::getName))
                .toList();
        try {
            int paginationSize = Services.REGION_CONFIG.getPaginationSize();
            RegionsInDimensionPagination childRegionPagination = new RegionsInDimensionPagination(dimCache, regionsForDim, pageNo, paginationSize);
            MultiLineMessage.send(ctx.getSource(), childRegionPagination);
        } catch (InvalidPageNumberException e) {
            sendError(ctx.getSource(), e.getError());
            return -1;
        }
        return 0;
    }
}