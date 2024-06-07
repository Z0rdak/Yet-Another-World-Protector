package de.z0rdak.yawp.commands;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.api.events.region.RegionEvent;
import de.z0rdak.yawp.api.events.region.RegionEvents;
import de.z0rdak.yawp.commands.arguments.region.ContainingOwnedRegionArgumentType;
import de.z0rdak.yawp.commands.arguments.region.RegionArgumentType;
import de.z0rdak.yawp.config.server.RegionConfig;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.area.SphereArea;
import de.z0rdak.yawp.core.region.*;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.LocalRegions;
import net.minecraft.command.CommandSource;
import net.minecraft.command.argument.BlockPosArgumentType;
import net.minecraft.command.argument.DimensionArgumentType;
import net.minecraft.server.command.CommandManager;
import net.minecraft.server.command.ServerCommandSource;
import net.minecraft.server.network.ServerPlayerEntity;
import net.minecraft.text.MutableText;
import net.minecraft.text.Text;
import net.minecraft.util.math.BlockPos;
import org.jetbrains.annotations.Nullable;

import java.util.*;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.commands.CommandUtil.*;
import static de.z0rdak.yawp.commands.arguments.ArgumentUtil.*;
import static de.z0rdak.yawp.util.ChatComponentBuilder.*;
import static de.z0rdak.yawp.util.MessageSender.sendCmdFeedback;

public class DimensionCommands {

    private DimensionCommands() {
    }

    private static List<String> getRegionNameSuggestions() {
        String examples = Text.translatableWithFallback("cli.region.name.examples", "newRegion,spawn,home,town,arena").getString();
        return Arrays.asList(examples.split(","));
    }

    public static String getRandomExample() {
        List<String> regionNameSuggestions = getRegionNameSuggestions();
        return regionNameSuggestions.get(new Random().nextInt(regionNameSuggestions.size()));
    }

    public static LiteralArgumentBuilder<ServerCommandSource> build() {
        return literal(DIM)
                /* /wp dimension <dim> list region */
                .then(CommandManager.argument(DIM.toString(), DimensionArgumentType.dimension())
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
                                        .then(CommandManager.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                                .executes(ctx -> promptDimensionRegionList(ctx, getDimCacheArgument(ctx), getPageNoArgument(ctx)))))
                        )
                        .then(literal(DELETE)
                                .then(CommandManager.argument(CommandConstants.LOCAL.toString(), StringArgumentType.word())
                                        .suggests((ctx, builder) -> RegionArgumentType.region().listSuggestions(ctx, builder))
                                        .executes(ctx -> attemptDeleteRegion(ctx, getDimCacheArgument(ctx), getRegionArgument(ctx)))
                                        .then(CommandManager.literal("-y")
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
                                        .then(CommandManager.argument(ALERT.toString(), BoolArgumentType.bool())
                                                .executes(ctx -> CommandUtil.setAlertState(ctx, getDimCacheArgument(ctx).getDimensionalRegion(), getAlertArgument(ctx))))
                                )
                                .then(literal(ALERT_LOCAL)
                                        .then(CommandManager.argument(ALERT.toString(), BoolArgumentType.bool())
                                                .executes(ctx -> setAlertStateForAllLocal(ctx, getDimCacheArgument(ctx), getAlertArgument(ctx))))
                                )
                                .then(literal(ENABLE)
                                        .executes(ctx -> CommandUtil.setActiveState(ctx, getDimCacheArgument(ctx).getDimensionalRegion(), !getDimCacheArgument(ctx).getDimensionalRegion().isActive()))
                                        .then(CommandManager.argument(ENABLE.toString(), BoolArgumentType.bool())
                                                .executes(ctx -> CommandUtil.setActiveState(ctx, getDimCacheArgument(ctx).getDimensionalRegion(), getEnableArgument(ctx))))
                                )
                                .then(literal(ENABLE_LOCAL)
                                        .then(CommandManager.argument(ENABLE.toString(), BoolArgumentType.bool())
                                                .executes(ctx -> setActiveStateForAllLocal(ctx, getDimCacheArgument(ctx), getEnableArgument(ctx))))
                                )
                        )
                        .then(literal(CREATE)
                                .then(literal(CommandConstants.LOCAL)
                                        .then(CommandManager.argument(CommandConstants.NAME.toString(), StringArgumentType.word())
                                                .suggests((ctx, builder) -> CommandSource.suggestMatching(Collections.singletonList(getRandomExample()), builder))
                                                //.then(CommandManager.argument(AREA.toString(), StringArgumentType.word())
                                                //        .suggests((ctx, builder) -> AreaArgumentType.areaType().listSuggestions(ctx, builder))
                                                //        .executes(ctx -> createRegion(ctx.getSource(), getRegionNameArgument(ctx), getDimCacheArgument(ctx), getAreaTypeArgument(ctx))))
                                                .then(CommandManager.literal(AreaType.CUBOID.areaType)
                                                        .then(CommandManager.argument(POS1.toString(), BlockPosArgumentType.blockPos())
                                                                .then(CommandManager.argument(POS2.toString(), BlockPosArgumentType.blockPos())
                                                                        .executes(ctx -> createCuboidRegion(ctx, getRegionNameArgument(ctx), getDimCacheArgument(ctx),
                                                                                BlockPosArgumentType.getLoadedBlockPos(ctx, POS1.toString()),
                                                                                BlockPosArgumentType.getLoadedBlockPos(ctx, POS2.toString()), null))
                                                                        .then(CommandManager.argument(CommandConstants.PARENT.toString(), StringArgumentType.word())
                                                                                .suggests((ctx, builder) -> ContainingOwnedRegionArgumentType.owningRegions().listSuggestions(ctx, builder))
                                                                                .executes(ctx -> createCuboidRegion(ctx, getRegionNameArgument(ctx), getDimCacheArgument(ctx),
                                                                                        BlockPosArgumentType.getLoadedBlockPos(ctx, POS1.toString()),
                                                                                        BlockPosArgumentType.getLoadedBlockPos(ctx, POS2.toString()), getContainingOwnedRegionArgument(ctx))))))
                                                )
                                                .then(CommandManager.literal(AreaType.SPHERE.areaType)
                                                        .then(CommandManager.argument(CENTER_POS.toString(), BlockPosArgumentType.blockPos())
                                                                .then(CommandManager.argument(RADIUS_POS.toString(), BlockPosArgumentType.blockPos())
                                                                        .executes(ctx -> createSphereRegion(ctx, getRegionNameArgument(ctx), getDimCacheArgument(ctx),
                                                                                BlockPosArgumentType.getLoadedBlockPos(ctx, CENTER_POS.toString()),
                                                                                BlockPosArgumentType.getLoadedBlockPos(ctx, RADIUS_POS.toString()), null))
                                                                        .then(CommandManager.argument(CommandConstants.PARENT.toString(), StringArgumentType.word())
                                                                                .suggests((ctx, builder) -> ContainingOwnedRegionArgumentType.owningRegions().listSuggestions(ctx, builder))
                                                                                .executes(ctx -> createSphereRegion(ctx, getRegionNameArgument(ctx), getDimCacheArgument(ctx),
                                                                                        BlockPosArgumentType.getLoadedBlockPos(ctx, CENTER_POS.toString()),
                                                                                        BlockPosArgumentType.getLoadedBlockPos(ctx, RADIUS_POS.toString()), getContainingOwnedRegionArgument(ctx))))))
                                                )
                                                .then(CommandManager.literal(AreaType.SPHERE.areaType)
                                                        .then(CommandManager.argument(CENTER_POS.toString(), BlockPosArgumentType.blockPos())
                                                                .then(CommandManager.argument(RADIUS.toString(), IntegerArgumentType.integer(0))
                                                                        .executes(ctx -> createSphereRegion(ctx, getRegionNameArgument(ctx), getDimCacheArgument(ctx),
                                                                                BlockPosArgumentType.getLoadedBlockPos(ctx, CENTER_POS.toString()),
                                                                                IntegerArgumentType.getInteger(ctx, RADIUS.toString()), null))
                                                                        .then(CommandManager.argument(CommandConstants.PARENT.toString(), StringArgumentType.word())
                                                                                .suggests((ctx, builder) -> ContainingOwnedRegionArgumentType.owningRegions().listSuggestions(ctx, builder))
                                                                                .executes(ctx -> createSphereRegion(ctx, getRegionNameArgument(ctx), getDimCacheArgument(ctx),
                                                                                        BlockPosArgumentType.getLoadedBlockPos(ctx, CENTER_POS.toString()),
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


    private static int setActiveStateForAllLocal(CommandContext<ServerCommandSource> ctx, DimensionRegionCache dimCache, boolean enable) {
        if (dimCache != null) {
            dimCache.getRegionsInDimension().values().forEach(region -> region.setIsActive(enable));
            if (enable)
                sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.info.region.state.enable.all.set.on.value",
                        "Activates alert for all local regions of %s", buildRegionInfoLink(dimCache.getDimensionalRegion())));
            else
                sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.info.region.state.enable.all.set.off.value",
                        "Deactivated all local regions of %s", buildRegionInfoLink(dimCache.getDimensionalRegion())));
            RegionDataManager.save();
            return 0;
        } else {
            return 1;
        }
    }

    private static int setAlertStateForAllLocal(CommandContext<ServerCommandSource> ctx, DimensionRegionCache dimCache, boolean mute) {
        if (dimCache != null) {
            dimCache.getRegionsInDimension().values().forEach(region -> region.setIsMuted(mute));
            if (mute)
                sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.info.region.state.alert.all.set.on.value",
                        "Activated alert for all local regions of %s", buildRegionInfoLink(dimCache.getDimensionalRegion())));
            else
                sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.info.region.state.alert.all.set.off.value",
                        "Deactivated alert for all local regions of %s", buildRegionInfoLink(dimCache.getDimensionalRegion())));
            RegionDataManager.save();
            return 0;
        } else {
            return 1;
        }
    }

    /**
     * Reset groups (players and teams) and state for all local regions in the dimension.<br></br>
     * This keeps region hierarchy and flags intact. <br></br>
     * Scenario: You want to keep the local region layout and hierarchy but want to reset players and teams.<br></br>
     */
    private static int resetLocalRegions(CommandContext<ServerCommandSource> ctx, DimensionRegionCache dimCache) {
        dimCache.getRegionsInDimension().values().forEach(region -> {
            region.resetGroups();
            region.setIsActive(true);
            region.setIsMuted(false);
        });
        RegionDataManager.save();
        sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.info.dim.reset.all.confirm", "Successfully reset all local regions in %s", buildRegionInfoLink(dimCache.getDimensionalRegion())));
        return 0;
    }

    /**
     * Reset groups (players and teams) and state for the dimensional region.<br></br>
     * This keeps region hierarchy and flags intact.<br></br>
     */
    private static int resetDimRegion(CommandContext<ServerCommandSource> ctx, DimensionRegionCache dimCache) {
        dimCache.getDimensionalRegion().resetGroups();
        dimCache.getDimensionalRegion().setIsActive(true);
        dimCache.getDimensionalRegion().setIsMuted(false);
        dimCache.getDimensionalRegion().getFlagContainer().clear();
        RegionDataManager.save();
        sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.info.dim.reset.confirm", "Successfully reset dimensional region %s", buildRegionInfoLink(dimCache.getDimensionalRegion())));
        return 0;
    }

    private static int createRegion(CommandContext<ServerCommandSource> ctx, String regionName, DimensionRegionCache dimCache, IMarkableRegion region, IProtectedRegion parent) {
        int res = RegionDataManager.get().isValidRegionName(dimCache.getDimensionalRegion().getDim(), regionName);
        if (res == -1) {
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.dim.info.region.create.name.invalid", "Invalid region name supplied: '%s'", regionName));
            return res;
        }
        if (res == 1) {
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.dim.info.region.create.name.exists", "Dimension %s already contains region with name %s", dimCache.getDimensionalRegion().getName(), buildRegionInfoLink(dimCache.getRegion(regionName))));
            return res;
        }
        ServerPlayerEntity player;
        try {
            player = ctx.getSource().getPlayerOrThrow();
        } catch (CommandSyntaxException e) {
            player = null;
        }
        if (RegionEvents.CREATE_REGION.invoker().createRegion(new RegionEvent.CreateRegionEvent(region, player))) {
            return 0;
        }
        if (parent.getRegionType() != RegionType.DIMENSION && parent.getRegionType() != RegionType.LOCAL) {
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.dim.info.region.create.error", "Error creating region %s", buildRegionInfoLink(parent)));
            return -1;
        }
        RegionDataManager.addFlags(RegionConfig.getDefaultFlags(), region);
        dimCache.addRegion(parent, region);
        LocalRegions.ensureHigherRegionPriorityFor(region, RegionConfig.getDefaultPriority());
        RegionDataManager.save();
        sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.dim.info.region.create.success", "Successfully created region %s (with parent %s)", buildRegionInfoLink(region), buildRegionInfoLink(parent)));
        return 0;
    }

    private static int createCuboidRegion(CommandContext<ServerCommandSource> ctx, String regionName, DimensionRegionCache dimCache, BlockPos pos1, BlockPos pos2, @Nullable IProtectedRegion parent) {
        CuboidRegion region = new CuboidRegion(regionName, new CuboidArea(pos1, pos2), null, dimCache.dimensionKey());
        if (parent == null) {
            return createRegion(ctx, regionName, dimCache, region, dimCache.getDimensionalRegion());
        }
        return createRegion(ctx, regionName, dimCache, region, parent);
    }

    private static int createSphereRegion(CommandContext<ServerCommandSource> ctx, String regionName, DimensionRegionCache dimCache, BlockPos centerPos, BlockPos radiusPos, @Nullable IProtectedRegion parent) {
        SphereRegion region = new SphereRegion(regionName, new SphereArea(centerPos, radiusPos), null, dimCache.dimensionKey());
        if (parent == null) {
            return createRegion(ctx, regionName, dimCache, region, dimCache.getDimensionalRegion());
        }
        return createRegion(ctx, regionName, dimCache, region, parent);
    }

    private static int createSphereRegion(CommandContext<ServerCommandSource> ctx, String regionName, DimensionRegionCache dimCache, BlockPos centerPos, int radius, @Nullable IProtectedRegion parent) {
        SphereRegion region = new SphereRegion(regionName, new SphereArea(centerPos, radius), null, dimCache.dimensionKey());
        if (parent == null) {
            return createRegion(ctx, regionName, dimCache, region, dimCache.getDimensionalRegion());
        }
        return createRegion(ctx, regionName, dimCache, region, parent);
    }

    private static int attemptDeleteRegion(CommandContext<ServerCommandSource> ctx, DimensionRegionCache dim, IMarkableRegion region) {
        if (dim.contains(region.getName())) {
            MutableText removeRegionLink = buildRemoveRegionLink(region);
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.info.dim.region.remove.attempt", "Attempt to remove region %s from %s. Confirm by clicking here %s",
                    buildRegionInfoLink(region), buildRegionInfoLink(dim.getDimensionalRegion()), removeRegionLink));
            return 0;
        }
        return 1;
    }

    private static int deleteRegion(CommandContext<ServerCommandSource> ctx, DimensionRegionCache dim, IMarkableRegion region) {
        ServerPlayerEntity player;
        try {
            player = ctx.getSource().getPlayerOrThrow();
        } catch (CommandSyntaxException e) {
            player = null;
        }
        if (RegionEvents.DELETE_REGION.invoker().deleteRegion(new RegionEvent.RemoveRegionEvent(region, player))) {
            return 0;
        }
        if (dim.contains(region.getName())) {
            if (!region.getChildren().isEmpty()) {
                sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.info.dim.region.remove.fail.hasChildren", "Region %s can't be deleted because it has child regions.", buildRegionInfoLink(region)));
                return -1;
            }
            RegionType parentType = region.getParent().getRegionType();
            if (parentType == RegionType.DIMENSION) {
                dim.removeRegion(region);
                RegionDataManager.save();
                sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.info.dim.region.remove.confirm", "Removed region '%s' from %s", region.getName(), buildRegionInfoLink(dim.getDimensionalRegion())));
                return 0;
            }
            if (parentType == RegionType.LOCAL) {
                sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.info.dim.region.remove.fail.hasParent", "Region %s can't be deleted because it has a Local Regions as parent.", buildRegionInfoLink(region)));
                return 1;
            }
        }
        return 1;
    }

    private static int attemptDeleteRegions(CommandContext<ServerCommandSource> ctx, DimensionRegionCache dimCache) {
        int amount = dimCache.getRegionsInDimension().size();
        MutableText removeAllRegionsLink = buildRemoveAllRegionsLink(dimCache);
        sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.info.dim.region.remove.all.attempt", "Attempt to remove all (%s) regions from dimension %s. Confirm removal by clicking here %s",
                amount, buildRegionInfoLink(dimCache.getDimensionalRegion()), removeAllRegionsLink));
        return 0;
    }

    private static int deleteRegions(CommandContext<ServerCommandSource> ctx, DimensionRegionCache dimCache) {
        int amount = dimCache.getRegionsInDimension().size();
        dimCache.clearRegions();
        RegionDataManager.save();
        sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.info.dim.region.remove.all.confirm", "Removed %s regions from dimension %s", amount, buildRegionInfoLink(dimCache.getDimensionalRegion())));
        return 0;
    }

    /**
     * Note: Could, together with promptChildRegionList, be refactored to a single method
     */
    private static int promptDimensionRegionList(CommandContext<ServerCommandSource> ctx, DimensionRegionCache dimCache, int pageNo) {
        if (dimCache != null) {
            DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
            List<IProtectedRegion> regionsForDim = dimCache.getRegionsInDimension()
                    .values()
                    .stream()
                    .map(r -> (IProtectedRegion) r)
                    .sorted(Comparator.comparing(IProtectedRegion::getName))
                    .toList();
            if (regionsForDim.isEmpty()) {
                sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.dim.info.regions.empty", "No regions defined in %s", buildRegionInfoLink(dimCache.getDimensionalRegion())));
                return -1;
            }
            MutableText dimRegionsHeader = buildHeader(Text.translatableWithFallback("cli.msg.info.header.in", "== %s in %s ==", buildDimRegionsLink(dimCache), buildRegionInfoLink(dimRegion)));
            List<MutableText> regionPagination = buildPaginationComponents(
                    dimRegionsHeader,
                    buildCommandStr(DIM.toString(), dimRegion.getName(), LIST.toString(), CommandConstants.LOCAL.toString()),
                    buildRemoveRegionEntries(dimRegion, regionsForDim),
                    pageNo,
                    Text.literal(" - ").append(buildDimCreateRegionLink(dimRegion)));
            regionPagination.forEach(line -> sendCmdFeedback(ctx.getSource(), line));
            return 0;
        }
        return 1;
    }
}