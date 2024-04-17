package de.z0rdak.yawp.commands;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.api.events.region.RegionEvent;
import de.z0rdak.yawp.commands.arguments.region.RegionArgumentType;
import de.z0rdak.yawp.config.server.RegionConfig;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.area.SphereArea;
import de.z0rdak.yawp.core.region.CuboidRegion;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.SphereRegion;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.LocalRegions;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.SharedSuggestionProvider;
import net.minecraft.commands.arguments.DimensionArgument;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.commands.arguments.TeamArgument;
import net.minecraft.commands.arguments.coordinates.BlockPosArgument;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.resources.ResourceKey;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.level.Level;
import net.minecraft.world.scores.PlayerTeam;
import org.apache.commons.lang3.NotImplementedException;
import net.minecraftforge.common.MinecraftForge;

import javax.annotation.Nullable;
import java.util.*;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.commands.CommandUtil.*;
import static de.z0rdak.yawp.commands.arguments.ArgumentUtil.*;
import static de.z0rdak.yawp.util.MessageUtil.*;

public class DimensionCommands {

    private DimensionCommands() {
    }

    private static List<String> getRegionNameSuggestions() {
        String examples = new TranslatableComponent("cli.region.name.examples").getString();
        return Arrays.asList(examples.split(","));
    }

    public static String getRandomExample() {
        List<String> regionNameSuggestions = getRegionNameSuggestions();
        return regionNameSuggestions.get(new Random().nextInt(regionNameSuggestions.size()));
    }

    public static LiteralArgumentBuilder<CommandSourceStack> build() {
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
                                        .then(Commands.literal("-y")
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
                                        .executes(ctx -> CommandUtil.setAlertState(ctx, getDimCacheArgument(ctx).getDimensionalRegion(), !getDimCacheArgument(ctx).getDimensionalRegion().isMuted()))
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
                                                                        .then(Commands.argument(CommandConstants.OWNER.toString(), EntityArgument.player())
                                                                                .executes(ctx -> createCuboidRegion(ctx, getRegionNameArgument(ctx), getDimCacheArgument(ctx),
                                                                                        BlockPosArgument.getSpawnablePos(ctx, POS1.toString()),
                                                                                        BlockPosArgument.getSpawnablePos(ctx, POS2.toString()), getOwnerArgument(ctx))))))
                                                )
                                                .then(Commands.literal(AreaType.SPHERE.areaType)
                                                        .then(Commands.argument(CENTER_POS.toString(), BlockPosArgument.blockPos())
                                                                .then(Commands.argument(RADIUS_POS.toString(), BlockPosArgument.blockPos())
                                                                        .executes(ctx -> createSphereRegion(ctx, getRegionNameArgument(ctx), getDimCacheArgument(ctx),
                                                                                BlockPosArgument.getSpawnablePos(ctx, CENTER_POS.toString()),
                                                                                BlockPosArgument.getSpawnablePos(ctx, RADIUS_POS.toString()), null))
                                                                        .then(Commands.argument(CommandConstants.OWNER.toString(), EntityArgument.player())
                                                                                .executes(ctx -> createSphereRegion(ctx, getRegionNameArgument(ctx), getDimCacheArgument(ctx),
                                                                                        BlockPosArgument.getSpawnablePos(ctx, CENTER_POS.toString()),
                                                                                        BlockPosArgument.getSpawnablePos(ctx, RADIUS_POS.toString()), getOwnerArgument(ctx))))))
                                                )
                                                .then(Commands.literal(AreaType.SPHERE.areaType)
                                                        .then(Commands.argument(CENTER_POS.toString(), BlockPosArgument.blockPos())
                                                                .then(Commands.argument(RADIUS.toString(), IntegerArgumentType.integer())
                                                                        .executes(ctx -> createSphereRegion(ctx, getRegionNameArgument(ctx), getDimCacheArgument(ctx),
                                                                                BlockPosArgument.getSpawnablePos(ctx, CENTER_POS.toString()),
                                                                                IntegerArgumentType.getInteger(ctx, RADIUS.toString()), null))
                                                                        .then(Commands.argument(CommandConstants.OWNER.toString(), EntityArgument.player())
                                                                                .executes(ctx -> createSphereRegion(ctx, getRegionNameArgument(ctx), getDimCacheArgument(ctx),
                                                                                        BlockPosArgument.getSpawnablePos(ctx, CENTER_POS.toString()),
                                                                                        IntegerArgumentType.getInteger(ctx, RADIUS.toString()), getOwnerArgument(ctx))))))
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
            String state = enable ? "Enabled" : "Disabled";
            sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.info.region.state.enable.all.set.value",
                    state, buildRegionInfoLink(dimCache.getDimensionalRegion())));
            RegionDataManager.save();
            return 0;
        } else {
            return 1;
        }
    }

    private static int setAlertStateForAllLocal(CommandContext<CommandSourceStack> ctx, DimensionRegionCache dimCache, boolean mute) {
        if (dimCache != null) {
            dimCache.getRegionsInDimension().values().forEach(region -> region.setIsMuted(mute));
            String state = mute ? "Enabled" : "Disabled";
            sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.info.region.state.alert.all.set.value",
                    state, buildRegionInfoLink(dimCache.getDimensionalRegion())));
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
    private static int resetLocalRegions(CommandContext<CommandSourceStack> ctx, DimensionRegionCache dimCache) {
        dimCache.getRegionsInDimension().values().forEach(region -> {
            region.resetGroups();
            region.setIsActive(true);
            region.setIsMuted(false);
        });
        RegionDataManager.save();
        sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.info.dim.reset.all.confirm", buildRegionInfoLink(dimCache.getDimensionalRegion())));
        return 0;
    }

    /**
     * Reset groups (players and teams) and state for the dimensional region.<br></br>
     * This keeps region hierarchy and flags intact.<br></br>
     */
    private static int resetDimRegion(CommandContext<CommandSourceStack> ctx, DimensionRegionCache dimCache) {
        dimCache.getDimensionalRegion().resetGroups();
        dimCache.getDimensionalRegion().setIsActive(true);
        dimCache.getDimensionalRegion().setIsMuted(false);
        dimCache.getDimensionalRegion().getFlagContainer().clear();
        RegionDataManager.save();
        sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.info.dim.reset.confirm", buildRegionInfoLink(dimCache.getDimensionalRegion())));
        return 0;
    }

    private static int createRegion(CommandContext<CommandSourceStack> ctx, String regionName, DimensionRegionCache dimCache, IMarkableRegion region) {
        int res = RegionDataManager.get().isValidRegionName(dimCache.getDimensionalRegion().getDim(), regionName);
        if (res == -1) {
            sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.dim.info.region.create.name.invalid", regionName));
            return res;
        }
        if (res == 1) {
            sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.dim.info.region.create.name.exists", dimCache.getDimensionalRegion().getName(), buildRegionInfoLink(dimCache.getRegion(regionName))));
            return res;
        }
        ServerPlayer player;
        try {
            player = ctx.getSource().getPlayerOrException();
        } catch (CommandSyntaxException e) {
            player = null;
        }

        if(MinecraftForge.EVENT_BUS.post(new RegionEvent.CreateRegionEvent(region, player))) {
            return 0;
        }

        RegionDataManager.addFlags(RegionConfig.getDefaultFlags(), region);
        dimCache.addRegion(dimCache.getDimensionalRegion(), region);
        LocalRegions.ensureHigherRegionPriorityFor(region, RegionConfig.getDefaultPriority());
        RegionDataManager.save();
        sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.dim.info.region.create.success", buildRegionInfoLink(region)));
        return 0;
    }

    private static int createCuboidRegion(CommandContext<CommandSourceStack> ctx, String regionName, DimensionRegionCache dimCache, BlockPos pos1, BlockPos pos2, @Nullable ServerPlayer owner) {
        CuboidRegion region = new CuboidRegion(regionName, new CuboidArea(pos1, pos2), owner, dimCache.dimensionKey());
        return createRegion(ctx, regionName, dimCache, region);
    }

    private static int createSphereRegion(CommandContext<CommandSourceStack> ctx, String regionName, DimensionRegionCache dimCache, BlockPos centerPos, BlockPos radiusPos, @Nullable ServerPlayer owner) {
        SphereRegion region = new SphereRegion(regionName, new SphereArea(centerPos, radiusPos), owner, dimCache.dimensionKey());
        return createRegion(ctx, regionName, dimCache, region);
    }

    private static int createSphereRegion(CommandContext<CommandSourceStack> ctx, String regionName, DimensionRegionCache dimCache, BlockPos centerPos, int radius, @Nullable ServerPlayer owner) {
        SphereRegion region = new SphereRegion(regionName, new SphereArea(centerPos, radius), owner, dimCache.dimensionKey());
        return createRegion(ctx, regionName, dimCache, region);
    }

    private static int attemptDeleteRegion(CommandContext<CommandSourceStack> ctx, DimensionRegionCache dim, IMarkableRegion region) {
        if (dim.contains(region.getName())) {
            MutableComponent removeRegionLink = buildRemoveRegionLink(region);
            sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.info.dim.region.remove.attempt",
                    buildRegionInfoLink(region), buildRegionInfoLink(dim.getDimensionalRegion(), removeRegionLink)));
            return 0;
        }
        return 1;
    }

    private static int deleteRegion(CommandContext<CommandSourceStack> ctx, DimensionRegionCache dim, IMarkableRegion region) {
        ServerPlayer player;
        try {
            player = ctx.getSource().getPlayerOrException();
        } catch (CommandSyntaxException e) {
            player = null;
        }

        if(MinecraftForge.EVENT_BUS.post(new RegionEvent.RemoveRegionEvent(region, player))) {
            return 0;
        }
        if (dim.contains(region.getName())) {
            if (!region.getChildren().isEmpty()) {
                sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.info.dim.region.remove.fail.hasChildren", buildRegionInfoLink(region)));
                return -1;
            }
            if (region.getParent() != null) {
                // TODO: Check, does not seem to work according to bug report
                /* Reproduce: Parent P, Child C
                1. delete C
                2. Save
                3. Check if Parent P still has child C
                */
                region.getParent().removeChild(region);
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(region.getDim());
                dimCache.getDimensionalRegion().addChild(region);
            }
            dim.removeRegion(region);
            RegionDataManager.save();
            sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.info.dim.region.remove.confirm", region.getName(), buildRegionInfoLink(dim.getDimensionalRegion())));
            return 0;
        }
        return 1;
    }

    private static int attemptDeleteRegions(CommandContext<CommandSourceStack> ctx, DimensionRegionCache dimCache) {
        int amount = dimCache.getRegionsInDimension().size();
        MutableComponent removeAllRegionsLink = buildRemoveAllRegionsLink(dimCache);
        sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.info.dim.region.remove.all.attempt",
                amount, buildRegionInfoLink(dimCache.getDimensionalRegion()), removeAllRegionsLink));
        return 0;
    }

    private static int deleteRegions(CommandContext<CommandSourceStack> ctx, DimensionRegionCache dimCache) {
        int amount = dimCache.getRegionsInDimension().size();
        dimCache.clearRegions();
        RegionDataManager.save();
        sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.info.dim.region.remove.all.confirm", amount, buildRegionInfoLink(dimCache.getDimensionalRegion())));
        return 0;
    }

    private static int promptDimensionRegionList(CommandContext<CommandSourceStack> ctx, DimensionRegionCache dimCache, int pageNo) {
        if (dimCache != null) {
            DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
            List<IMarkableRegion> regionsForDim = dimCache.getRegionsInDimension()
                    .values()
                    .stream()
                    .sorted(Comparator.comparing(IMarkableRegion::getName))
                    .toList();
            if (regionsForDim.isEmpty()) {
                sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.dim.info.regions.empty", buildRegionInfoLink(dimCache.getDimensionalRegion())));
                return -1;
            }
            List<MutableComponent> regionPagination = buildPaginationComponents(
                    buildRegionListHeader(dimRegion),
                    buildCommandStr(DIM.toString(), dimRegion.getName(), LIST.toString(), CommandConstants.LOCAL.toString()),
                    buildRemoveRegionEntries(dimRegion, regionsForDim),
                    pageNo,
                    new TextComponent(" - ").append(buildDimCreateRegionLink(dimRegion)));
            regionPagination.forEach(line -> sendCmdFeedback(ctx.getSource(), line));
            return 0;
        }
        return 1;
    }
}