package de.z0rdak.yawp.commands;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
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

import javax.annotation.Nonnull;
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

    public static final List<String> regionNameSuggestions = Arrays.asList("newRegion", "spawn", "home", "town", "arena");

    // FIXME: typing in invalid dimension is adding this dimension to set of dims
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
                                .then(literal(REGION)
                                        .executes(ctx -> promptDimensionRegionList(ctx, getDimCacheArgument(ctx), 0))
                                        .then(Commands.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                                .executes(ctx -> promptDimensionRegionList(ctx, getDimCacheArgument(ctx), getPageNoArgument(ctx)))))
                        )
                        .then(literal(DELETE)
                                .then(Commands.argument(REGION.toString(), StringArgumentType.word())
                                        .suggests((ctx, builder) -> RegionArgumentType.region().listSuggestions(ctx, builder))
                                        .executes(ctx -> attemptDeleteRegion(ctx, getDimCacheArgument(ctx), getRegionArgument(ctx)))
                                        .then(Commands.literal("-y")
                                                .executes(ctx -> deleteRegion(ctx, getDimCacheArgument(ctx), getRegionArgument(ctx))))))
                        .then(literal(DELETE_ALL)
                                .then(literal(REGIONS)
                                        .executes(ctx -> deleteRegions(ctx, getDimCacheArgument(ctx)))
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
                                .then(literal(ENABLE)
                                        .executes(ctx -> CommandUtil.setAlertState(ctx, getDimCacheArgument(ctx).getDimensionalRegion(), !getDimCacheArgument(ctx).getDimensionalRegion().isActive()))
                                        .then(Commands.argument(ENABLE.toString(), BoolArgumentType.bool())
                                                .executes(ctx -> CommandUtil.setAlertState(ctx, getDimCacheArgument(ctx).getDimensionalRegion(), getEnableArgument(ctx))))
                                )
                        )
                        .then(literal(CREATE)
                                .then(literal(REGION)
                                        .then(Commands.argument(REGION.toString(), StringArgumentType.word())
                                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(Collections.singletonList(regionNameSuggestions.get(new Random().nextInt(regionNameSuggestions.size()))), builder))
                                                //.then(Commands.argument(AREA.toString(), StringArgumentType.word())
                                                //        .suggests((ctx, builder) -> AreaArgumentType.areaType().listSuggestions(ctx, builder))
                                                //        .executes(ctx -> createRegion(ctx.getSource(), getRegionNameArgument(ctx), getDimCacheArgument(ctx), getAreaTypeArgument(ctx))))
                                                .then(Commands.literal(AreaType.CUBOID.areaType)
                                                        .then(Commands.argument("pos1", BlockPosArgument.blockPos())
                                                                .then(Commands.argument("pos2", BlockPosArgument.blockPos())
                                                                        .executes(ctx -> createCuboidRegion(ctx, getRegionNameArgument(ctx), getDimCacheArgument(ctx),
                                                                                BlockPosArgument.getSpawnablePos(ctx, "pos1"),
                                                                                BlockPosArgument.getSpawnablePos(ctx, "pos2"), null))
                                                                        .then(Commands.argument(CommandConstants.OWNER.toString(), EntityArgument.player())
                                                                                .executes(ctx -> createCuboidRegion(ctx, getRegionNameArgument(ctx), getDimCacheArgument(ctx),
                                                                                        BlockPosArgument.getSpawnablePos(ctx, "pos1"),
                                                                                        BlockPosArgument.getSpawnablePos(ctx, "pos2"), getOwnerArgument(ctx))))))
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


    private static int resetLocalRegions(CommandContext<CommandSourceStack> ctx, DimensionRegionCache dimCache) {
        return 0;
    }

    private static int deleteRegions(CommandContext<CommandSourceStack> ctx, DimensionRegionCache dimCache) {
        return 0;
    }

    private static int resetDimRegion(CommandContext<CommandSourceStack> ctx, DimensionRegionCache dimCache) {
        return 0;
    }

    public static int checkValidRegionName(String regionName, DimensionRegionCache dimCache) {
        List<String> commandStrings = Arrays.stream(values()).map(CommandConstants::toString).collect(Collectors.toList());
        if (!regionName.matches(RegionArgumentType.VALID_NAME_PATTERN.pattern())
                || commandStrings.contains(regionName.toLowerCase())) {
            return -1;
        }
        if (dimCache.contains(regionName)) {
            return 1;
        }
        return 0;
    }

    private static int createCuboidRegion(CommandContext<CommandSourceStack> ctx, String regionName, DimensionRegionCache dimCache, BlockPos pos1, BlockPos pos2, @Nullable ServerPlayerEntity owner) {
        int res = checkValidRegionName(regionName, dimCache);
        if (res == -1) {
            sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.dim.info.region.create.name.invalid", regionName));
            return res;
        }
        if (res == 1) {
            sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.dim.info.region.create.name.exists", dimCache.getDimensionalRegion().getName(), buildRegionInfoLink(dimCache.getRegion(regionName))));
            return res;
        }
        CuboidRegion region = new CuboidRegion(regionName, new CuboidArea(pos1, pos2), owner, dimCache.dimensionKey());
        RegionDataManager.addFlags(RegionConfig.getDefaultFlags(), region);
        dimCache.addRegion(region);
        LocalRegions.ensureHigherRegionPriorityFor(region, RegionConfig.getDefaultPriority());
        RegionDataManager.save();
        sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.dim.info.region.create.success", buildRegionInfoLink(region)));
        return 0;
    }

    private static int createSphereRegion(CommandContext<CommandSourceStack> ctx, @Nonnull String regionName, DimensionRegionCache dimCache, BlockPos center, BlockPos outerPos, ServerPlayerEntity owner) {
        int res = checkValidRegionName(regionName, dimCache);
        if (res == -1) {
            sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.dim.info.region.create.name.invalid", regionName));
            return res;
        }
        if (res == 1) {
            sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.dim.info.region.create.name.exists", dimCache.getDimensionalRegion().getName(), buildRegionInfoLink(dimCache.getRegion(regionName))));
            return res;
        }
        SphereArea area = new SphereArea(center, outerPos);
        SphereRegion region = new SphereRegion(regionName, area, owner, dimCache.dimensionKey());
        RegionDataManager.addFlags(RegionConfig.getDefaultFlags(), region);
        dimCache.addRegion(region);
        RegionDataManager.save();
        sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.dim.info.region.create.success", buildRegionInfoLink(region)));
        return 0;
    }

    private static int attemptDeleteRegion(CommandContext<CommandSourceStack> ctx, DimensionRegionCache dim, IMarkableRegion region) {
        if (dim.contains(region.getName())) {
            sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.info.dim.region.remove.attempt", buildRegionInfoLink(region), buildRegionInfoLink(dim.getDimensionalRegion())));
            return 0;
        }
        return 1;
    }

    // FIXME: Are child / parent relation properly removed when deleting a region?
    private static int deleteRegion(CommandContext<CommandSourceStack> ctx, DimensionRegionCache dim, IMarkableRegion region) {
        if (dim.contains(region.getName())) {
            if (!region.getChildren().isEmpty()) {
                // TODO: config option which allows deleting region with children? children then default to dim parent
                sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.info.dim.region.remove.fail.hasChildren", buildRegionInfoLink(region)));
                return -1;
            }
            if (region.getParent() != null) {
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
                    buildCommandStr(DIM.toString(), dimRegion.getName(), LIST.toString(), REGION.toString()),
                    buildRemoveRegionEntries(dimRegion, regionsForDim),
                    pageNo,
                    new TextComponent(" - ").append(buildDimCreateRegionLink(dimRegion)));
            regionPagination.forEach(line -> sendCmdFeedback(ctx.getSource(), line));
            return 0;
        }
        return 1;
    }
}