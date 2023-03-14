package de.z0rdak.yawp.commands;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import de.z0rdak.yawp.commands.arguments.region.RegionArgumentType;
import de.z0rdak.yawp.config.server.RegionConfig;
import de.z0rdak.yawp.core.affiliation.AffiliationType;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.area.SphereArea;
import de.z0rdak.yawp.core.flag.BooleanFlag;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.*;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.LocalRegions;
import net.minecraft.command.CommandSource;
import net.minecraft.command.argument.BlockPosArgumentType;
import net.minecraft.command.argument.DimensionArgumentType;
import net.minecraft.command.argument.EntityArgumentType;
import net.minecraft.command.argument.TeamArgumentType;
import net.minecraft.scoreboard.Team;
import net.minecraft.server.command.CommandManager;
import net.minecraft.server.command.ServerCommandSource;
import net.minecraft.server.network.ServerPlayerEntity;
import net.minecraft.text.MutableText;
import net.minecraft.text.Text;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.registry.RegistryKey;
import net.minecraft.world.World;

import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.core.region.RegionType.LOCAL;
import static de.z0rdak.yawp.util.CommandUtil.*;
import static de.z0rdak.yawp.util.MessageUtil.*;

public class DimensionCommands {

    private DimensionCommands() {
    }

    public static final LiteralArgumentBuilder<ServerCommandSource> DIMENSION_COMMAND = register();

    public static LiteralArgumentBuilder<ServerCommandSource> register() {
        List<String> affiliationList = Arrays.asList(RegionCommands.MEMBER, RegionCommands.OWNER);
        return literal(CommandConstants.DIM)
                /* /wp dimension <dim> list region */
                .then(CommandManager.argument(CommandConstants.DIM.toString(), DimensionArgumentType.dimension())
                        .then(literal(CREATE)
                                .then(literal(REGION)
                                        .then(CommandManager.argument(REGION.toString(), StringArgumentType.word())
                                                .suggests((ctx, builder) -> CommandSource.suggestMatching(Collections.singletonList("name"), builder))
                                                //.then(CommandManager.argument(AREA.toString(), StringArgumentType.word())
                                                //        .suggests((ctx, builder) -> AreaArgumentType.areaType().listSuggestions(ctx, builder))
                                                //        .executes(ctx -> createRegion(ctx.getSource(), getRegionNameArgument(ctx), getDimCacheArgument(ctx), getAreaTypeArgument(ctx))))
                                                .then(CommandManager.literal(AreaType.CUBOID.areaType)
                                                        .then(CommandManager.argument("pos1", BlockPosArgumentType.blockPos())
                                                                .then(CommandManager.argument("pos2", BlockPosArgumentType.blockPos())
                                                                        .executes(ctx -> createCuboidRegion(ctx.getSource(), getRegionNameArgument(ctx), getDimCacheArgument(ctx),
                                                                                BlockPosArgumentType.getBlockPos(ctx, "pos1"),
                                                                                BlockPosArgumentType.getBlockPos(ctx, "pos2"), null))
                                                                        .then(CommandManager.argument(OWNER.toString(), EntityArgumentType.player())
                                                                                .executes(ctx -> createCuboidRegion(ctx.getSource(), getRegionNameArgument(ctx), getDimCacheArgument(ctx),
                                                                                        BlockPosArgumentType.getBlockPos(ctx, "pos1"),
                                                                                        BlockPosArgumentType.getBlockPos(ctx, "pos2"), getOwnerArgument(ctx))))))
                                                )
                                        )
                                )
                        )
                        /* /wp dimension <dim> [info] */
                        .executes(ctx -> promptDimensionInfo(ctx.getSource(), getDimCacheArgument(ctx)))
                        .then(literal(INFO).executes(ctx -> promptDimensionInfo(ctx.getSource(), getDimCacheArgument(ctx))))
                        /* /wp dimension <dim> activate */
                        .then(literal(ENABLE)
                                // TODO: Add toggle cmd
                                .executes(ctx -> setActiveState(ctx.getSource(), getDimCacheArgument(ctx), true))
                                .then(CommandManager.argument(ENABLE.toString(), BoolArgumentType.bool())
                                        .executes(ctx -> setActiveState(ctx.getSource(), getDimCacheArgument(ctx), getEnableArgument(ctx)))))
                        .then(literal(LIST)
                                .then(literal(REGION)
                                        .executes(ctx -> promptDimensionRegionList(ctx.getSource(), getDimCacheArgument(ctx), 0))
                                        .then(CommandManager.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                                .executes(ctx -> promptDimensionRegionList(ctx.getSource(), getDimCacheArgument(ctx), getPageNoArgument(ctx)))))
                                /* /wp dimension <dim> list owner */
                                .then(literal(OWNER)
                                        .executes(ctx -> promptDimensionAffiliates(ctx.getSource(), getDimCacheArgument(ctx), OWNER.toString()))
                                        .then(literal(TEAM)
                                                .executes(ctx -> promptDimensionAffiliationList(ctx.getSource(), getDimCacheArgument(ctx), OWNER.toString(), AffiliationType.TEAM, 0))
                                                .then(CommandManager.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                                        .executes(ctx -> promptDimensionAffiliationList(ctx.getSource(), getDimCacheArgument(ctx), OWNER.toString(), AffiliationType.TEAM, getPageNoArgument(ctx)))))
                                        .then(literal(PLAYER)
                                                .executes(ctx -> promptDimensionAffiliationList(ctx.getSource(), getDimCacheArgument(ctx), OWNER.toString(), AffiliationType.PLAYER, 0))
                                                .then(CommandManager.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                                        .executes(ctx -> promptDimensionAffiliationList(ctx.getSource(), getDimCacheArgument(ctx), OWNER.toString(), AffiliationType.PLAYER, getPageNoArgument(ctx))))
                                        )
                                )
                                /* /wp dimension <dim> list member */
                                .then(literal(MEMBER)
                                        .executes(ctx -> promptDimensionAffiliates(ctx.getSource(), getDimCacheArgument(ctx), MEMBER.toString()))
                                        .then(literal(TEAM)
                                                .executes(ctx -> promptDimensionAffiliationList(ctx.getSource(), getDimCacheArgument(ctx), MEMBER.toString(), AffiliationType.TEAM, 0))
                                                .then(CommandManager.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                                        .executes(ctx -> promptDimensionAffiliationList(ctx.getSource(), getDimCacheArgument(ctx), MEMBER.toString(), AffiliationType.TEAM, getPageNoArgument(ctx)))))
                                        .then(literal(PLAYER)
                                                .executes(ctx -> promptDimensionAffiliationList(ctx.getSource(), getDimCacheArgument(ctx), MEMBER.toString(), AffiliationType.PLAYER, 0))
                                                .then(CommandManager.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                                        .executes(ctx -> promptDimensionAffiliationList(ctx.getSource(), getDimCacheArgument(ctx), MEMBER.toString(), AffiliationType.PLAYER, getPageNoArgument(ctx))))
                                        )
                                )
                                /* /wp dimension <dim> list flag */
                                .then(literal(FLAG)
                                        .executes(ctx -> promptDimensionFlagList(ctx.getSource(), getDimCacheArgument(ctx), 0))
                                        .then(CommandManager.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                                .executes(ctx -> promptDimensionFlagList(ctx.getSource(), getDimCacheArgument(ctx), getPageNoArgument(ctx))))))
                        .then(literal(DELETE)
                                .then(CommandManager.argument(REGION.toString(), StringArgumentType.word())
                                        .suggests((ctx, builder) -> RegionArgumentType.region().listSuggestions(ctx, builder))
                                        .executes(ctx -> attemptDeleteRegion(ctx.getSource(), getDimCacheArgument(ctx), getRegionArgument(ctx)))
                                        .then(CommandManager.literal("-y")
                                                .executes(ctx -> deleteRegion(ctx.getSource(), getDimCacheArgument(ctx), getRegionArgument(ctx))))))
                        .then(literal(REMOVE)
                                .then(literal(PLAYER)
                                        .then(CommandManager.argument(AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> CommandSource.suggestMatching(affiliationList, builder))
                                                .then(CommandManager.argument(PLAYER.toString(), EntityArgumentType.player())
                                                        .executes(ctx -> removePlayer(ctx.getSource(), getPlayerArgument(ctx), getDimCacheArgument(ctx), getAffiliationArgument(ctx)))))

                                        .then(CommandManager.argument(AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> CommandSource.suggestMatching(affiliationList, builder))
                                                .then(CommandManager.argument(PLAYER.toString(), EntityArgumentType.player())
                                                        .executes(ctx -> removePlayer(ctx.getSource(), getPlayerArgument(ctx), getDimCacheArgument(ctx), getAffiliationArgument(ctx))))))
                                .then(literal(TEAM)
                                        // TODO: only list contained list for removal
                                        .then(CommandManager.argument(AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> CommandSource.suggestMatching(affiliationList, builder))
                                                .then(CommandManager.argument(TEAM.toString(), TeamArgumentType.team())
                                                        .executes(ctx -> removeTeam(ctx.getSource(), getTeamArgumentType(ctx), getDimCacheArgument(ctx), getAffiliationArgument(ctx)))))

                                        .then(CommandManager.argument(AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> CommandSource.suggestMatching(affiliationList, builder))
                                                .then(CommandManager.argument(TEAM.toString(), TeamArgumentType.team())
                                                        .executes(ctx -> removeTeam(ctx.getSource(), getTeamArgumentType(ctx), getDimCacheArgument(ctx), getAffiliationArgument(ctx))))))
                                .then(literal(FLAG)
                                        .then(CommandManager.argument(FLAG.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> CommandSource.suggestMatching(RegionDataManager.get().getFlagsIdsForDim(getDimCacheArgument(ctx)), builder))
                                                .executes(ctx -> removeFlag(ctx.getSource(), getDimCacheArgument(ctx), StringArgumentType.getString(ctx, FLAG.toString()))))))
                        .then(literal(ADD)
                                .then(literal(PLAYER)
                                        .then(CommandManager.argument(AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> CommandSource.suggestMatching(affiliationList, builder))
                                                .then(CommandManager.argument(PLAYER.toString(), EntityArgumentType.player())
                                                        .executes(ctx -> addPlayer(ctx.getSource(), getPlayerArgument(ctx), getDimCacheArgument(ctx), getAffiliationArgument(ctx)))))

                                        .then(CommandManager.argument(AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> CommandSource.suggestMatching(affiliationList, builder))
                                                .then(CommandManager.argument(PLAYER.toString(), EntityArgumentType.player())
                                                        .executes(ctx -> addPlayer(ctx.getSource(), getPlayerArgument(ctx), getDimCacheArgument(ctx), getAffiliationArgument(ctx))))))
                                .then(literal(TEAM)
                                        .then(CommandManager.argument(AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> CommandSource.suggestMatching(affiliationList, builder))
                                                .then(CommandManager.argument(TEAM.toString(), TeamArgumentType.team())
                                                        .executes(ctx -> addTeam(ctx.getSource(), getTeamArgumentType(ctx), getDimCacheArgument(ctx), getAffiliationArgument(ctx)))))

                                        .then(CommandManager.argument(AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> CommandSource.suggestMatching(affiliationList, builder))
                                                .then(CommandManager.argument(TEAM.toString(), TeamArgumentType.team())
                                                        .executes(ctx -> addTeam(ctx.getSource(), getTeamArgumentType(ctx), getDimCacheArgument(ctx), getAffiliationArgument(ctx))))))
                                .then(literal(FLAG)
                                        .then(CommandManager.argument(FLAG.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> CommandSource.suggestMatching(RegionFlag.getFlagNames(), builder))
                                                .executes(ctx -> addFlag(ctx.getSource(), getDimCacheArgument(ctx), StringArgumentType.getString(ctx, FLAG.toString())))))));
    }

    public static int checkValidRegionName(String regionName, DimensionRegionCache dimCache) {
        if (!regionName.matches(RegionArgumentType.VALID_NAME_PATTERN.pattern())) {
            return -1;
        }
        if (dimCache.contains(regionName)) {
            return 1;
        }
        return 0;
    }

    private static int createCuboidRegion(ServerCommandSource src, String regionName, DimensionRegionCache dimCache, BlockPos pos1, BlockPos pos2, ServerPlayerEntity owner) {
        int res = checkValidRegionName(regionName, dimCache);
        if (res == -1) {
            sendCmdFeedback(src, Text.translatable("cli.msg.dim.info.region.create.name.invalid", regionName));
            return res;
        }
        if (res == 1) {

            sendCmdFeedback(src, Text.translatable("cli.msg.dim.info.region.create.name.exists", dimCache.getDimensionalRegion().getName(), buildRegionInfoLink(dimCache.getRegion(regionName), LOCAL)));
            return res;
        }
        CuboidRegion region = new CuboidRegion(regionName, new CuboidArea(pos1, pos2), owner, dimCache.dimensionKey());
        RegionDataManager.addFlags(RegionConfig.getDefaultFlags(), region);
        dimCache.addRegion(region);
        LocalRegions.ensureHigherRegionPriorityFor(region, RegionConfig.DEFAULT_REGION_PRIORITY.get());
        RegionDataManager.save();
        sendCmdFeedback(src, Text.translatable("cli.msg.dim.info.region.create.success", buildRegionInfoLink(region, LOCAL)));
        return 0;
    }

    private static int createSphereRegion(ServerCommandSource src, String regionName, DimensionRegionCache dimCache, BlockPos center, BlockPos outerPos, ServerPlayerEntity owner) {
        if (!regionName.matches(RegionArgumentType.VALID_NAME_PATTERN.pattern())) {
            sendCmdFeedback(src, Text.translatable("cli.msg.dim.info.region.create.name.invalid", regionName));
            return -1;
        }
        if (dimCache.contains(regionName)) {
            sendCmdFeedback(src, Text.translatable("cli.msg.dim.info.region.create.name.exists", dimCache.dimensionKey(), regionName));
            return 1;
        }
        SphereArea area = new SphereArea(center, outerPos);
        SphereRegion region = new SphereRegion(regionName, area, owner, dimCache.dimensionKey());
        RegionDataManager.addFlags(RegionConfig.getDefaultFlags(), region);
        dimCache.addRegion(region);
        RegionDataManager.save();
        sendCmdFeedback(src, Text.translatable("cli.msg.dim.info.region.create.success", buildRegionInfoLink(region, LOCAL)));
        return 0;
    }

    private static int attemptDeleteRegion(ServerCommandSource src, DimensionRegionCache dim, IMarkableRegion region) {
        if (dim.contains(region.getName())) {
            sendCmdFeedback(src, Text.translatable("cli.msg.info.dim.region.remove.attempt", buildRegionInfoLink(region, LOCAL), buildRegionInfoLink(dim.getDimensionalRegion(), RegionType.DIMENSION)));
            return 0;
        }
        return 1;
    }

    // FIXME: Are child / parent relation properly removed when deleting a region?
    private static int deleteRegion(ServerCommandSource src, DimensionRegionCache dim, IMarkableRegion region) {
        if (dim.contains(region.getName())) {
            if (!region.getChildren().isEmpty()) {
                // TODO: config option which allows deleting region with children? children then default to dim parent
                sendCmdFeedback(src, Text.translatable("cli.msg.info.dim.region.remove.fail.hasChildren", buildRegionInfoLink(region, LOCAL)));
                return -1;
            }
            if (region.getParent() != null) {
                region.getParent().removeChild(region);
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(region.getDim());
                dimCache.getDimensionalRegion().addChild(region);
            }
            dim.removeRegion(region);
            RegionDataManager.save();
            sendCmdFeedback(src, Text.translatable("cli.msg.info.dim.region.remove.confirm", region.getName(), buildRegionInfoLink(dim.getDimensionalRegion(), RegionType.DIMENSION)));
            return 0;
        }
        return 1;
    }

    private static int removeFlag(ServerCommandSource src, DimensionRegionCache dimCache, String flag) {
        if (dimCache != null) {
            dimCache.removeFlag(flag);
            sendCmdFeedback(src, Text.translatable("cli.msg.flags.removed", flag, buildRegionInfoLink(dimCache.getDimensionalRegion(), RegionType.DIMENSION)));
            return 0;
        }
        return 1;
    }

    private static int addFlag(ServerCommandSource src, DimensionRegionCache dimCache, String flag) {
        // FIXME: For now this works because we only have condition flags and no black/whitelist feature
        IFlag iflag = new BooleanFlag(flag, false);
        if (dimCache != null) {
            dimCache.addFlag(iflag);
            // TODO: replace with flag info link
            sendCmdFeedback(src, Text.translatable("cli.msg.flags.added", flag, buildRegionInfoLink(dimCache.getDimensionalRegion(), RegionType.DIMENSION)));
            return 0;
        }
        return 1;
    }

    // TODO: Option to remove player by name
    private static int removePlayer(ServerCommandSource src, ServerPlayerEntity player, DimensionRegionCache dimCache, String affiliationType) {
        if (dimCache != null) {
            if (affiliationType.equals(MEMBER.toString())) {
                if (dimCache.getDimensionalRegion().hasMember(player.getUuid())) {
                    dimCache.getDimensionalRegion().removeMember(player);
                    MutableText playerInfo = buildAffiliateInfo(dimCache.getDimensionalRegion(), player.getEntityName(), AffiliationType.PLAYER);
                    sendCmdFeedback(src, Text.translatable("cli.msg.dim.info.player.removed", affiliationType, playerInfo, buildRegionInfoLink(dimCache.getDimensionalRegion(), RegionType.DIMENSION)));
                    RegionDataManager.save();
                    return 0;
                }
            }
            if (affiliationType.equals(OWNER.toString())) {
                if (dimCache.getDimensionalRegion().hasOwner(player.getUuid())) {
                    dimCache.getDimensionalRegion().removeOwner(player);
                    MutableText playerInfo = buildAffiliateInfo(dimCache.getDimensionalRegion(), player.getEntityName(), AffiliationType.PLAYER);
                    sendCmdFeedback(src, Text.translatable("cli.msg.dim.info.player.removed", affiliationType, playerInfo, buildRegionInfoLink(dimCache.getDimensionalRegion(), RegionType.DIMENSION)));
                    RegionDataManager.save();
                    return 0;
                }
            }
        }
        return 1;
    }

    private static int removeTeam(ServerCommandSource src, Team team, DimensionRegionCache dimCache, String affiliationType) {
        if (dimCache != null) {
            if (affiliationType.equals(MEMBER.toString())) {
                if (dimCache.getDimensionalRegion().hasMember(team.getName())) {
                    dimCache.getDimensionalRegion().removeMember(team);
                    sendCmdFeedback(src, Text.translatable("cli.msg.dim.info.player.removed", affiliationType, buildTeamHoverComponent(team), buildRegionInfoLink(dimCache.getDimensionalRegion(), RegionType.DIMENSION)));
                    RegionDataManager.save();
                    return 0;
                }
            }
            if (affiliationType.equals(OWNER.toString())) {
                if (dimCache.getDimensionalRegion().hasOwner(team.getName())) {
                    dimCache.getDimensionalRegion().removeOwner(team);
                    sendCmdFeedback(src, Text.translatable("cli.msg.dim.info.player.removed", affiliationType, buildTeamHoverComponent(team), buildRegionInfoLink(dimCache.getDimensionalRegion(), RegionType.DIMENSION)));
                    RegionDataManager.save();
                    return 0;
                }
            }
        }
        return 1;
    }

    // TODO: If works replace with switch and catch error
    private static int addPlayer(ServerCommandSource src, ServerPlayerEntity player, DimensionRegionCache dimCache, String affiliationType) {
        if (dimCache != null) {
            RegistryKey<World> dim = dimCache.dimensionKey();
            if (affiliationType.equals(MEMBER.toString())) {
                dimCache.addMember(player);
            }
            if (affiliationType.equals(OWNER.toString())) {
                dimCache.addOwner(player);
            }
            sendCmdFeedback(src, Text.translatable("cli.msg.dim.info.player.added", buildPlayerHoverComponent(player), dim.getValue().toString(), affiliationType));
            return 0;
        }
        return 1;
    }

    private static int addTeam(ServerCommandSource src, Team team, DimensionRegionCache dimCache, String affiliationType) {
        if (dimCache != null) {
            RegistryKey<World> dim = dimCache.dimensionKey();
            if (affiliationType.equals(MEMBER.toString())) {
                dimCache.addMember(team);
            }
            if (affiliationType.equals(OWNER.toString())) {
                dimCache.addOwner(team);
            }
            sendCmdFeedback(src, Text.translatable("cli.msg.dim.info.team.added", buildTeamHoverComponent(team), dim.getValue().toString(), affiliationType));
            return 0;
        }
        return 1;
    }

    private static int setActiveState(ServerCommandSource src, DimensionRegionCache dimCache, boolean activate) {
        if (dimCache != null) {
            dimCache.setDimState(activate);
            String langKey = "cli.msg.info.state." + (activate ? "activated" : "deactivated");
            sendCmdFeedback(src, Text.translatable(langKey, buildRegionInfoLink(dimCache.getDimensionalRegion(), RegionType.DIMENSION)));
            return 0;
        }
        return 1;
    }

    private static int promptDimensionRegionList(ServerCommandSource src, DimensionRegionCache dimCache, int pageNo) {
        if (dimCache != null) {
            DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
            List<IMarkableRegion> regionsForDim = dimCache.getRegionsInDimension()
                    .values()
                    .stream()
                    .sorted(Comparator.comparing(IMarkableRegion::getName))
                    .toList();
            if (regionsForDim.isEmpty()) {
                sendCmdFeedback(src, Text.translatable("cli.msg.dim.info.regions.empty", buildRegionInfoLink(dimCache.getDimensionalRegion(), RegionType.DIMENSION)));
                return -1;
            }
            List<MutableText> regionPagination = buildPaginationComponents(
                    buildDimRegionListHeader(dimRegion),
                    buildCommandStr(DIM.toString(), dimRegion.getName(), LIST.toString(), REGION.toString()),
                    buildRemoveRegionEntries(dimRegion, regionsForDim, RegionType.DIMENSION),
                    pageNo,
                    Text.literal((" - ")).append(buildDimCreateRegionLink(dimRegion)));
            regionPagination.forEach(line -> sendCmdFeedback(src, line));
            return 0;
        }
        return 1;
    }

    private static int promptDimensionFlagList(ServerCommandSource src, DimensionRegionCache dimCache, int pageNo) {
        List<IFlag> flags = LocalRegions.getSortedFlags(dimCache.getDimensionalRegion());
        if (flags.isEmpty()) {
            sendCmdFeedback(src, Text.translatable("cli.msg.dim.info.flags.empty", buildRegionInfoLink(dimCache.getDimensionalRegion(), RegionType.DIMENSION)));
            return 1;
        }
        List<MutableText> flagPagination = buildPaginationComponents(
                buildFlagHeader(dimCache.getDimensionalRegion(), RegionType.DIMENSION),
                buildCommandStr(DIM.toString(), dimCache.getDimensionalRegion().getName(), LIST.toString(), FLAG.toString()),
                buildRemoveFlagEntries(dimCache.getDimensionalRegion(), flags, RegionType.DIMENSION),
                pageNo,
                Text.literal((" - ")).append(buildDimAddFlagLink(dimCache.getDimensionalRegion())));
        flagPagination.forEach(line -> sendCmdFeedback(src, line));
        return 0;
    }

    private static int promptDimensionAffiliates(ServerCommandSource src, DimensionRegionCache dimCache, String affiliation) {
        DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
        sendCmdFeedback(src, buildAffiliationHeader(dimRegion, affiliation, RegionType.DIMENSION));
        sendCmdFeedback(src, buildAffiliationPlayerListLink(dimRegion, affiliation, RegionType.DIMENSION));
        sendCmdFeedback(src, buildAffiliationTeamListLink(dimRegion, affiliation, RegionType.DIMENSION));
        return 0;
    }

    private static int promptDimensionAffiliationList(ServerCommandSource src, DimensionRegionCache dimCache, String affiliation, AffiliationType affiliationType, int pageNo) {
        DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
        List<String> affiliateNames = getAffiliateList(dimRegion, affiliation, affiliationType);
        if (affiliateNames.isEmpty()) {
            sendCmdFeedback(src, Text.translatable("cli.msg.info.region.affiliation." + affiliationType.name + ".empty", affiliation, buildRegionInfoLink(dimCache.getDimensionalRegion(), RegionType.DIMENSION)));
            return 1;
        }
        List<MutableText> affiliatePagination = buildPaginationComponents(
                buildAffiliationHeader(dimRegion, affiliation, affiliationType, RegionType.DIMENSION),
                buildCommandStr(DIM.toString(), dimRegion.getDim().getValue().toString(), LIST.toString(), affiliation, affiliationType.name),
                buildRemoveAffiliationEntries(dimRegion, affiliateNames, affiliationType, affiliation, RegionType.DIMENSION),
                pageNo,
                Text.literal((" - ")).append(buildAddAffiliateLink(dimRegion, affiliation, affiliationType, RegionType.DIMENSION)));
        affiliatePagination.forEach(line -> sendCmdFeedback(src, line));
        return 0;
    }

    private static int promptDimensionInfo(ServerCommandSource src, DimensionRegionCache dimCache) {
        DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
        // [] header []
        sendCmdFeedback(src, buildRegionOverviewHeader(dimRegion, RegionType.DIMENSION));
        // [n region(s)]
        sendCmdFeedback(src, buildInfoComponent("cli.msg.info.dim.region", buildRegionChildrenLink(dimCache.getDimensionalRegion(), RegionType.DIMENSION)));
        // Affiliations: [owners], [members], [<listAffiliations>]
        sendCmdFeedback(src, buildInfoComponent("cli.msg.info.region.affiliation", buildAffiliationLinks(dimRegion, RegionType.DIMENSION)));
        // Flags: [n flag(s)] [+]
        sendCmdFeedback(src, buildInfoComponent("cli.msg.info.region.flag", buildFlagListLink(dimRegion, RegionType.DIMENSION)));
        // State: [activated]
        sendCmdFeedback(src, buildStateLink(dimRegion));
        return 0;
    }
}
