package de.z0rdak.yawp.commands;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;

import de.z0rdak.yawp.api.events.region.RegionEvent;
import de.z0rdak.yawp.commands.arguments.region.RegionArgumentType;
import de.z0rdak.yawp.config.server.FlagConfig;
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
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.command.CommandSource;
import net.minecraft.command.Commands;
import net.minecraft.command.ISuggestionProvider;
import net.minecraft.command.arguments.BlockPosArgument;
import net.minecraft.command.arguments.DimensionArgument;
import net.minecraft.command.arguments.EntityArgument;
import net.minecraft.command.arguments.TeamArgument;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.scoreboard.Team;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.text.IFormattableTextComponent;
import net.minecraft.util.text.StringTextComponent;
import net.minecraft.util.text.TranslationTextComponent;
import net.minecraft.world.World;
import net.minecraftforge.common.MinecraftForge;
import org.apache.commons.lang3.NotImplementedException;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.*;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.core.region.RegionType.DIMENSION;
import static de.z0rdak.yawp.core.region.RegionType.LOCAL;
import static de.z0rdak.yawp.util.CommandUtil.*;
import static de.z0rdak.yawp.util.MessageUtil.*;

public class DimensionCommands {

    private DimensionCommands() {
    }

    public static final List<String> regionNameSuggestions = Arrays.asList("newRegion", "spawn", "home", "town", "arena");

    public static LiteralArgumentBuilder<CommandSource> build() {
        List<String> affiliationList = Arrays.asList(RegionCommands.MEMBER, RegionCommands.OWNER);
        return literal(DIM)
                /* /wp dimension <dim> list region */
                .then(Commands.argument(DIM.toString(), DimensionArgument.dimension())
                        .then(literal(CREATE)
                                .then(literal(REGION)
                                        .then(Commands.argument(REGION.toString(), StringArgumentType.word())
                                                .suggests((ctx, builder) -> ISuggestionProvider.suggest(Collections.singletonList(regionNameSuggestions.get(new Random().nextInt(regionNameSuggestions.size()))), builder))
                                                //.then(Commands.argument(AREA.toString(), StringArgumentType.word())
                                                //        .suggests((ctx, builder) -> AreaArgumentType.areaType().listSuggestions(ctx, builder))
                                                //        .executes(ctx -> createRegion(ctx.getSource(), getRegionNameArgument(ctx), getDimCacheArgument(ctx), getAreaTypeArgument(ctx))))
                                                .then(Commands.literal(AreaType.CUBOID.areaType)
                                                        .then(Commands.argument("pos1", BlockPosArgument.blockPos())
                                                                .then(Commands.argument("pos2", BlockPosArgument.blockPos())
                                                                        .executes(ctx -> createCuboidRegion(ctx.getSource(), getRegionNameArgument(ctx), getDimCacheArgument(ctx),
                                                                                BlockPosArgument.getOrLoadBlockPos(ctx, "pos1"),
                                                                                BlockPosArgument.getOrLoadBlockPos(ctx, "pos2"), null))
                                                                        .then(Commands.argument(OWNER.toString(), EntityArgument.player())
                                                                                .executes(ctx -> createCuboidRegion(ctx.getSource(), getRegionNameArgument(ctx), getDimCacheArgument(ctx),
                                                                                        BlockPosArgument.getOrLoadBlockPos(ctx, "pos1"),
                                                                                        BlockPosArgument.getOrLoadBlockPos(ctx, "pos2"), getOwnerArgument(ctx))))))
                                                )
                                        )
                                )
                        )
                        /* /wp dimension <dim> [info] */
                        .executes(ctx -> promptDimensionInfo(ctx.getSource(), getDimCacheArgument(ctx)))
                        .then(literal(INFO).executes(ctx -> promptDimensionInfo(ctx.getSource(), getDimCacheArgument(ctx))))
                        /* /wp dimension <dim> activate */
                        .then(literal(ENABLE)
                                .executes(ctx -> setActiveState(ctx, getDimCacheArgument(ctx)))
                                .then(Commands.argument(ENABLE.toString(), BoolArgumentType.bool())
                                        .executes(ctx -> setActiveState(ctx, getDimCacheArgument(ctx), getEnableArgument(ctx)))))
                        // TODO: State (muted
                        .then(literal(COPY)
                                .then(Commands.argument(SRC_DIM.toString(), DimensionArgument.dimension())
                                        .then(Commands.argument(SRC_REGION.toString(), StringArgumentType.word())
                                                        .suggests((ctx, builder) -> RegionArgumentType.region().listSrcRegions(ctx, builder))
                                                        .then(literal(FLAGS)
                                                                .executes(ctx -> copyRegionFlags(ctx, getRegionArgument(ctx), getSourceRegionArgument(ctx))))
                                                        .then(literal(STATE)
                                                                .executes(ctx -> copyRegionState(ctx, getRegionArgument(ctx), getSourceRegionArgument(ctx))))
                                                        .then(literal(PLAYERS)
                                                                .executes(ctx -> copyRegionPlayers(ctx, getRegionArgument(ctx), getSourceRegionArgument(ctx)))
                                                                .then(Commands.argument(CommandConstants.AFFILIATION.toString(), StringArgumentType.word())
                                                                        .suggests((ctx, builder) -> ISuggestionProvider.suggest(affiliationList, builder))
                                                                        .executes(ctx -> copyRegionPlayers(ctx, getRegionArgument(ctx), getSourceRegionArgument(ctx), getAffiliationArgument(ctx))))
                                                        )
                                                // .then(literal(TEAMS)
                                                //         .executes(ctx -> copyRegionTeams(ctx, getRegionArgument(ctx), getSourceRegionArgument(ctx)))
                                                //         .then(Commands.argument(CommandConstants.AFFILIATION.toString(), StringArgumentType.word())
                                                //                 .suggests((ctx, builder) -> ISuggestionProvider.suggest(affiliationList, builder))
                                                //                 .executes(ctx -> copyRegionTeams(ctx, getRegionArgument(ctx), getSourceRegionArgument(ctx), getAffiliationArgument(ctx))))
                                                // )
                                                // .then(literal(AFFILIATION)
                                                //         .then(Commands.argument(CommandConstants.AFFILIATION.toString(), StringArgumentType.word())
                                                //                 .suggests((ctx, builder) -> ISuggestionProvider.suggest(affiliationList, builder))
                                                //                 .executes(ctx -> copyRegionAffiliation(ctx, getRegionArgument(ctx), getSourceRegionArgument(ctx), getAffiliationArgument(ctx))))
                                                // )
                                        )
                                )
                        )
                        .then(literal(LIST)
                                .then(literal(REGION)
                                        .executes(ctx -> promptDimensionRegionList(ctx.getSource(), getDimCacheArgument(ctx), 0))
                                        .then(Commands.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                                .executes(ctx -> promptDimensionRegionList(ctx.getSource(), getDimCacheArgument(ctx), getPageNoArgument(ctx)))))
                                /* /wp dimension <dim> list owner */
                                .then(literal(OWNER)
                                        .executes(ctx -> promptDimensionAffiliates(ctx.getSource(), getDimCacheArgument(ctx), OWNER.toString()))
                                        .then(literal(TEAM)
                                                .executes(ctx -> promptDimensionAffiliationList(ctx.getSource(), getDimCacheArgument(ctx), OWNER.toString(), AffiliationType.TEAM, 0))
                                                .then(Commands.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                                        .executes(ctx -> promptDimensionAffiliationList(ctx.getSource(), getDimCacheArgument(ctx), OWNER.toString(), AffiliationType.TEAM, getPageNoArgument(ctx)))))
                                        .then(literal(PLAYER)
                                                .executes(ctx -> promptDimensionAffiliationList(ctx.getSource(), getDimCacheArgument(ctx), OWNER.toString(), AffiliationType.PLAYER, 0))
                                                .then(Commands.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                                        .executes(ctx -> promptDimensionAffiliationList(ctx.getSource(), getDimCacheArgument(ctx), OWNER.toString(), AffiliationType.PLAYER, getPageNoArgument(ctx))))
                                        )
                                )
                                /* /wp dimension <dim> list member */
                                .then(literal(MEMBER)
                                        .executes(ctx -> promptDimensionAffiliates(ctx.getSource(), getDimCacheArgument(ctx), MEMBER.toString()))
                                        .then(literal(TEAM)
                                                .executes(ctx -> promptDimensionAffiliationList(ctx.getSource(), getDimCacheArgument(ctx), MEMBER.toString(), AffiliationType.TEAM, 0))
                                                .then(Commands.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                                        .executes(ctx -> promptDimensionAffiliationList(ctx.getSource(), getDimCacheArgument(ctx), MEMBER.toString(), AffiliationType.TEAM, getPageNoArgument(ctx)))))
                                        .then(literal(PLAYER)
                                                .executes(ctx -> promptDimensionAffiliationList(ctx.getSource(), getDimCacheArgument(ctx), MEMBER.toString(), AffiliationType.PLAYER, 0))
                                                .then(Commands.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                                        .executes(ctx -> promptDimensionAffiliationList(ctx.getSource(), getDimCacheArgument(ctx), MEMBER.toString(), AffiliationType.PLAYER, getPageNoArgument(ctx))))
                                        )
                                )
                                /* /wp dimension <dim> list flag */
                                .then(literal(FLAG)
                                        .executes(ctx -> promptDimensionFlagList(ctx.getSource(), getDimCacheArgument(ctx), 0))
                                        .then(Commands.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                                .executes(ctx -> promptDimensionFlagList(ctx.getSource(), getDimCacheArgument(ctx), getPageNoArgument(ctx))))))
                        .then(literal(DELETE)
                                .then(Commands.argument(REGION.toString(), StringArgumentType.word())
                                        .suggests((ctx, builder) -> RegionArgumentType.region().listSuggestions(ctx, builder))
                                        .executes(ctx -> attemptDeleteRegion(ctx.getSource(), getDimCacheArgument(ctx), getRegionArgument(ctx)))
                                        .then(Commands.literal("-y")
                                                .executes(ctx -> deleteRegion(ctx.getSource(), getDimCacheArgument(ctx), getRegionArgument(ctx))))))
                        .then(literal(REMOVE)
                                .then(literal(PLAYER)
                                        .then(Commands.argument(AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> ISuggestionProvider.suggest(affiliationList, builder))
                                                .then(Commands.argument(PLAYER.toString(), EntityArgument.player())
                                                        .executes(ctx -> removePlayer(ctx, getPlayerArgument(ctx), getDimCacheArgument(ctx), getAffiliationArgument(ctx)))))

                                        .then(Commands.argument(AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> ISuggestionProvider.suggest(affiliationList, builder))
                                                .then(Commands.argument(PLAYER.toString(), EntityArgument.player())
                                                        .executes(ctx -> removePlayer(ctx, getPlayerArgument(ctx), getDimCacheArgument(ctx), getAffiliationArgument(ctx))))))
                                .then(literal(TEAM)
                                        .then(Commands.argument(AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> ISuggestionProvider.suggest(affiliationList, builder))
                                                .then(Commands.argument(TEAM.toString(), TeamArgument.team())
                                                        .executes(ctx -> removeTeam(ctx, getTeamArgument(ctx), getDimCacheArgument(ctx), getAffiliationArgument(ctx)))))

                                        .then(Commands.argument(AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> ISuggestionProvider.suggest(affiliationList, builder))
                                                .then(Commands.argument(TEAM.toString(), TeamArgument.team())
                                                        .executes(ctx -> removeTeam(ctx, getTeamArgument(ctx), getDimCacheArgument(ctx), getAffiliationArgument(ctx))))))
                                .then(literal(FLAG)
                                        .then(Commands.argument(FLAG.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> ISuggestionProvider.suggest(RegionDataManager.get().getFlagsIdsForDim(getDimCacheArgument(ctx)), builder))
                                                .executes(ctx -> removeFlag(ctx, getDimCacheArgument(ctx), getFlagArgument(ctx))))))
                        .then(literal(ADD)
                                .then(literal(PLAYER)
                                        .then(Commands.argument(AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> ISuggestionProvider.suggest(affiliationList, builder))
                                                .then(Commands.argument(PLAYER.toString(), EntityArgument.player())
                                                        .executes(ctx -> addPlayer(ctx, getPlayerArgument(ctx), getDimCacheArgument(ctx), getAffiliationArgument(ctx)))))

                                        .then(Commands.argument(AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> ISuggestionProvider.suggest(affiliationList, builder))
                                                .then(Commands.argument(PLAYER.toString(), EntityArgument.player())
                                                        .executes(ctx -> addPlayer(ctx, getPlayerArgument(ctx), getDimCacheArgument(ctx), getAffiliationArgument(ctx))))))
                                .then(literal(TEAM)
                                        .then(Commands.argument(AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> ISuggestionProvider.suggest(affiliationList, builder))
                                                .then(Commands.argument(TEAM.toString(), TeamArgument.team())
                                                        .executes(ctx -> addTeam(ctx, getTeamArgument(ctx), getDimCacheArgument(ctx), getAffiliationArgument(ctx)))))

                                        .then(Commands.argument(AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> ISuggestionProvider.suggest(affiliationList, builder))
                                                .then(Commands.argument(TEAM.toString(), TeamArgument.team())
                                                        .executes(ctx -> addTeam(ctx, getTeamArgument(ctx), getDimCacheArgument(ctx), getAffiliationArgument(ctx))))))
                                .then(literal(FLAG)
                                        .then(Commands.argument(FLAG.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> ISuggestionProvider.suggest(RegionFlag.getFlagNames(), builder))
                                                .executes(ctx -> addFlag(ctx, getDimCacheArgument(ctx), getFlagArgument(ctx)))))));
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

    private static int createCuboidRegion(CommandSource src, String regionName, DimensionRegionCache dimCache, BlockPos pos1, BlockPos pos2, @Nullable ServerPlayerEntity owner) {
        int res = checkValidRegionName(regionName, dimCache);
        if (res == -1) {
            sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info.region.create.name.invalid", regionName));
            return res;
        }
        if (res == 1) {
            sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info.region.create.name.exists", dimCache.getDimensionalRegion().getName(), buildRegionInfoLink(dimCache.getRegion(regionName), LOCAL)));
            return res;
        }
        CuboidRegion region = new CuboidRegion(regionName, new CuboidArea(pos1, pos2), owner, dimCache.dimensionKey());

        ServerPlayerEntity player;
        try {
            player = src.getPlayerOrException();
        } catch (CommandSyntaxException e) {
            player = null;
        }

        if(MinecraftForge.EVENT_BUS.post(new RegionEvent.CreateRegionEvent(region, player))) {
            return 0;
        }

        RegionDataManager.addFlags(RegionConfig.getDefaultFlags(), region);
        dimCache.addRegion(region);
        LocalRegions.ensureHigherRegionPriorityFor(region, RegionConfig.DEFAULT_REGION_PRIORITY.get());
        RegionDataManager.save();
        sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info.region.create.success", buildRegionInfoLink(region, LOCAL)));
        return 0;
    }

    private static int createSphereRegion(CommandSource src, @Nonnull String regionName, DimensionRegionCache dimCache, BlockPos center, BlockPos outerPos, ServerPlayerEntity owner) {
        if (!regionName.matches(RegionArgumentType.VALID_NAME_PATTERN.pattern())) {
            sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info.region.create.name.invalid", regionName));
            return -1;
        }
        if (dimCache.contains(regionName)) {
            sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info.region.create.name.exists", dimCache.dimensionKey(), regionName));
            return 1;
        }
        SphereArea area = new SphereArea(center, outerPos);
        SphereRegion region = new SphereRegion(regionName, area, owner, dimCache.dimensionKey());
        RegionDataManager.addFlags(RegionConfig.getDefaultFlags(), region);
        dimCache.addRegion(region);
        RegionDataManager.save();
        sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info.region.create.success", buildRegionInfoLink(region, LOCAL)));
        return 0;
    }

    public static int copyRegionFlags(CommandContext<CommandSource> ctx, IProtectedRegion region, IProtectedRegion srcRegion) {
        srcRegion.getFlags().forEach(region::addFlag);
        RegionDataManager.save();
        RegionType srcRegiontype = srcRegion instanceof DimensionalRegion ? DIMENSION : LOCAL;
        RegionType targetRegiontype = region instanceof DimensionalRegion ? DIMENSION : LOCAL;
        sendCmdFeedback(ctx.getSource(), new TranslationTextComponent("Copied flags from region %s to %s", buildRegionInfoLink(srcRegion, srcRegiontype), buildRegionInfoLink(region, targetRegiontype)));
        return 0;
    }

    public static int copyRegionState(CommandContext<CommandSource> ctx, IProtectedRegion region, IProtectedRegion srcRegion) {
        region.setIsActive(srcRegion.isActive());
        region.setIsMuted(srcRegion.isMuted());
        if (region instanceof IMarkableRegion && srcRegion instanceof IMarkableRegion) {
            IMarkableRegion regionTarget = (IMarkableRegion) region;
            IMarkableRegion regionSource = (IMarkableRegion) srcRegion;
            regionTarget.setPriority(regionSource.getPriority());
        }
        RegionDataManager.save();
        RegionType srcRegiontype = srcRegion instanceof DimensionalRegion ? DIMENSION : LOCAL;
        RegionType targetRegiontype = region instanceof DimensionalRegion ? DIMENSION : LOCAL;
        sendCmdFeedback(ctx.getSource(), new TranslationTextComponent("Copied state from region %s to %s", buildRegionInfoLink(srcRegion, srcRegiontype), buildRegionInfoLink(region, targetRegiontype)));
        return 0;
    }

    public static int copyRegionPlayers(CommandContext<CommandSource> ctx, IProtectedRegion region, IProtectedRegion srcRegion, String affiliation) {
        switch (affiliation) {
            case "member":
                srcRegion.getMembers().getPlayers().forEach((uuid, name) -> region.getMembers().addPlayer(uuid, name));
                break;
            case "owner":
                srcRegion.getOwners().getPlayers().forEach((uuid, name) -> region.getOwners().addPlayer(uuid, name));
                break;
            default:
                throw new IllegalArgumentException("Unexpected value = " + affiliation);
        }
        RegionDataManager.save();
        RegionType srcRegiontype = srcRegion instanceof DimensionalRegion ? DIMENSION : LOCAL;
        RegionType targetRegiontype = region instanceof DimensionalRegion ? DIMENSION : LOCAL;
        sendCmdFeedback(ctx.getSource(), new TranslationTextComponent("Copied players with affiliation '%s' from %s to %s", affiliation, buildRegionInfoLink(srcRegion, srcRegiontype), buildRegionInfoLink(region, targetRegiontype)));
        return 0;
    }

    public static int copyRegionPlayers(CommandContext<CommandSource> ctx, IProtectedRegion region, IProtectedRegion srcRegion) {
        return copyRegionPlayers(ctx, region, srcRegion, MEMBER.toString()) + copyRegionPlayers(ctx, region, srcRegion, OWNER.toString());
    }

    // FIXME: Teams can only be copied from the same dimension!
    public static int copyRegionTeams(CommandContext<CommandSource> ctx, IProtectedRegion region, IProtectedRegion srcRegion, String affiliation) {
        switch (affiliation) {
            case "member":
                srcRegion.getMembers().getTeams().forEach(team -> region.getMembers().addTeam(team));
                break;
            case "owner":
                srcRegion.getOwners().getTeams().forEach(team -> region.getOwners().addTeam(team));
                break;
            default:
                throw new IllegalArgumentException("Unexpected value = " + affiliation);
        }
        RegionDataManager.save();
        RegionType srcRegiontype = srcRegion instanceof DimensionalRegion ? DIMENSION : LOCAL;
        RegionType targetRegiontype = region instanceof DimensionalRegion ? DIMENSION : LOCAL;
        sendCmdFeedback(ctx.getSource(), new TranslationTextComponent("Copied teams with affiliation '%s' from %s to %s", affiliation, buildRegionInfoLink(srcRegion, srcRegiontype), buildRegionInfoLink(region, targetRegiontype)));
        return 0;
    }

    public static int copyRegionTeams(CommandContext<CommandSource> ctx, IProtectedRegion region, IProtectedRegion srcRegion) {
        return copyRegionTeams(ctx, region, srcRegion, MEMBER.toString()) + copyRegionTeams(ctx, region, srcRegion, OWNER.toString());
    }

    // FIXME: Teams can only be copied from the same dimension - maybe replace teams with some permission group from another mod
    public static int copyRegionAffiliation(CommandContext<CommandSource> ctx, IProtectedRegion region, IProtectedRegion srcRegion, String affiliation) {
        RegionDataManager.save();
        return 0;
    }

    private static int attemptDeleteRegion(CommandSource src, DimensionRegionCache dim, IMarkableRegion region) {
        if (dim.contains(region.getName())) {
            sendCmdFeedback(src, new TranslationTextComponent("cli.msg.info.dim.region.remove.attempt", buildRegionInfoLink(region, LOCAL), buildRegionInfoLink(dim.getDimensionalRegion(), RegionType.DIMENSION)));
            return 0;
        }
        return 1;
    }

    // FIXME: Are child / parent relation properly removed when deleting a region?
    private static int deleteRegion(CommandSource src, DimensionRegionCache dim, IMarkableRegion region) {
        ServerPlayerEntity player;
        try {
            player = src.getPlayerOrException();
        } catch (CommandSyntaxException e) {
            player = null;
        }

        if(MinecraftForge.EVENT_BUS.post(new RegionEvent.RemoveRegionEvent(region, player))) {
            return 0;
        }

        if (dim.contains(region.getName())) {
            if (!region.getChildren().isEmpty()) {
                // TODO: config option which allows deleting region with children? children then default to dim parent
                sendCmdFeedback(src, new TranslationTextComponent("cli.msg.info.dim.region.remove.fail.hasChildren", buildRegionInfoLink(region, LOCAL)));
                return -1;
            }
            if (region.getParent() != null) {
                region.getParent().removeChild(region);
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(region.getDim());
                dimCache.getDimensionalRegion().addChild(region);
            }
            dim.removeRegion(region);
            RegionDataManager.save();
            sendCmdFeedback(src, new TranslationTextComponent("cli.msg.info.dim.region.remove.confirm", region.getName(), buildRegionInfoLink(dim.getDimensionalRegion(), RegionType.DIMENSION)));
            return 0;
        }
        return 1;
    }

    private static int removeFlag(CommandContext<CommandSource> src, DimensionRegionCache dimCache, RegionFlag flag) {
        if (dimCache.getDimensionalRegion().containsFlag(flag)) {
            dimCache.getDimensionalRegion().removeFlag(flag.name);
            RegionDataManager.save();
            sendCmdFeedback(src.getSource(), new TranslationTextComponent("cli.msg.flags.removed", flag.name,
                    buildRegionInfoLink(dimCache.getDimensionalRegion(), RegionType.DIMENSION))
                    .append(" ")
                    .append(buildRegionActionUndoLink(src.getInput(), REMOVE, ADD)));
            return 0;
        }
        return 1;
    }

    private static int addFlag(CommandContext<CommandSource> src, DimensionRegionCache dimCache, RegionFlag flag) {
        if (!dimCache.getDimensionalRegion().containsFlag(flag)) {
            IFlag iflag = null;
            switch (flag.type) {
                case BOOLEAN_FLAG:
                    iflag = new BooleanFlag(flag.name, false);
                    break;
                case LIST_FLAG:
                case INT_FLAG:
                    return 1;
                default:
                    return 1;
            }
            if (flag.name.contains("spawning")) {
                RegionCommands.removeInvolvedEntities(src, dimCache.getDimensionalRegion(), flag);
            }
            dimCache.getDimensionalRegion().addFlag(iflag);
            RegionDataManager.save();
            IFormattableTextComponent flagLink = MessageUtil.buildFlagCmdInfoLink(dimCache.getDimensionalRegion(), RegionType.DIMENSION, iflag);
            sendCmdFeedback(src.getSource(), new TranslationTextComponent("cli.msg.flags.added", buildFlagQuickInfo(iflag),
                    buildRegionInfoLink(dimCache.getDimensionalRegion(), RegionType.DIMENSION))
                    .append(" ")
                    .append(buildRegionActionUndoLink(src.getInput(), ADD, REMOVE)));
            return 0;
        }
        return 1;
    }

    private static int removePlayer(CommandContext<CommandSource> src, String playerName, DimensionRegionCache dimCache, String affiliationType) {

        return 1;
    }

    private static int removePlayer(CommandContext<CommandSource> src, ServerPlayerEntity player, DimensionRegionCache dimCache, String affiliationType) {
        if (dimCache != null) {
            IFormattableTextComponent undoLink = buildRegionActionUndoLink(src.getInput(), REMOVE, ADD);
            if (affiliationType.equals(MEMBER.toString())) {
                if (dimCache.getDimensionalRegion().hasMember(player.getUUID())) {
                    dimCache.getDimensionalRegion().removeMember(player);
                    IFormattableTextComponent playerInfo = buildAffiliateInfo(dimCache.getDimensionalRegion(), player.getScoreboardName(), AffiliationType.PLAYER);
                    sendCmdFeedback(src.getSource(), new TranslationTextComponent("cli.msg.dim.info.player.removed", affiliationType, playerInfo,
                            buildRegionInfoLink(dimCache.getDimensionalRegion(), RegionType.DIMENSION))
                            .append(" ")
                            .append(undoLink));
                    RegionDataManager.save();
                    return 0;
                }
            }
            if (affiliationType.equals(OWNER.toString())) {
                if (dimCache.getDimensionalRegion().hasOwner(player.getUUID())) {
                    dimCache.getDimensionalRegion().removeOwner(player);
                    IFormattableTextComponent playerInfo = buildAffiliateInfo(dimCache.getDimensionalRegion(), player.getScoreboardName(), AffiliationType.PLAYER);
                    sendCmdFeedback(src.getSource(), new TranslationTextComponent("cli.msg.dim.info.player.removed", affiliationType, playerInfo,
                            buildRegionInfoLink(dimCache.getDimensionalRegion(), RegionType.DIMENSION))
                            .append(" ")
                            .append(undoLink));
                    RegionDataManager.save();
                    return 0;
                }
            }
        }
        return 1;
    }

    private static int removeTeam(CommandContext<CommandSource> src, Team team, DimensionRegionCache dimCache, String affiliationType) {
        if (dimCache != null) {
            IFormattableTextComponent undoLink = buildRegionActionUndoLink(src.getInput(), REMOVE, ADD);
            if (affiliationType.equals(MEMBER.toString())) {
                if (dimCache.getDimensionalRegion().hasMember(team.getName())) {
                    dimCache.getDimensionalRegion().removeMember(team);
                    sendCmdFeedback(src.getSource(), new TranslationTextComponent("cli.msg.dim.info.player.removed", affiliationType, buildTeamHoverComponent(team),
                            buildRegionInfoLink(dimCache.getDimensionalRegion(), RegionType.DIMENSION))
                            .append(" ")
                            .append(undoLink));
                    RegionDataManager.save();
                    return 0;
                }
            }
            if (affiliationType.equals(OWNER.toString())) {
                if (dimCache.getDimensionalRegion().hasOwner(team.getName())) {
                    dimCache.getDimensionalRegion().removeOwner(team);
                    sendCmdFeedback(src.getSource(), new TranslationTextComponent("cli.msg.dim.info.player.removed",
                            affiliationType, buildTeamHoverComponent(team), buildRegionInfoLink(dimCache.getDimensionalRegion(),
                            RegionType.DIMENSION))
                            .append(" ")
                            .append(undoLink));
                    RegionDataManager.save();
                    return 0;
                }
            }
        }
        return 1;
    }

    // TODO: If works replace with switch and catch error
    private static int addPlayer(CommandContext<CommandSource> src, ServerPlayerEntity player, DimensionRegionCache dimCache, String affiliationType) {
        if (dimCache != null) {
            RegistryKey<World> dim = dimCache.dimensionKey();
            if (affiliationType.equals(MEMBER.toString())) {
                dimCache.getDimensionalRegion().addMember(player);
            }
            if (affiliationType.equals(OWNER.toString())) {
                dimCache.getDimensionalRegion().addOwner(player);
            }
            RegionDataManager.save();
            sendCmdFeedback(src.getSource(), new TranslationTextComponent("cli.msg.dim.info.player.added",
                    buildPlayerHoverComponent(player), dim.location().toString(), affiliationType)
                    .append(" ")
                    .append(buildRegionActionUndoLink(src.getInput(), ADD, REMOVE)));
            return 0;
        }
        return 1;
    }

    private static int addTeam(CommandContext<CommandSource> src, Team team, DimensionRegionCache dimCache, String affiliationType) {
        if (dimCache != null) {
            RegistryKey<World> dim = dimCache.dimensionKey();
            if (affiliationType.equals(MEMBER.toString())) {
                dimCache.getDimensionalRegion().addMember(team);
            }
            if (affiliationType.equals(OWNER.toString())) {
                dimCache.getDimensionalRegion().addOwner(team);
            }
            RegionDataManager.save();
            sendCmdFeedback(src.getSource(), new TranslationTextComponent("cli.msg.dim.info.team.added",
                    buildTeamHoverComponent(team), dim.location().toString(), affiliationType)
                    .append(" ")
                    .append(buildRegionActionUndoLink(src.getInput(), ADD, REMOVE)));
            return 0;
        }
        return 1;
    }

    private static int setActiveState(CommandContext<CommandSource> src, DimensionRegionCache dimCache) {
        return setActiveState(src, dimCache, !dimCache.getDimensionalRegion().isActive());
    }

    private static int setActiveState(CommandContext<CommandSource> src, DimensionRegionCache dimCache, boolean activate) {
        if (dimCache != null) {
            CommandConstants toReplace = activate ? FALSE : TRUE;
            CommandConstants replacement = activate ? TRUE : FALSE;
            boolean oldState = dimCache.getDimensionalRegion().isActive();
            dimCache.getDimensionalRegion().setIsActive(activate);
            RegionDataManager.save();
            IFormattableTextComponent undoLink = buildRegionActionUndoLink(src.getInput(), toReplace, replacement);
            String langKey = "cli.msg.info.state." + (activate ? "activated" : "deactivated");
            sendCmdFeedback(src.getSource(), new TranslationTextComponent(langKey, buildRegionInfoLink(dimCache.getDimensionalRegion(), RegionType.DIMENSION))
                    .append(" ")
                    .append(undoLink));
            return 0;
        }
        return 1;
    }

    private static int promptDimensionRegionList(CommandSource src, DimensionRegionCache dimCache, int pageNo) {
        if (dimCache != null) {
            DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
            List<IMarkableRegion> regionsForDim = dimCache.getRegionsInDimension()
                    .values()
                    .stream()
                    .sorted(Comparator.comparing(IMarkableRegion::getName))
                    .collect(Collectors.toList());
            if (regionsForDim.isEmpty()) {
                sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info.regions.empty", buildRegionInfoLink(dimCache.getDimensionalRegion(), RegionType.DIMENSION)));
                return -1;
            }
            List<IFormattableTextComponent> regionPagination = buildPaginationComponents(
                    buildDimRegionListHeader(dimRegion),
                    buildCommandStr(DIM.toString(), dimRegion.getName(), LIST.toString(), REGION.toString()),
                    buildRemoveRegionEntries(dimRegion, regionsForDim, RegionType.DIMENSION),
                    pageNo,
                    new StringTextComponent(" - ").append(buildDimCreateRegionLink(dimRegion)));
            regionPagination.forEach(line -> sendCmdFeedback(src, line));
            return 0;
        }
        return 1;
    }

    private static int promptDimensionFlagList(CommandSource src, DimensionRegionCache dimCache, int pageNo) {
        List<IFlag> flags = LocalRegions.getSortedFlags(dimCache.getDimensionalRegion());
        if (flags.isEmpty()) {
            sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info.flags.empty", buildRegionInfoLink(dimCache.getDimensionalRegion(), RegionType.DIMENSION)));
            return 1;
        }
        List<IFormattableTextComponent> flagPagination = buildPaginationComponents(
                buildFlagHeader(dimCache.getDimensionalRegion(), RegionType.DIMENSION),
                buildCommandStr(DIM.toString(), dimCache.getDimensionalRegion().getName(), LIST.toString(), FLAG.toString()),
                buildRemoveFlagEntries(dimCache.getDimensionalRegion(), flags, RegionType.DIMENSION),
                pageNo,
                new StringTextComponent(" - ").append(buildDimAddFlagLink(dimCache.getDimensionalRegion())));
        flagPagination.forEach(line -> sendCmdFeedback(src, line));
        return 0;
    }

    private static int promptDimensionAffiliates(CommandSource src, DimensionRegionCache dimCache, String affiliation) {
        DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
        sendCmdFeedback(src, buildAffiliationHeader(dimRegion, affiliation, RegionType.DIMENSION));
        sendCmdFeedback(src, buildAffiliationPlayerListLink(dimRegion, affiliation, RegionType.DIMENSION));
        sendCmdFeedback(src, buildAffiliationTeamListLink(dimRegion, affiliation, RegionType.DIMENSION));
        return 0;
    }

    private static int promptDimensionAffiliationList(CommandSource src, DimensionRegionCache dimCache, String affiliation, AffiliationType affiliationType, int pageNo) {
        DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
        List<String> affiliateNames = getAffiliateList(dimRegion, affiliation, affiliationType);
        if (affiliateNames.isEmpty()) {
            sendCmdFeedback(src, new TranslationTextComponent("cli.msg.info.region.affiliation." + affiliationType.name + ".empty", affiliation, buildRegionInfoLink(dimCache.getDimensionalRegion(), RegionType.DIMENSION)));
            return 1;
        }
        List<IFormattableTextComponent> affiliatePagination = buildPaginationComponents(
                buildAffiliationHeader(dimRegion, affiliation, affiliationType, RegionType.DIMENSION),
                buildCommandStr(DIM.toString(), dimRegion.getDim().location().toString(), LIST.toString(), affiliation, affiliationType.name),
                buildRemoveAffiliationEntries(dimRegion, affiliateNames, affiliationType, affiliation, RegionType.DIMENSION),
                pageNo,
                new StringTextComponent(" - ").append(buildAddAffiliateLink(dimRegion, affiliation, affiliationType, RegionType.DIMENSION)));
        affiliatePagination.forEach(line -> sendCmdFeedback(src, line));
        return 0;
    }

    private static int promptDimensionInfo(CommandSource src, DimensionRegionCache dimCache) {
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
