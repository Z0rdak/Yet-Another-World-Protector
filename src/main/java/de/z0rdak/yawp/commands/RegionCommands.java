package de.z0rdak.yawp.commands;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.commands.arguments.flag.RegionFlagArgumentType;
import de.z0rdak.yawp.commands.arguments.region.AddRegionChildArgumentType;
import de.z0rdak.yawp.commands.arguments.region.RegionArgumentType;
import de.z0rdak.yawp.commands.arguments.region.RemoveRegionChildArgumentType;
import de.z0rdak.yawp.config.server.RegionConfig;
import de.z0rdak.yawp.core.affiliation.AffiliationType;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.flag.BooleanFlag;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.*;
import de.z0rdak.yawp.core.stick.AbstractStick;
import de.z0rdak.yawp.core.stick.MarkerStick;
import de.z0rdak.yawp.handler.flags.HandlerUtil;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.LocalRegions;
import de.z0rdak.yawp.util.StickException;
import de.z0rdak.yawp.util.StickType;
import de.z0rdak.yawp.util.StickUtil;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.SharedSuggestionProvider;
import net.minecraft.commands.arguments.DimensionArgument;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.commands.arguments.TeamArgument;
import net.minecraft.commands.arguments.coordinates.BlockPosArgument;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.ExperienceOrb;
import net.minecraft.world.entity.animal.AbstractGolem;
import net.minecraft.world.entity.monster.Slime;
import net.minecraft.world.entity.npc.WanderingTrader;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.entity.EntityTypeTest;
import net.minecraft.world.scores.PlayerTeam;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.commands.DimensionCommands.checkValidRegionName;
import static de.z0rdak.yawp.core.region.RegionType.LOCAL;
import static de.z0rdak.yawp.util.CommandUtil.*;
import static de.z0rdak.yawp.util.MessageUtil.*;
import static net.minecraft.ChatFormatting.RESET;

public class RegionCommands {

    private RegionCommands() {
    }

    public final static String MEMBER = "member";
    public final static String OWNER = "owner";

    public static LiteralArgumentBuilder<CommandSourceStack> build() {
        List<String> affiliationList = Arrays.asList(MEMBER, OWNER);

        return literal(REGION)
                .then(Commands.argument(DIM.toString(), DimensionArgument.dimension())
                        .then(Commands.argument(REGION.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> RegionArgumentType.region().listSuggestions(ctx, builder))
                                .executes(ctx -> promptRegionInfo(ctx.getSource(), getRegionArgument(ctx)))
                                .then(literal(INFO)
                                        .executes(ctx -> promptRegionInfo(ctx.getSource(), getRegionArgument(ctx))))
                                .then(literal(SPATIAL)
                                        .executes(ctx -> promptRegionSpatialProperties(ctx.getSource(), getRegionArgument(ctx))))
                                .then(literal(STATE)
                                        .executes(ctx -> promptRegionState(ctx.getSource(), getRegionArgument(ctx)))
                                        .then(literal(ALERT)
                                                .executes(ctx -> setAlertState(ctx, getRegionArgument(ctx)))
                                                .then(Commands.argument(ALERT.toString(), BoolArgumentType.bool())
                                                        .executes(ctx -> setAlertState(ctx, getRegionArgument(ctx), getAlertArgument(ctx)))))
                                        .then(literal(ENABLE)
                                                .executes(ctx -> setEnableState(ctx, getRegionArgument(ctx)))
                                                .then(Commands.argument(ENABLE.toString(), BoolArgumentType.bool())
                                                        .executes(ctx -> setEnableState(ctx, getRegionArgument(ctx), getEnableArgument(ctx)))))
                                        .then(literal(PRIORITY)
                                                .then(Commands.argument(PRIORITY.toString(), IntegerArgumentType.integer())
                                                        .executes(ctx -> setPriority(ctx, getRegionArgument(ctx), getPriorityArgument(ctx))))
                                                .then(literal(INC)
                                                        .then(Commands.argument(PRIORITY.toString(), IntegerArgumentType.integer())
                                                                .executes(ctx -> setPriority(ctx, getRegionArgument(ctx), getPriorityArgument(ctx), 1))))
                                                .then(literal(DEC)
                                                        .then(Commands.argument(PRIORITY.toString(), IntegerArgumentType.integer())
                                                                .executes(ctx -> setPriority(ctx, getRegionArgument(ctx), getPriorityArgument(ctx), -1))))))
                                .then(literal(LIST)
                                        .then(literal(FLAG)
                                                .executes(ctx -> promptRegionFlags(ctx.getSource(), getRegionArgument(ctx), 0))
                                                .then(Commands.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                                        .executes(ctx -> promptRegionFlags(ctx.getSource(), getRegionArgument(ctx), getPageNoArgument(ctx)))))
                                        .then(literal(CommandConstants.OWNER)
                                                .executes(ctx -> promptRegionAffiliates(ctx.getSource(), getRegionArgument(ctx), OWNER))
                                                .then(literal(TEAM)
                                                        .executes(ctx -> promptRegionAffiliationList(ctx.getSource(), getRegionArgument(ctx), OWNER, AffiliationType.TEAM, 0))
                                                        .then(Commands.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                                                .executes(ctx -> promptRegionAffiliationList(ctx.getSource(), getRegionArgument(ctx), OWNER, AffiliationType.TEAM, getPageNoArgument(ctx)))))
                                                .then(literal(PLAYER)
                                                        .executes(ctx -> promptRegionAffiliationList(ctx.getSource(), getRegionArgument(ctx), OWNER, AffiliationType.PLAYER, 0))
                                                        .then(Commands.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                                                .executes(ctx -> promptRegionAffiliationList(ctx.getSource(), getRegionArgument(ctx), OWNER, AffiliationType.PLAYER, getPageNoArgument(ctx))))
                                                ))
                                        .then(literal(CommandConstants.MEMBER)
                                                .executes(ctx -> promptRegionAffiliates(ctx.getSource(), getRegionArgument(ctx), MEMBER))
                                                .then(literal(TEAM)
                                                        .executes(ctx -> promptRegionAffiliationList(ctx.getSource(), getRegionArgument(ctx), MEMBER, AffiliationType.TEAM, 0))
                                                        .then(Commands.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                                                .executes(ctx -> promptRegionAffiliationList(ctx.getSource(), getRegionArgument(ctx), MEMBER, AffiliationType.TEAM, getPageNoArgument(ctx)))))
                                                .then(literal(PLAYER)
                                                        .executes(ctx -> promptRegionAffiliationList(ctx.getSource(), getRegionArgument(ctx), MEMBER, AffiliationType.PLAYER, 0))
                                                        .then(Commands.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                                                .executes(ctx -> promptRegionAffiliationList(ctx.getSource(), getRegionArgument(ctx), MEMBER, AffiliationType.PLAYER, getPageNoArgument(ctx))))
                                                ))
                                        .then(literal(CHILDREN)
                                                .executes(ctx -> promptRegionChildren(ctx.getSource(), getRegionArgument(ctx), 0))
                                                .then(Commands.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                                        .executes(ctx -> promptRegionChildren(ctx.getSource(), getRegionArgument(ctx), getPageNoArgument(ctx))))
                                        ))
                                .then(literal(AREA)
                                        .then(Commands.literal(AreaType.CUBOID.areaType)
                                                .then(Commands.argument("pos1", BlockPosArgument.blockPos())
                                                        .then(Commands.argument("pos2", BlockPosArgument.blockPos())
                                                                .executes(ctx -> updateArea(ctx, getRegionArgument(ctx), AreaType.CUBOID,
                                                                        BlockPosArgument.getSpawnablePos(ctx, "pos1"),
                                                                        BlockPosArgument.getSpawnablePos(ctx, "pos2")))))
                                        ))
                                // .then(literal(NAME)
                                //         .then(Commands.argument(REGION.toString(), StringArgumentType.word())
                                //                 .executes(ctx -> renameRegion(ctx, getRegionArgument(ctx), getRegionNameArgument(ctx), getDimCacheArgument(ctx)))
                                //         )
                                // )
                                // TODO: Only with marker
                                //.then(literal(UPDATE)
                                //        .then(Commands.argument(AREA.toString(), StringArgumentType.word())
                                //                .suggests((ctx, builder) -> AreaArgumentType.areaType().listSuggestions(ctx, builder))
                                //                .executes(ctx -> updateRegion(ctx.getSource(), getRegionArgument(ctx)))))
                                .then(literal(ADD)
                                        .then(literal(CommandConstants.PLAYER)
                                                .then(Commands.argument(CommandConstants.AFFILIATION.toString(), StringArgumentType.word())
                                                        .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(affiliationList, builder))
                                                        .then(Commands.argument(CommandConstants.PLAYER.toString(), EntityArgument.player())
                                                                .executes(ctx -> addPlayer(ctx, getPlayerArgument(ctx), getRegionArgument(ctx), getAffiliationArgument(ctx)))))
                                                .then(Commands.argument(CommandConstants.AFFILIATION.toString(), StringArgumentType.word())
                                                        .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(affiliationList, builder))
                                                        .then(Commands.argument(CommandConstants.PLAYER.toString(), EntityArgument.player())
                                                                .executes(ctx -> addPlayer(ctx, getPlayerArgument(ctx), getRegionArgument(ctx), getAffiliationArgument(ctx))))))
                                        .then(literal(CommandConstants.TEAM)
                                                .then(Commands.argument(CommandConstants.AFFILIATION.toString(), StringArgumentType.word())
                                                        .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(affiliationList, builder))
                                                        .then(Commands.argument(CommandConstants.TEAM.toString(), TeamArgument.team())
                                                                .executes(ctx -> addTeam(ctx, getTeamArgument(ctx), getRegionArgument(ctx), getAffiliationArgument(ctx)))))
                                                .then(Commands.argument(CommandConstants.AFFILIATION.toString(), StringArgumentType.word())
                                                        .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(affiliationList, builder))
                                                        .then(Commands.argument(CommandConstants.TEAM.toString(), TeamArgument.team())
                                                                .executes(ctx -> addTeam(ctx, getTeamArgument(ctx), getRegionArgument(ctx), getAffiliationArgument(ctx))))))
                                        .then(literal(FLAG)
                                                .then(Commands.argument(FLAG.toString(), StringArgumentType.word())
                                                        .suggests((ctx, builder) -> RegionFlagArgumentType.flag().listSuggestions(ctx, builder))
                                                        .executes(ctx -> addFlag(ctx, getRegionArgument(ctx), getFlagArgument(ctx)))))
                                        .then(literal(CHILD)
                                                .then(Commands.argument(CHILD.toString(), StringArgumentType.word())
                                                        .suggests((ctx, builder) -> AddRegionChildArgumentType.potentialChildRegions().listSuggestions(ctx, builder))
                                                        .executes(ctx -> addChildren(ctx, getRegionArgument(ctx), getChildRegionArgument(ctx))))))
                                .then(literal(REMOVE)
                                        .then(literal(CommandConstants.PLAYER)
                                                .then(Commands.argument(CommandConstants.AFFILIATION.toString(), StringArgumentType.word())
                                                        .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(affiliationList, builder))
                                                        .then(Commands.argument(CommandConstants.PLAYER.toString(), EntityArgument.player())
                                                                .executes(ctx -> removePlayer(ctx, getPlayerArgument(ctx), getRegionArgument(ctx), getAffiliationArgument(ctx)))))
                                                .then(Commands.argument(CommandConstants.AFFILIATION.toString(), StringArgumentType.word())
                                                        .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(affiliationList, builder))
                                                        .then(Commands.argument(CommandConstants.PLAYER.toString(), EntityArgument.player())
                                                                .executes(ctx -> removePlayer(ctx, getPlayerArgument(ctx), getRegionArgument(ctx), getAffiliationArgument(ctx))))))
                                        .then(literal(CommandConstants.TEAM)
                                                .then(Commands.argument(CommandConstants.AFFILIATION.toString(), StringArgumentType.word())
                                                        .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(affiliationList, builder))
                                                        .then(Commands.argument(CommandConstants.TEAM.toString(), TeamArgument.team())
                                                                .executes(ctx -> removeTeam(ctx, getTeamArgument(ctx), getRegionArgument(ctx), getAffiliationArgument(ctx)))))
                                                .then(Commands.argument(CommandConstants.AFFILIATION.toString(), StringArgumentType.word())
                                                        .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(affiliationList, builder))
                                                        .then(Commands.argument(CommandConstants.TEAM.toString(), TeamArgument.team())
                                                                .executes(ctx -> removeTeam(ctx, getTeamArgument(ctx), getRegionArgument(ctx), getAffiliationArgument(ctx))))))
                                        .then(literal(FLAG)
                                                .then(Commands.argument(FLAG.toString(), StringArgumentType.word())
                                                        .suggests((ctx, builder) -> RegionFlagArgumentType.flag().listSuggestions(ctx, builder))
                                                        .executes(ctx -> removeFlag(ctx, getRegionArgument(ctx), getFlagArgument(ctx)))))
                                        .then(literal(CHILD)
                                                .then(Commands.argument(CHILD.toString(), StringArgumentType.word())
                                                        .suggests((ctx, builder) -> RemoveRegionChildArgumentType.childRegions().listSuggestions(ctx, builder))
                                                        .executes(ctx -> removeChildren(ctx, getDimCacheArgument(ctx), getRegionArgument(ctx), getChildRegionArgument(ctx))))))
                                /* TODO: Facade for reverse child setting ?
                                .then(literal(PARENT)
                                        .then(literal(SET)
                                                .then(Commands.argument(PARENT_REGION.toString(), StringArgumentType.word())
                                                        .suggests((ctx, builder) -> SetRegionParentArgumentType.parentRegion().listSuggestions(ctx, builder))
                                                        .executes(ctx -> setRegionParent(ctx.getSource(), RegionArgumentType.getRegion(ctx, REGION.toString()), RegionArgumentType.getRegion(ctx, PARENT_REGION.toString())))))
                                        .then(literal(CLEAR)
                                                .executes(ctx -> clearRegionParent(ctx.getSource(), RegionArgumentType.getRegion(ctx, REGION.toString())))))
                                 */
                                .then(literal(TELEPORT)
                                        .executes(ctx -> teleport(ctx.getSource(), getRegionArgument(ctx)))
                                        .then(Commands.argument(PLAYER.toString(), EntityArgument.player())
                                                .executes(ctx -> teleport(ctx.getSource(), getRegionArgument(ctx), getPlayerArgument(ctx))))
                                        .then(Commands.literal(SET.toString())
                                                .then(Commands.argument(TARGET.toString(), BlockPosArgument.blockPos())
                                                        .executes(ctx -> setTeleportPos(ctx, getRegionArgument(ctx), BlockPosArgument.getSpawnablePos(ctx, TARGET.toString()))))))));
    }


    private static int updateArea(CommandContext<CommandSourceStack> src, IMarkableRegion region, AreaType areaType, BlockPos pos1, BlockPos pos2) {
        IProtectedRegion parent = region.getParent();
        switch (areaType) {
            case CUBOID:
                CuboidArea cuboidArea = new CuboidArea(pos1, pos2);
                CuboidRegion cuboidRegion = (CuboidRegion) region;
                if (parent instanceof DimensionalRegion) {
                    int newPriority = LocalRegions.ensureHigherRegionPriorityFor(cuboidRegion, RegionConfig.DEFAULT_REGION_PRIORITY.get());
                }
                if (parent instanceof IMarkableRegion localParentRegion) {
                    CuboidArea parentArea = (CuboidArea) localParentRegion.getArea();
                    if (parentArea.contains(cuboidArea)) {
                        int newPriority = LocalRegions.ensureHigherRegionPriorityFor(cuboidRegion, localParentRegion.getPriority() + 1);
                    } else {
                        MutableComponent updateAreaFailMsg = Component.translatableWithFallback("cli.msg.info.region.spatial.area.update.fail", "Failed to update %s for region %s", buildRegionSpatialPropLink(region), buildRegionInfoLink(region, LOCAL));
                        sendCmdFeedback(src.getSource(), updateAreaFailMsg);
                        return 1;
                    }
                }
                MutableComponent updateAreaMsg = Component.translatableWithFallback("cli.msg.info.region.spatial.area.update", "Updated %s for region %s", buildRegionSpatialPropLink(region), buildRegionInfoLink(region, LOCAL));
                cuboidRegion.setArea(cuboidArea);
                RegionDataManager.save();
                sendCmdFeedback(src.getSource(), updateAreaMsg);
                break;
            case CYLINDER:
            case SPHERE:
            case POLYGON_3D:
            case PRISM:
                throw new UnsupportedOperationException("Unsupported region type");
        }
        return 0;
    }

    private static int renameRegion(CommandContext<CommandSourceStack> src, IMarkableRegion region, String regionName, DimensionRegionCache dimCache) {
        int res = checkValidRegionName(regionName, dimCache);
        if (res == -1) {
            sendCmdFeedback(src.getSource(), Component.translatableWithFallback("cli.msg.dim.info.region.create.name.invalid", "Invalid region name supplied: '%s'", regionName));
            return res;
        }
        if (res == 1) {
            sendCmdFeedback(src.getSource(), Component.translatableWithFallback("cli.msg.dim.info.region.create.name.exists", "Dimension %s already contains region with name %s", dimCache.getDimensionalRegion().getName(), buildRegionInfoLink(dimCache.getRegion(regionName), LOCAL)));
            return res;
        }
        // FIXME:
        dimCache.renameRegion(region, regionName);
        RegionDataManager.save();
        return 0;
    }

    private static int removeTeam(CommandContext<CommandSourceStack> src, PlayerTeam team, IMarkableRegion region, String affiliation) {
        MutableComponent undoLink = buildRegionActionUndoLink(src.getInput(), REMOVE, ADD);
        switch (affiliation) {
            case "member":
                if (region.hasMember(team.getName())) {
                    region.removeMember(team);
                    RegionDataManager.save();
                    sendCmdFeedback(src.getSource(), Component.translatableWithFallback("cli.msg.info.region.affiliation.team.removed", "Removed team '%s' from region %s",
                            team.getName(), buildRegionInfoLink(region, LOCAL)).append(" ").append(undoLink));
                }
                break;
            case "owner":
                if (region.hasOwner(team.getName())) {
                    region.removeOwner(team);
                    RegionDataManager.save();
                    sendCmdFeedback(src.getSource(), Component.translatableWithFallback("cli.msg.info.region.affiliation.team.removed", "Removed team '%s' from region %s",
                            team.getName(), buildRegionInfoLink(region, LOCAL)).append(" ").append(undoLink));
                }
                break;
            default:
                return 1;
        }
        return 0;
    }

    private static int addTeam(CommandContext<CommandSourceStack> src, PlayerTeam team, IMarkableRegion region, String affiliation) {
        MutableComponent undoLink = buildRegionActionUndoLink(src.getInput(), ADD, REMOVE);
        switch (affiliation) {
            case "member":
                if (!region.hasMember(team.getName())) {
                    region.addMember(team);
                    RegionDataManager.save();
                    sendCmdFeedback(src.getSource(), Component.translatableWithFallback("cli.msg.info.region.affiliation.team.added", "Added team '%s' with as '%s' to region %s",
                            team.getName(), affiliation, buildRegionInfoLink(region, LOCAL)).append(" ").append(undoLink));
                }
                break;
            case "owner":
                if (!region.hasOwner(team.getName())) {
                    region.addOwner(team);
                    RegionDataManager.save();
                    sendCmdFeedback(src.getSource(), Component.translatableWithFallback("cli.msg.info.region.affiliation.team.added", "Added team '%s' with as '%s' to region %s",
                            team.getName(), affiliation, buildRegionInfoLink(region, LOCAL)).append(" ").append(undoLink));
                }
                break;
            default:
                return 1;
        }
        return 0;
    }

    // TODO: Option to remove player by name
    private static int removePlayer(CommandContext<CommandSourceStack> src, String playerName, IMarkableRegion region, String affiliation) {
        return 1;
    }

    private static int removePlayer(CommandContext<CommandSourceStack> src, ServerPlayer player, IMarkableRegion region, String affiliation) {
        MutableComponent undoLink = buildRegionActionUndoLink(src.getInput(), REMOVE, ADD);
        switch (affiliation) {
            case "member":
                if (region.hasMember(player.getUUID())) {
                    region.removeMember(player);
                    RegionDataManager.save();
                    sendCmdFeedback(src.getSource(), Component.translatableWithFallback("cli.msg.info.region.affiliation.player.removed", "Removed player '%s' from region %s",
                            buildPlayerHoverComponent(player), buildRegionInfoLink(region, LOCAL)).append(" ").append(undoLink));
                }
                break;
            case "owner":
                if (region.hasOwner(player.getUUID())) {
                    region.removeOwner(player);
                    RegionDataManager.save();
                    sendCmdFeedback(src.getSource(), Component.translatableWithFallback("cli.msg.info.region.affiliation.player.removed", "Removed player '%s' from region %s",
                            buildPlayerHoverComponent(player), buildRegionInfoLink(region, LOCAL)).append(" ").append(undoLink));
                }
                break;
            default:
                return 1;
        }
        return 0;
    }

    private static int addPlayer(CommandContext<CommandSourceStack> src, ServerPlayer player, IMarkableRegion region, String affiliation) {
        MutableComponent undoLink = buildRegionActionUndoLink(src.getInput(), ADD, REMOVE);
        switch (affiliation) {
            case "member":
                if (!region.hasMember(player.getUUID())) {
                    region.addMember(player);
                    RegionDataManager.save();
                    sendCmdFeedback(src.getSource(), Component.translatableWithFallback("cli.msg.info.region.affiliation.player.added", "Added player '%s' as '%s' to region %s",
                            buildPlayerHoverComponent(player), affiliation, buildRegionInfoLink(region, LOCAL)).append(" ").append(undoLink));
                }
                break;
            case "owner":
                if (!region.hasOwner(player.getUUID())) {
                    region.addOwner(player);
                    RegionDataManager.save();
                    sendCmdFeedback(src.getSource(), Component.translatableWithFallback("cli.msg.info.region.affiliation.player.added", "Added player '%s' as '%s' to region %s",
                            buildPlayerHoverComponent(player), affiliation, buildRegionInfoLink(region, LOCAL)).append(" ").append(undoLink));
                }
                break;
            default:
                return 1;
        }
        return 0;
    }

    private static int removeChildren(CommandContext<CommandSourceStack> src, DimensionRegionCache dimCache, IMarkableRegion parent, IMarkableRegion child) {
        if (parent.hasChild(child)) {
            // FIXME: Removing child does not set priority correct with overlapping regions
            dimCache.getDimensionalRegion().addChild(child); // this also removes the child from the local parent
            child.setIsActive(false);
            LocalRegions.ensureLowerRegionPriorityFor((CuboidRegion) child, RegionConfig.DEFAULT_REGION_PRIORITY.get());
            RegionDataManager.save();
            MutableComponent parentLink = buildRegionInfoLink(parent, LOCAL);
            MutableComponent notLongerChildLink = buildRegionInfoLink(child, LOCAL);
            MutableComponent dimensionalLink = buildRegionInfoLink(dimCache.getDimensionalRegion(), RegionType.DIMENSION);
            sendCmdFeedback(src.getSource(), Component.translatableWithFallback("cli.msg.info.region.children.remove", "Removed child '%s' from region %s", notLongerChildLink, parentLink));
            sendCmdFeedback(src.getSource(), Component.translatableWithFallback("cli.msg.info.region.parent.clear", "Reset default parent for %s back to %s", notLongerChildLink, dimensionalLink));
            return 0;
        }
        // should not happen, due to RemoveRegionChildArgumentType should only provide valid child regions
        return -1;
    }

    private static int addChildren(CommandContext<CommandSourceStack> src, IMarkableRegion parent, IMarkableRegion child) {
        if (!parent.hasChild(child) && child.getParent() != null && child.getParent() instanceof DimensionalRegion) {
            parent.addChild(child);
            LocalRegions.ensureHigherRegionPriorityFor((CuboidRegion) child, parent.getPriority() + 1);
            RegionDataManager.save();
            MutableComponent parentLink = buildRegionInfoLink(parent, LOCAL);
            MutableComponent childLink = buildRegionInfoLink(child, LOCAL);
            MutableComponent undoLink = buildRegionActionUndoLink(src.getInput(), ADD, REMOVE);
            sendCmdFeedback(src.getSource(), Component.translatableWithFallback("cli.msg.info.region.children.add", "Added child %s to region %s", childLink, parentLink).append(" ").append(undoLink));
            return 0;
        }
        // should not happen, due to AddRegionChildArgumentType should only provide valid child regions
        return -1;
    }


    // Adds default flag for provided RegionFlag
    private static int addFlag(CommandContext<CommandSourceStack> src, IMarkableRegion region, RegionFlag flag) {
        if (!region.containsFlag(flag)) {
            IFlag iFlag = null;
            switch (flag.type) {
                case BOOLEAN_FLAG:
                    iFlag = new BooleanFlag(flag);
                    region.addFlag(iFlag);
                    break;
                case LIST_FLAG:
                case INT_FLAG:
                    break;
            }
            // TODO: More general: Trigger for adding flags?
            if (flag.name.contains("spawning")) {
                removeInvolvedEntities(src, region, flag);
            }
            RegionDataManager.save();
            // TODO: flag cmd info link
            sendCmdFeedback(src.getSource(), Component.translatableWithFallback("cli.msg.flags.added", "Added flag '%s' to region %s", buildFlagQuickInfo(iFlag),
                    buildRegionInfoLink(region, LOCAL)).append(" ").append(buildRegionActionUndoLink(src.getInput(), ADD, REMOVE)));
            return 0;
        }
        return 1;
    }

    public static void removeInvolvedEntities(CommandContext<CommandSourceStack> src, IProtectedRegion region, RegionFlag flag) {
        // FIXME: Level is where the command source is, not the target level of the region
        ServerLevel level = src.getSource().getLevel();
        Predicate<? super Entity> entityFilter = getEntityFilterForFlag(flag);
        if (region instanceof DimensionalRegion) {
            List<Entity> entities = getEntitiesToRemove(level, entityFilter, flag);
            entities.forEach(e -> e.setRemoved(Entity.RemovalReason.DISCARDED));
        }
        if (region instanceof IMarkableRegion) {
            List<Entity> entities = getEntitiesToRemove(level, (IMarkableRegion) region, entityFilter);
            entities.forEach(e -> e.setRemoved(Entity.RemovalReason.DISCARDED));
        }
        if (region instanceof GlobalRegion) {
            Map<ServerLevel, List<Entity>> entities = getEntitiesToRemove((GlobalRegion) region, entityFilter);
            entities.forEach((world, entityList) -> {
                entityList.forEach(e -> e.setRemoved(Entity.RemovalReason.DISCARDED));
            });
        }
    }

    private static Predicate<? super Entity> getEntityFilterForFlag(RegionFlag flag) {
        switch (flag) {
            case SPAWNING_ALL:
                // FIXME: does not remove ExperienceOrbEntity
                return e -> !(e instanceof Player);
            case SPAWNING_MONSTER:
                return HandlerUtil::isMonster;
            case SPAWNING_ANIMAL:
                return HandlerUtil::isAnimal;
            case SPAWNING_GOLEM:
                return e -> e instanceof AbstractGolem;
            case SPAWNING_TRADER:
                return e -> e instanceof WanderingTrader;
            case SPAWNING_SLIME:
                return e -> e instanceof Slime;
            case SPAWNING_VILLAGER:
                return HandlerUtil::isVillager;
            case SPAWNING_XP:
                return e -> e instanceof ExperienceOrb;
            default:
                return e -> false;
        }
    }

    private static List<Entity> getEntitiesToRemove(ServerLevel level, IMarkableRegion region, Predicate<? super Entity> entityFilter) {
        // TODO: make this work with areas of different shapes by manually implementing it
        // TODO: Use the predicate to determine if the entity is within the region
        // area.intersects(entity.getBoundingBox()) ...
        return level.getEntities((Entity) null, ((CuboidArea) region.getArea()).getArea(), entityFilter);
    }

    private static Map<ServerLevel, List<Entity>> getEntitiesToRemove(GlobalRegion region, Predicate<? super Entity> entityFilter) {
        // FIXME: Exclude entities from Local Regions or DimensionalRegions with flag
        return new HashMap<>();
    }

    private static List<Entity> getEntitiesToRemove(ServerLevel level, Predicate<? super Entity> entityFilter, RegionFlag flag) {
        List<? extends Entity> entities = level.getEntities(EntityTypeTest.forClass(Entity.class), entityFilter);
        // List<Entity> entities = level.getEntities(null, entityFilter);
        // don't consider entities, which are currently in a Local Region which doesn't have the flag
        // TODO: fixme after flags can be negated (either flag is not existent or deactivated...)
        return entities.stream()
                .filter(e -> isInRegionWithoutFlag(level, flag, e))
                .collect(Collectors.toList());
    }

    private static boolean isInRegionWithoutFlag(ServerLevel level, RegionFlag flag, Entity e) {
        return LocalRegions.getRegionWithoutFlag(flag, e.blockPosition(), level.dimension()) == null;
    }

    private static int removeFlag(CommandContext<CommandSourceStack> src, IMarkableRegion region, RegionFlag flag) {
        if (region.containsFlag(flag)) {
            region.removeFlag(flag.name);
            RegionDataManager.save();
            sendCmdFeedback(src.getSource(), Component.translatableWithFallback("cli.msg.flags.removed", "Removed flag '%s' from region %s",
                    flag.name, buildRegionInfoLink(region, LOCAL)).append(" ").append(buildRegionActionUndoLink(src.getInput(), REMOVE, ADD)));
            return 0;
        }
        return 1;
    }

    private static int setAlertState(CommandContext<CommandSourceStack> src, IMarkableRegion region, boolean showAlert) {
        boolean oldState = !region.isMuted();
        region.setIsMuted(showAlert);
        RegionDataManager.save();
        if (oldState == region.isMuted()) {
            boolean isEnabled = !region.isMuted();
            MutableComponent undoLink = buildRegionActionUndoLink(src.getInput(), showAlert ? TRUE : FALSE, showAlert ? FALSE : TRUE);
            sendCmdFeedback(src.getSource(), Component.translatableWithFallback("cli.msg.info.region.state.alert.set.value", "Changed alert state for region %s: %s -> %s",
                    buildRegionInfoLink(region, LOCAL), oldState, isEnabled).append(" ").append(undoLink));
        }
        return 0;
    }

    private static int setAlertState(CommandContext<CommandSourceStack> src, IMarkableRegion region) {
        return setAlertState(src, region, !region.isMuted());
    }

    private static int setEnableState(CommandContext<CommandSourceStack> src, IMarkableRegion region, boolean enable) {
        boolean oldState = region.isActive();
        region.setIsActive(enable);
        RegionDataManager.save();
        if (oldState != region.isActive()) {
            MutableComponent undoLink = buildRegionActionUndoLink(src.getInput(), enable ? TRUE : FALSE, enable ? FALSE : TRUE);
            sendCmdFeedback(src.getSource(), Component.translatableWithFallback("cli.msg.info.region.state.enable.set.value", "Changed enable state for region %s: %s -> %s",
                    buildRegionInfoLink(region, LOCAL), oldState, region.isActive()).append(" ").append(undoLink));
        }
        return 0;
    }

    private static int setEnableState(CommandContext<CommandSourceStack> src, IMarkableRegion region) {
        return setEnableState(src, region, !region.isActive());
    }

    private static int setPriority(CommandContext<CommandSourceStack> src, IMarkableRegion region, int priority, int factor) {
        long newValue = (long) region.getPriority() + ((long) priority * factor);
        if (Integer.MAX_VALUE - newValue > 0) {
            return setPriority(src, region, (int) newValue);
        } else {
            sendCmdFeedback(src.getSource(), Component.translatableWithFallback("cli.msg.warn.region.state.priority.set.invalid", "Unable to change priority for region %s: %s is to high/low", buildRegionInfoLink(region, LOCAL), newValue));
            return -1;
        }
    }

    /**
     * Attempt to set new priority for the given region. <br>
     * Fails if region priority is used by an overlapping region at same hierarchy level.
     *
     * @param src
     * @param region
     * @param priority
     * @return
     */
    private static int setPriority(CommandContext<CommandSourceStack> src, IMarkableRegion region, int priority) {
        CuboidRegion cuboidRegion = (CuboidRegion) region;
        List<CuboidRegion> intersectingRegions = LocalRegions.getIntersectingRegionsFor(cuboidRegion);
        boolean existRegionWithSamePriority = intersectingRegions
                .stream()
                .anyMatch(r -> r.getPriority() == priority);
        IProtectedRegion parent = region.getParent();
        if (parent instanceof IMarkableRegion) {
            int parentPriority = ((IMarkableRegion) parent).getPriority();
            if (parentPriority >= priority) {
                MutableComponent updatePriorityFailMsg = Component.translatableWithFallback("cli.msg.info.region.state.priority.set.fail.to-low", "Unable to set priority for region %s. The priority is not higher than its parents priority", buildRegionInfoLink(region, LOCAL));
                sendCmdFeedback(src.getSource(), updatePriorityFailMsg);
                return 1;
            }
        }
        if (existRegionWithSamePriority) {
            MutableComponent updatePriorityFailMsg = Component.translatableWithFallback("cli.msg.info.region.state.priority.set.fail.same", "Unable to set priority for region %s. There is already another region with priority %s.", buildRegionInfoLink(region, LOCAL), priority);
            sendCmdFeedback(src.getSource(), updatePriorityFailMsg);
            return 1;
        } else {
            int oldPriority = region.getPriority();
            if (oldPriority != priority) {
                region.setPriority(priority);
                RegionDataManager.save();
                MutableComponent undoLink = buildRegionActionUndoLink(src.getInput(), String.valueOf(oldPriority), String.valueOf(priority));
                sendCmdFeedback(src.getSource(), Component.translatableWithFallback("cli.msg.info.region.state.priority.set.success", "Changed priority for region %s: %s -> %s",
                        buildRegionInfoLink(region, LOCAL), oldPriority, region.getPriority()).append(" ").append(undoLink));
                return 0;
            } else {
                sendCmdFeedback(src.getSource(), Component.translatableWithFallback("cli.msg.info.region.state.priority.set.fail.no-change", "Unable to set priority for region %s. The priority is the same.", buildRegionInfoLink(region, LOCAL)));
                return 1;
            }
        }
    }

    private static int promptRegionInfo(CommandSourceStack src, IMarkableRegion region) {
        // == Region [<name>] overview ==
        sendCmdFeedback(src, buildRegionOverviewHeader(region, LOCAL));
        // Flags: [n flag(s)][+]
        sendCmdFeedback(src, buildInfoComponent("cli.msg.info.region.flag", "Flags", buildFlagListLink(region, RegionType.LOCAL)));
        // Spatial: [Spatial Properties]
        sendCmdFeedback(src, buildInfoComponent("cli.msg.info.region.spatial", "Spatial", buildRegionSpatialPropLink(region)));
        // Affiliations: [owners], [members], [<listAffiliations>]
        sendCmdFeedback(src, buildInfoComponent("cli.msg.info.region.affiliation", "Affiliation", buildAffiliationLinks(region, RegionType.LOCAL)));
        // Hierarchy: [parent][-|+], [n children][+]
        MutableComponent regionHierarchy = Component.translatableWithFallback("cli.msg.info.region.hierarchy", "Hierarchy")
                .append(": ")
                .append(buildRegionParentLink(region))
                .append(Component.literal(", ").withStyle(RESET))
                .append(buildRegionChildrenLink(region, LOCAL));
        sendCmdFeedback(src, regionHierarchy);
        // State: [State]
        sendCmdFeedback(src, buildInfoComponent("cli.msg.info.region.state", "State", buildRegionStateLink(region)));
        return 0;
    }

    private static int promptRegionChildren(CommandSourceStack src, IMarkableRegion region, int pageNo) {
        List<IMarkableRegion> children = region.getChildren().values().stream().map(r -> (IMarkableRegion) r).collect(Collectors.toList());
        MutableComponent childRegionList = Component.literal("");
        if (children.isEmpty()) {
            MutableComponent noChildrenText = Component.translatableWithFallback("cli.msg.info.region.children.empty", "No children defined for region %s", buildRegionInfoLink(region, LOCAL));
            childRegionList.append(noChildrenText);
            sendCmdFeedback(src, childRegionList);
        }
        List<MutableComponent> regionPagination = buildPaginationComponents(
                buildRegionChildrenHeader(region, LOCAL),
                buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), CHILDREN.toString()),
                buildRemoveRegionEntries(region, children, LOCAL),
                pageNo,
                Component.literal(" - ").append(buildRegionAddChildrenLink(region)));
        regionPagination.forEach(line -> sendCmdFeedback(src, line));
        return 0;
    }

    /**
     * == Affiliation '%s' for '%s'==
     * Players: [n player(s)][+]
     * Teams: [m team(s)][+]
     */
    private static int promptRegionAffiliates(CommandSourceStack src, IMarkableRegion region, String affiliation) {
        sendCmdFeedback(src, buildAffiliationHeader(region, affiliation, RegionType.LOCAL));
        sendCmdFeedback(src, buildAffiliationPlayerListLink(region, affiliation, RegionType.LOCAL));
        sendCmdFeedback(src, buildAffiliationTeamListLink(region, affiliation, RegionType.LOCAL));
        return 0;
    }

    private static int promptRegionAffiliationList(CommandSourceStack src, IMarkableRegion region, String affiliation, AffiliationType affiliationType, int pageNo) {
        List<String> affiliateNames = getAffiliateList(region, affiliation, affiliationType);
        if (affiliateNames.isEmpty()) {
            String fallback = "No " + affiliationType.name + "s defined as '%s' in %s";
            String key = "cli.msg.info.region.affiliation." + affiliationType.name + ".empty";
            sendCmdFeedback(src, Component.translatableWithFallback(key, fallback, affiliation, buildRegionInfoLink(region, LOCAL)));
            return 1;
        }
        List<MutableComponent> regionPagination = buildPaginationComponents(
                buildAffiliationHeader(region, affiliation, affiliationType, RegionType.LOCAL),
                buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), affiliation, affiliationType.name),
                buildRemoveAffiliationEntries(region, affiliateNames, affiliationType, affiliation, RegionType.LOCAL),
                pageNo,
                Component.literal(" - ").append(buildAddAffiliateLink(region, affiliation, affiliationType, RegionType.LOCAL)));
        regionPagination.forEach(line -> sendCmdFeedback(src, line));
        return 0;
    }

    /**
     * Prompt region spatial properties like teleport location and area.
     * == Region [<name>] spatial properties ==
     * Location: [dimInfo]@[tpCoordinates]
     * Area: [spatialProperties]
     *
     * @param src
     * @param region
     * @return
     */
    public static int promptRegionSpatialProperties(CommandSourceStack src, IMarkableRegion region) {
        sendCmdFeedback(src, buildHeader(Component.translatableWithFallback("cli.msg.info.header.for", "== %s for %s ==", buildRegionSpatialPropLink(region), buildRegionInfoLink(region, LOCAL))));
        sendCmdFeedback(src, buildInfoComponent("cli.msg.info.region.spatial.location", "Location", buildDimensionTeleportLink(region)));
        sendCmdFeedback(src, buildInfoComponent("cli.msg.info.region.spatial.area", "Area", buildRegionAreaDetailComponent(region)));
        return 0;
    }

    /**
     * Prompt the region state to the command issuer.
     * ==  [state] for [<name>]  ==
     * Enabled: [true|false]
     * Priority: n [#][+5][-5]
     * Alert: [on|off]
     *
     * @param src
     * @param region
     * @return
     */
    public static int promptRegionState(CommandSourceStack src, IMarkableRegion region) {
        sendCmdFeedback(src, buildHeader(Component.translatableWithFallback("cli.msg.info.header.for", "== %s for %s ==", buildRegionStateLink(region), buildRegionInfoLink(region, LOCAL))));
        sendCmdFeedback(src, buildInfoComponent("cli.msg.info.region.state.priority", "Priority", buildRegionPriorityComponent(region)));
        sendCmdFeedback(src, buildInfoComponent("cli.msg.info.region.state.enable", "Enabled", buildRegionEnableComponent(region)));
        sendCmdFeedback(src, buildInfoComponent("cli.msg.info.region.state.alert", "Alert", buildRegionAlertComponentLink(region)));
        return 0;
    }

    public static int promptRegionFlags(CommandSourceStack src, IMarkableRegion region, int pageNo) {
        if (region.getFlags().isEmpty()) {
            sendCmdFeedback(src, Component.translatableWithFallback("cli.msg.info.region.flag.empty", "No flags defined in region %s", buildRegionInfoLink(region, LOCAL)));
            return 1;
        }
        List<IFlag> flags = LocalRegions.getSortedFlags(region);
        List<MutableComponent> flagPagination = buildPaginationComponents(
                buildFlagHeader(region, LOCAL),
                buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), FLAG.toString()),
                buildRemoveFlagEntries(region, flags, LOCAL),
                pageNo,
                Component.literal(" - ").append(buildRegionAddFlagLink(region))
        );
        flagPagination.forEach(line -> sendCmdFeedback(src, line));
        return 0;
    }

    // TODO: Only with region marker
    // assumption: regions are only updated with the region marker when in the same dimension
    private static int updateRegion(CommandContext<CommandSourceStack> src, IMarkableRegion region) {
        try {
            Player player = src.getSource().getPlayerOrException();
            ItemStack maybeStick = player.getMainHandItem();
            if (StickUtil.isVanillaStick(maybeStick)) {
                try {
                    AbstractStick abstractStick = StickUtil.getStick(maybeStick);
                    if (abstractStick.getStickType() == StickType.MARKER) {
                        MarkerStick marker = (MarkerStick) abstractStick;
                        // TODO: RegionDataManager.get().update(regionName, marker);
                    }
                } catch (StickException e) {
                    sendCmdFeedback(src.getSource(), "CommandSource is not player. Aborting.. Needs RegionMarker with Block-NBT data in player hand");
                }
            }
        } catch (CommandSyntaxException e) {
            e.printStackTrace();
        }
        return 0;
    }

    // TODO
    private static int listRegionsAround(CommandSourceStack source) {

        return 0;
    }

    private static int teleport(CommandSourceStack src, IMarkableRegion region) {
        try {
            ServerPlayer player = src.getPlayerOrException();
            src.getServer().getCommands().getDispatcher().execute(buildRegionTpCmd(region, player.getScoreboardName()), src);
            return 0;
        } catch (CommandSyntaxException e) {
            YetAnotherWorldProtector.LOGGER.warn("Unable to teleport command source to region.");
            return -1;
        }
    }

    private static int teleport(CommandSourceStack src, IMarkableRegion region, Player player) {
        try {
            src.getServer().getCommands().getDispatcher().execute(buildRegionTpCmd(region, player.getScoreboardName()), src);
            return 0;
        } catch (CommandSyntaxException e) {
            YetAnotherWorldProtector.LOGGER.warn("Error executing teleport command.");
            // TODO: error executing tp command
            return -1;
        }
    }

    // Todo: Enable/Disable teleporting? - Only for owners?
    private static int setTeleportPos(CommandContext<CommandSourceStack> src, IMarkableRegion region, BlockPos target) {
        if (!region.getTpTarget().equals(target)) {
            region.setTpTarget(target);
            RegionDataManager.save();
            MutableComponent newTpTargetLink = buildDimensionalBlockTpLink(region.getDim(), target);
            sendCmdFeedback(src.getSource(), Component.translatableWithFallback("cli.msg.info.region.spatial.location.teleport.set", "Set new teleport target for region %s: '%s'", buildRegionInfoLink(region, LOCAL), newTpTargetLink));
            return 0;
        }
        return 1;
    }

}
