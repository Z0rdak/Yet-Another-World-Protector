package de.z0rdak.yawp.commands;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.builder.RequiredArgumentBuilder;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.commands.arguments.flag.RegionFlagArgumentType;
import de.z0rdak.yawp.commands.arguments.region.AddRegionChildArgumentType;
import de.z0rdak.yawp.commands.arguments.region.RegionArgumentType;
import de.z0rdak.yawp.commands.arguments.region.RemoveRegionChildArgumentType;
import de.z0rdak.yawp.config.server.RegionConfig;
import de.z0rdak.yawp.core.affiliation.PlayerContainer;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.flag.BooleanFlag;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.*;
import de.z0rdak.yawp.core.stick.AbstractStick;
import de.z0rdak.yawp.core.stick.MarkerStick;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.*;
import net.minecraft.command.CommandSource;
import net.minecraft.command.argument.BlockPosArgumentType;
import net.minecraft.command.argument.DimensionArgumentType;
import net.minecraft.command.argument.EntityArgumentType;
import net.minecraft.command.argument.TeamArgumentType;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.scoreboard.Team;
import net.minecraft.server.command.CommandManager;
import net.minecraft.server.command.ServerCommandSource;
import net.minecraft.server.network.ServerPlayerEntity;
import net.minecraft.text.LiteralTextContent;
import net.minecraft.text.MutableText;
import net.minecraft.text.TranslatableTextContent;
import net.minecraft.util.math.BlockPos;

import java.util.*;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.util.CommandUtil.*;
import static de.z0rdak.yawp.util.MessageUtil.*;

public class RegionCommands {

    public static final LiteralArgumentBuilder<ServerCommandSource> REGION_COMMAND = registerRegionCommands();

    private RegionCommands() {
    }

    public static LiteralArgumentBuilder<ServerCommandSource> registerRegionCommands() {
        return literal(REGION)
                .then(CommandManager.argument(DIMENSION.toString(), DimensionArgumentType.dimension())
                        .then(regionCommands()));
    }

    /**
     * TODO: Command to invert enable and alert based on region state
     * TODO: Renaming a region
     *
     * @return
     */
    private static RequiredArgumentBuilder<ServerCommandSource, String> regionCommands() {
        List<String> affiliationList = Arrays.asList("member", "owner");
        return CommandManager.argument(REGION.toString(), StringArgumentType.word())
                .suggests((ctx, builder) -> RegionArgumentType.region().listSuggestions(ctx, builder))
                .executes(ctx -> promptRegionInfo(ctx.getSource(), getRegionArgument(ctx)))
                .then(literal(INFO)
                        .executes(ctx -> promptRegionInfo(ctx.getSource(), getRegionArgument(ctx))))
                .then(literal(SPATIAL)
                        .executes(ctx -> promptRegionSpatialProperties(ctx.getSource(), getRegionArgument(ctx))))
                .then(literal(STATE)
                        .executes(ctx -> promptRegionState(ctx.getSource(), getRegionArgument(ctx)))
                        .then(literal(ALERT)
                                // TODO: add default true and toggle cmd
                                .then(CommandManager.argument(ALERT.toString(), BoolArgumentType.bool())
                                        .executes(ctx -> setAlertState(ctx.getSource(), getRegionArgument(ctx), getAlertArgument(ctx)))))
                        .then(literal(ENABLE)
                                // TODO: add default true and toggle cmd
                                .then(CommandManager.argument(ENABLE.toString(), BoolArgumentType.bool())
                                        .executes(ctx -> setEnableState(ctx.getSource(), getRegionArgument(ctx), getEnableArgument(ctx)))))
                        .then(literal(PRIORITY)
                                .then(CommandManager.argument(PRIORITY.toString(), IntegerArgumentType.integer())
                                        .executes(ctx -> setPriority(ctx.getSource(), getRegionArgument(ctx), getPriorityArgument(ctx))))
                                .then(literal(INC)
                                        .then(CommandManager.argument(PRIORITY.toString(), IntegerArgumentType.integer())
                                                .executes(ctx -> setPriority(ctx.getSource(), getRegionArgument(ctx), getPriorityArgument(ctx), 1))))
                                .then(literal(DEC)
                                        .then(CommandManager.argument(PRIORITY.toString(), IntegerArgumentType.integer())
                                                .executes(ctx -> setPriority(ctx.getSource(), getRegionArgument(ctx), getPriorityArgument(ctx), -1))))))
                .then(literal(LIST)
                        .then(literal(FLAG)
                                .executes(ctx -> promptRegionFlags(ctx.getSource(), getRegionArgument(ctx))))
                        .then(literal(OWNER)
                                .executes(ctx -> promptRegionAffiliates(ctx.getSource(), getRegionArgument(ctx), "owner"))
                                .then(literal(TEAM)
                                        .executes(ctx -> promptRegionAffiliationTeamList(ctx.getSource(), getRegionArgument(ctx), "owner")))
                                .then(literal(PLAYER)
                                        .executes(ctx -> promptRegionAffiliationPlayerList(ctx.getSource(), getRegionArgument(ctx), "owner"))))
                        .then(literal(MEMBER)
                                .executes(ctx -> promptRegionAffiliates(ctx.getSource(), getRegionArgument(ctx), "member"))
                                .then(literal(TEAM)
                                        .executes(ctx -> promptRegionAffiliationTeamList(ctx.getSource(), getRegionArgument(ctx), "member")))
                                .then(literal(PLAYER)
                                        .executes(ctx -> promptRegionAffiliationPlayerList(ctx.getSource(), getRegionArgument(ctx), "member"))))
                        .then(literal(CHILDREN)
                                .executes(ctx -> promptRegionChildren(ctx.getSource(), getRegionArgument(ctx)))))
                .then(literal(AREA)
                        .then(CommandManager.literal(AreaType.CUBOID.areaType)
                                .then(CommandManager.argument("pos1", BlockPosArgumentType.blockPos())
                                        .then(CommandManager.argument("pos2", BlockPosArgumentType.blockPos())
                                                .executes(ctx -> updateArea(ctx.getSource(), getRegionArgument(ctx), AreaType.CUBOID,
                                                        BlockPosArgumentType.getBlockPos(ctx, "pos1"),
                                                        BlockPosArgumentType.getBlockPos(ctx, "pos2")))))
                        ))
                // TODO: Only with marker
                //.then(literal(UPDATE)
                //        .then(CommandManager.argument(AREA.toString(), StringArgumentType.word())
                //                .suggests((ctx, builder) -> AreaArgumentType.areaType().listSuggestions(ctx, builder))
                //                .executes(ctx -> updateRegion(ctx.getSource(), getRegionArgument(ctx)))))
                .then(literal(ADD)
                        .then(literal(CommandConstants.PLAYER)
                                .then(CommandManager.argument(CommandConstants.AFFILIATION.toString(), StringArgumentType.word())
                                        .suggests((ctx, builder) -> CommandSource.suggestMatching(affiliationList, builder))
                                        .then(CommandManager.argument(CommandConstants.PLAYER.toString(), EntityArgumentType.player())
                                                .executes(ctx -> addPlayer(ctx.getSource(), getPlayerArgument(ctx), getRegionArgument(ctx), getAffiliationArgument(ctx)))))
                                .then(CommandManager.argument(CommandConstants.AFFILIATION.toString(), StringArgumentType.word())
                                        .suggests((ctx, builder) -> CommandSource.suggestMatching(affiliationList, builder))
                                        .then(CommandManager.argument(CommandConstants.PLAYER.toString(), EntityArgumentType.player())
                                                .executes(ctx -> addPlayer(ctx.getSource(), getPlayerArgument(ctx), getRegionArgument(ctx), getAffiliationArgument(ctx))))))
                        .then(literal(CommandConstants.TEAM)
                                .then(CommandManager.argument(CommandConstants.AFFILIATION.toString(), StringArgumentType.word())
                                        .suggests((ctx, builder) -> CommandSource.suggestMatching(affiliationList, builder))
                                        .then(CommandManager.argument(CommandConstants.TEAM.toString(), TeamArgumentType.team())
                                                .executes(ctx -> addTeam(ctx.getSource(), getTeamArgumentType(ctx), getRegionArgument(ctx), getAffiliationArgument(ctx)))))
                                .then(CommandManager.argument(CommandConstants.AFFILIATION.toString(), StringArgumentType.word())
                                        .suggests((ctx, builder) -> CommandSource.suggestMatching(affiliationList, builder))
                                        .then(CommandManager.argument(CommandConstants.TEAM.toString(), TeamArgumentType.team())
                                                .executes(ctx -> addTeam(ctx.getSource(), getTeamArgumentType(ctx), getRegionArgument(ctx), getAffiliationArgument(ctx))))))
                        .then(literal(FLAG)
                                .then(CommandManager.argument(FLAG.toString(), StringArgumentType.word())
                                        .suggests((ctx, builder) -> RegionFlagArgumentType.flag().listSuggestions(ctx, builder))
                                        .executes(ctx -> addFlag(ctx.getSource(), getRegionArgument(ctx), getFlagArgument(ctx)))))
                        .then(literal(CHILD)
                                .then(CommandManager.argument(CHILD.toString(), StringArgumentType.word())
                                        .suggests((ctx, builder) -> AddRegionChildArgumentType.potentialChildRegions().listSuggestions(ctx, builder))
                                        .executes(ctx -> addChildren(ctx.getSource(), getRegionArgument(ctx), getChildRegionArgument(ctx))))))
                .then(literal(REMOVE)
                        .then(literal(CommandConstants.PLAYER)
                                .then(CommandManager.argument(CommandConstants.AFFILIATION.toString(), StringArgumentType.word())
                                        .suggests((ctx, builder) -> CommandSource.suggestMatching(affiliationList, builder))
                                        .then(CommandManager.argument(CommandConstants.PLAYER.toString(), EntityArgumentType.player())
                                                .executes(ctx -> removePlayer(ctx.getSource(), getPlayerArgument(ctx), getRegionArgument(ctx), getAffiliationArgument(ctx)))))
                                .then(CommandManager.argument(CommandConstants.AFFILIATION.toString(), StringArgumentType.word())
                                        .suggests((ctx, builder) -> CommandSource.suggestMatching(affiliationList, builder))
                                        .then(CommandManager.argument(CommandConstants.PLAYER.toString(), EntityArgumentType.player())
                                                .executes(ctx -> removePlayer(ctx.getSource(), getPlayerArgument(ctx), getRegionArgument(ctx), getAffiliationArgument(ctx))))))
                        .then(literal(CommandConstants.TEAM)
                                .then(CommandManager.argument(CommandConstants.AFFILIATION.toString(), StringArgumentType.word())
                                        .suggests((ctx, builder) -> CommandSource.suggestMatching(affiliationList, builder))
                                        .then(CommandManager.argument(CommandConstants.TEAM.toString(), TeamArgumentType.team())
                                                .executes(ctx -> removeTeam(ctx.getSource(), getTeamArgumentType(ctx), getRegionArgument(ctx), getAffiliationArgument(ctx)))))
                                .then(CommandManager.argument(CommandConstants.AFFILIATION.toString(), StringArgumentType.word())
                                        .suggests((ctx, builder) -> CommandSource.suggestMatching(affiliationList, builder))
                                        .then(CommandManager.argument(CommandConstants.TEAM.toString(), TeamArgumentType.team())
                                                .executes(ctx -> removeTeam(ctx.getSource(), getTeamArgumentType(ctx), getRegionArgument(ctx), getAffiliationArgument(ctx))))))
                        .then(literal(FLAG)
                                .then(CommandManager.argument(FLAG.toString(), StringArgumentType.word())
                                        .suggests((ctx, builder) -> RegionFlagArgumentType.flag().listSuggestions(ctx, builder))
                                        .executes(ctx -> removeFlag(ctx.getSource(), getRegionArgument(ctx), getFlagArgument(ctx)))))
                        .then(literal(CHILD)
                                .then(CommandManager.argument(CHILD.toString(), StringArgumentType.word())
                                        .suggests((ctx, builder) -> RemoveRegionChildArgumentType.childRegions().listSuggestions(ctx, builder))
                                        .executes(ctx -> removeChildren(ctx.getSource(), getDimCacheArgument(ctx), getRegionArgument(ctx), getChildRegionArgument(ctx))))))
                /* TODO: Facade for reverse child setting ?
                .then(literal(PARENT)
                        .then(literal(SET)
                                .then(CommandManager.argument(PARENT_REGION.toString(), StringArgumentType.word())
                                        .suggests((ctx, builder) -> SetRegionParentArgumentType.parentRegion().listSuggestions(ctx, builder))
                                        .executes(ctx -> setRegionParent(ctx.getSource(), RegionArgumentType.getRegion(ctx, REGION.toString()), RegionArgumentType.getRegion(ctx, PARENT_REGION.toString())))))
                        .then(literal(CLEAR)
                                .executes(ctx -> clearRegionParent(ctx.getSource(), RegionArgumentType.getRegion(ctx, REGION.toString())))))
                 */
                .then(literal(TELEPORT)
                        .executes(ctx -> teleport(ctx.getSource(), getRegionArgument(ctx)))
                        .then(CommandManager.argument(PLAYER.toString(), EntityArgumentType.player())
                                .executes(ctx -> teleport(ctx.getSource(), getRegionArgument(ctx), getPlayerArgument(ctx))))
                        .then(CommandManager.literal(SET.toString())
                                .then(CommandManager.argument(TARGET.toString(), BlockPosArgumentType.blockPos())
                                        .executes(ctx -> setTeleportPos(ctx.getSource(), getRegionArgument(ctx), BlockPosArgumentType.getBlockPos(ctx, TARGET.toString()))))));
    }

    private static int updateArea(ServerCommandSource src, IMarkableRegion region, AreaType areaType, BlockPos pos1, BlockPos pos2) {
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
                        MutableText updateAreaFailMsg = MutableText.of(new TranslatableTextContent("cli.msg.info.region.spatial.area.update.fail", buildRegionSpatialPropLink(region), buildRegionInfoLink(region)));
                        sendCmdFeedback(src, updateAreaFailMsg);
                        return 1;
                    }
                }
                MutableText updateAreaMsg = MutableText.of(new TranslatableTextContent("cli.msg.info.region.spatial.area.update", buildRegionSpatialPropLink(region), buildRegionInfoLink(region)));
                cuboidRegion.setArea(cuboidArea);
                RegionDataManager.save();
                sendCmdFeedback(src, updateAreaMsg);
                break;
            case CYLINDER:
            case SPHERE:
            case POLYGON_3D:
            case PRISM:
                throw new UnsupportedOperationException("Unsupported region type");
        }
        return 0;
    }

    private static int removeTeam(ServerCommandSource src, Team team, IMarkableRegion region, String affiliation) {
        switch (affiliation) {
            case "member":
                if (!region.getMembers().containsTeam(team)) {
                    region.removeOwner(team);
                    RegionDataManager.save();
                    sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.team.removed", team.getName(), region.getName())));
                }
                break;
            case "owner":
                if (!region.getOwners().containsTeam(team)) {
                    region.removeOwner(team);
                    RegionDataManager.save();
                    sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.team.removed", team.getName(), region.getName())));
                }
                break;
            default:
                // TODO Create new affiliation with no permissions?
                return 1;
        }
        return 0;
    }

    private static int addTeam(ServerCommandSource src, Team team, IMarkableRegion region, String affiliation) {
        switch (affiliation) {
            case "member":
                if (!region.getMembers().containsTeam(team)) {
                    region.addMember(team);
                    RegionDataManager.save();
                    sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.team.added", team.getName(), affiliation, region.getName())));
                }
                break;
            case "owner":
                if (!region.getOwners().containsTeam(team)) {
                    region.addOwner(team);
                    RegionDataManager.save();
                    sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.team.added", team.getName(), affiliation, region.getName())));
                }
                break;
            default:
                // TODO Create new affiliation with no permissions?
                return 1;
        }
        return 0;
    }

    private static int removePlayer(ServerCommandSource src, ServerPlayerEntity player, IMarkableRegion region, String affiliation) {
        switch (affiliation) {
            case "member":
                if (region.getMembers().containsPlayer(player.getUuid())) {
                    region.removeMember(player);
                    RegionDataManager.save();
                    sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.player.removed", player.getEntityName(), region.getName())));
                }
                break;
            case "owner":
                if (region.getOwners().containsPlayer(player.getUuid())) {
                    region.removeOwner(player);
                    RegionDataManager.save();
                    sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.player.removed", player.getEntityName(), region.getName())));
                }
                break;
            default:
                // TODO Create new affiliation with no permissions?
                return 1;
        }
        return 0;
    }

    private static int addPlayer(ServerCommandSource src, ServerPlayerEntity player, IMarkableRegion region, String affiliation) {
        switch (affiliation) {
            case "member":
                if (!region.getMembers().containsPlayer(player.getUuid())) {
                    region.addMember(player);
                    RegionDataManager.save();
                    sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.player.added", player.getEntityName(), affiliation, region.getName())));
                }
                break;
            case "owner":
                if (!region.getOwners().containsPlayer(player.getUuid())) {
                    region.addOwner(player);
                    RegionDataManager.save();
                    sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.player.added", player.getEntityName(), affiliation, region.getName())));
                }
                break;
            default:
                // TODO Create new affiliation with no permissions?
                return 1;
        }
        return 0;
    }

    private static int removeChildren(ServerCommandSource src, DimensionRegionCache dimCache, IMarkableRegion parent, IMarkableRegion child) {
        if (parent.hasChild(child)) {
            // FIXME: Removing child does not set priority correct with overlapping regions
            dimCache.getDimensionalRegion().addChild(child); // this also removes the child from the local parent
            child.setIsActive(false);
            LocalRegions.ensureLowerRegionPriorityFor((CuboidRegion) child, RegionConfig.DEFAULT_REGION_PRIORITY.get());
            RegionDataManager.save();
            MutableText parentLink = buildRegionInfoLink(parent);
            MutableText notLongerChildLink = buildRegionInfoLink(child);
            MutableText dimensionalLink = buildDimensionalInfoLink(child.getDim());
            sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.info.region.children.remove", notLongerChildLink, parentLink)));
            sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.info.region.parent.clear", notLongerChildLink, dimensionalLink)));
            return 0;
        }
        // should not happen, due to RemoveRegionChildArgumentType should only provide valid child regions
        return -1;
    }

    private static int addChildren(ServerCommandSource src, IMarkableRegion parent, IMarkableRegion child) {
        if (!parent.hasChild(child) && child.getParent() != null && child.getParent() instanceof DimensionalRegion) {
            parent.addChild(child);
            LocalRegions.ensureHigherRegionPriorityFor((CuboidRegion) child, parent.getPriority() + 1);
            RegionDataManager.save();

            MutableText parentLink = buildRegionInfoLink(parent);
            MutableText childLink = buildRegionInfoLink(child);
            sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.info.region.children.add", childLink, parentLink)));
            return 0;
        }
        // should not happen, due to AddRegionChildArgumentType should only provide valid child regions
        return -1;
    }

    // Adds default flag for provided RegionFlag
    private static int addFlag(ServerCommandSource src, IMarkableRegion region, RegionFlag flag) {
        if (!region.containsFlag(flag)) {
            switch (flag.type) {
                case BOOLEAN_FLAG:
                    region.addFlag(new BooleanFlag(flag));
                    break;
                case LIST_FLAG:
                case INT_FLAG:
                    break;
            }
            RegionDataManager.save();
            sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.flags.added", flag.name, region.getName())));
            return 0;
        }
        return 1;
    }

    private static int removeFlag(ServerCommandSource src, IMarkableRegion region, RegionFlag flag) {
        if (region.containsFlag(flag)) {
            region.removeFlag(flag.name);
            RegionDataManager.save();
            sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.flags.removed", flag.name, region.getName())));
            return 0;
        }
        return 1;
    }

    private static int setAlertState(ServerCommandSource src, IMarkableRegion region, boolean showAlert) {
        boolean wasEnabled = !region.isMuted();
        region.setIsMuted(showAlert);
        RegionDataManager.save();
        if (wasEnabled == region.isMuted()) {
            boolean isEnabled = !region.isMuted();
            sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.info.region.state.alert.set.value", region.getName(), wasEnabled, isEnabled)));
        }
        return 0;
    }

    private static int setEnableState(ServerCommandSource src, IMarkableRegion region, boolean enable) {
        boolean oldState = region.isActive();
        region.setIsActive(enable);
        RegionDataManager.save();
        if (oldState != region.isActive()) {
            sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.info.region.state.enable.set.value", region.getName(), oldState, region.isActive())));
        }
        return 0;
    }

    private static int setPriority(ServerCommandSource src, IMarkableRegion region, int priority, int factor) {
        long newValue = (long) region.getPriority() + ((long) priority * factor);
        if (Integer.MAX_VALUE - newValue > 0) {
            return setPriority(src, region, (int) newValue);
        } else {
            sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.warn.region.state.priority.set.invalid", region.getName(), newValue)));
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
    private static int setPriority(ServerCommandSource src, IMarkableRegion region, int priority) {
        CuboidRegion cuboidRegion = (CuboidRegion) region;
        boolean existRegionWithSamePriority = LocalRegions.getIntersectingRegionsFor(cuboidRegion)
                .stream()
                .anyMatch(r -> r.getPriority() == cuboidRegion.getPriority());
        if (existRegionWithSamePriority) {
            MutableText updatePriorityFailMsg = MutableText.of(new TranslatableTextContent("cli.msg.info.region.###.area.update.fail", buildRegionSpatialPropLink(region), buildRegionInfoLink(region)));
            sendCmdFeedback(src, updatePriorityFailMsg);
            return 1;
        } else {
            int oldPriority = region.getPriority();
            if (oldPriority != region.getPriority()) {
                // Priority did not change
                sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.info.region.state.priority.set.value", region.getName(), oldPriority, region.getPriority())));
            } else {
                region.setPriority(priority);
                RegionDataManager.save();
                MutableText updatePriorityMsg = MutableText.of(new TranslatableTextContent("cli.msg.info.region.####.area.update.fail", buildRegionSpatialPropLink(region), buildRegionInfoLink(region)));
                sendCmdFeedback(src, updatePriorityMsg);
            }
            return 0;
        }
    }

    private static int promptRegionInfo(ServerCommandSource src, IMarkableRegion region) {
        // == Region [<name>] overview ==
        sendCmdFeedback(src, MessageUtil.buildRegionOverviewHeader(region));


        // Flags: [n flag(s)][+]
        MutableText regionFlags = MutableText.of(new TranslatableTextContent("cli.msg.info.region.flag"))
                .append(": ")
                .append(buildFlagListLink(region));
        sendCmdFeedback(src, regionFlags);

        // Spatial: [=> Spatial <=]
        MutableText regionSpatialProps = MutableText.of(new TranslatableTextContent("cli.msg.info.region.spatial"))
                .append(": ")
                .append(buildRegionSpatialPropLink(region));
        sendCmdFeedback(src, regionSpatialProps);

        // Affiliations: [owners], [members], [<listAffiliations>]
        MutableText regionAffiliation = MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation"))
                .append(": ")
                .append(buildRegionAffiliationLink(region));
        sendCmdFeedback(src, regionAffiliation);

        // Hierarchy: [parent][-|+], [n children][+]
        MutableText regionHierarchy = MutableText.of(new TranslatableTextContent("cli.msg.info.region.hierarchy"))
                .append(": ")
                .append(buildRegionHierarchyLink(region))
                .append(MutableText.of(new LiteralTextContent(RESET + ", ")))
                .append(buildRegionChildrenLink(region));
        sendCmdFeedback(src, regionHierarchy);

        // State: [=> State <=]
        MutableText regionState = MutableText.of(new TranslatableTextContent("cli.msg.info.region.state"))
                .append(": ")
                .append(buildRegionStateLink(region));
        sendCmdFeedback(src, regionState);
        return 0;
    }

    private static int promptRegionChildren(ServerCommandSource src, IMarkableRegion region) {
        sendCmdFeedback(src, buildRegionChildrenHeader(region));
        Collection<IProtectedRegion> children = region.getChildren().values();
        MutableText childRegionList = MutableText.of(new LiteralTextContent(""));
        if (children.isEmpty()) {
            MutableText noChildrenText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.children.empty", region.getName()));
            childRegionList.append(noChildrenText);
            sendCmdFeedback(src, childRegionList);
        }
        children.forEach(child -> {
            MutableText removeChildLink = MutableText.of(new TranslatableTextContent("cli.msg.info.region.children.remove.link.text.entry",
                    buildRegionRemoveChildLink(region, child), buildRegionInfoLink(child)));
            sendCmdFeedback(src, removeChildLink);
        });
        return 0;
    }

    /**
     * == Affiliation '%s' for '%s'==
     * Players: [n player(s)][+]
     * Teams: [m team(s)][+]
     */
    private static int promptRegionAffiliates(ServerCommandSource src, IMarkableRegion region, String affiliation) {
        PlayerContainer playerContainer;
        switch (affiliation) {
            case "owner":
                playerContainer = region.getOwners();
                break;
            case "member":
                playerContainer = region.getMembers();
                break;
            default:
                playerContainer = null;
                break;
        }
        if (playerContainer == null) {
            return -1;
        }
        MutableText affiliationHeader = buildRegionAffiliationHeader(region, affiliation);
        MutableText players = buildRegionAffiliationPlayerListLink(region, affiliation, playerContainer);
        MutableText teams = buildRegionAffiliationTeamListLink(region, affiliation, playerContainer);
        sendCmdFeedback(src, affiliationHeader);
        sendCmdFeedback(src, players);
        sendCmdFeedback(src, teams);
        return 0;
    }

    private static int promptRegionAffiliationPlayerList(ServerCommandSource src, IMarkableRegion region, String affiliation) {
        sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.player.list", buildRegionInfoLink(region), affiliation)));
        Set<String> playerNames = getAssociateList((AbstractRegion) region, affiliation, "player");
        MutableText playerList = MutableText.of(new LiteralTextContent(""));
        if (playerNames.isEmpty()) {
            MutableText noPlayersText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.player.empty", affiliation, region.getName()));
            playerList.append(noPlayersText);
            sendCmdFeedback(src, playerList);
        }
        playerNames.forEach(playerName -> {
            MutableText removePlayerLink = MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.player.remove.link.text.entry",
                    buildRegionRemovePlayerLink(region, playerName, affiliation), playerName));
            sendCmdFeedback(src, removePlayerLink);
        });
        return 0;
    }

    private static int promptRegionAffiliationTeamList(ServerCommandSource src, IMarkableRegion region, String affiliation) {
        sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.team.list", buildRegionInfoLink(region), affiliation)));
        Set<String> teamNames = getAssociateList((AbstractRegion) region, affiliation, "team");
        MutableText teamList = MutableText.of(new LiteralTextContent(""));
        if (teamNames.isEmpty()) {
            MutableText noTeamText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.team.empty",
                    affiliation, region.getName()));
            teamList.append(noTeamText);
            sendCmdFeedback(src, teamList);
        }
        teamNames.forEach(teamName -> {
            MutableText removeTeamLink = MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.team.remove.link.text.entry",
                    buildRegionRemoveTeamLink(region, teamName, affiliation), teamName));
            sendCmdFeedback(src, removeTeamLink);
        });
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
    public static int promptRegionSpatialProperties(ServerCommandSource src, IMarkableRegion region) {
        sendCmdFeedback(src, buildRegionSpatialHeader(region));
        sendCmdFeedback(src, buildRegionLocationComponent(region));
        sendCmdFeedback(src, buildRegionAreaComponent(region));
        return 0;
    }

    /**
     * Prompt the region state to the command issuer.
     * == Region [<name>] state ==
     * Enabled: [true|false]
     * Priority: n [#][+5][-5]
     * Alert: [on|off]
     *
     * @param src
     * @param region
     * @return
     */
    public static int promptRegionState(ServerCommandSource src, IMarkableRegion region) {
        sendCmdFeedback(src, buildRegionStateHeader(region));
        sendCmdFeedback(src, composeRegionPriorityComponent(region));
        sendCmdFeedback(src, composeRegionEnableComponent(region));
        sendCmdFeedback(src, composeRegionAlertComponent(region));
        return 0;
    }

    public static int promptRegionFlags(ServerCommandSource src, IMarkableRegion region) {
        sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.info.region.flag.header", buildRegionInfoLink(region))));
        if (region.getFlags().isEmpty()) {
            sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.info.region.flag.empty", region.getName())));
            return 1;
        }
        List<IFlag> activeFlags = region.getFlags().stream()
                .filter(IFlag::isActive)
                .sorted()
                .collect(Collectors.toList());
        List<IFlag> inActiveFlags = region.getFlags().stream()
                .filter(f -> !f.isActive())
                .sorted()
                .collect(Collectors.toList());
        activeFlags.addAll(inActiveFlags);
        List<IFlag> flags = new ArrayList<>(activeFlags);
        flags.addAll(inActiveFlags);
        flags.forEach(flag -> {
            MutableText removeFlagEntry = MutableText.of(new LiteralTextContent(" - "))
                    .append(buildRemoveFlagLink(flag, region))
                    .append(MutableText.of(new LiteralTextContent(" '" + flag.getFlagIdentifier() + "'")));
            sendCmdFeedback(src, removeFlagEntry);
        });
        return 0;
    }

    // TODO: Only with region marker
    // assumption: regions are only updated with the region marker when in the same dimension
    private static int updateRegion(ServerCommandSource src, IMarkableRegion region) {
        try {
            PlayerEntity player = src.getPlayerOrThrow();
            ItemStack maybeStick = player.getMainHandStack();
            if (StickUtil.isVanillaStick(maybeStick)) {
                try {
                    AbstractStick abstractStick = StickUtil.getStick(maybeStick);
                    if (abstractStick.getStickType() == StickType.MARKER) {
                        MarkerStick marker = (MarkerStick) abstractStick;
                        // TODO: RegionDataManager.get().update(regionName, marker);
                    }
                } catch (StickException e) {
                    sendCmdFeedback(src, "ServerCommandSource is not player. Aborting.. Needs RegionMarker with Block-NBT data in player hand");
                }
            }
        } catch (CommandSyntaxException e) {
            e.printStackTrace();
        }
        return 0;
    }

    // TODO
    private static int listRegionsAround(ServerCommandSource source) {

        return 0;
    }

    private static int teleport(ServerCommandSource src, IMarkableRegion region) {
        try {
            ServerPlayerEntity player = src.getPlayerOrThrow();
            src.getServer().getCommandManager().getDispatcher().execute(buildRegionTpCmd(region, player.getEntityName()), src);
            return 0;
        } catch (CommandSyntaxException e) {
            YetAnotherWorldProtector.LOGGER.warn("Unable to teleport command source to region.");
            return -1;
        }
    }

    private static int teleport(ServerCommandSource src, IMarkableRegion region, PlayerEntity player) {
        try {
            src.getServer().getCommandManager().getDispatcher().execute(buildRegionTpCmd(region, player.getEntityName()), src);
            return 0;
        } catch (CommandSyntaxException e) {
            YetAnotherWorldProtector.LOGGER.warn("Error executing teleport command.");
            // TODO: error executing tp command
            return -1;
        }
    }

    private static int setTeleportPos(ServerCommandSource src, IMarkableRegion region, BlockPos target) {
        if (!region.getTpTarget().equals(target)) {
            region.setTpTarget(target);
            RegionDataManager.save();
            MutableText newTpTargetLink = buildDimensionalBlockTpLink(region.getDim(), target);
            sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.info.region.spatial.location.teleport.set", region.getName(), newTpTargetLink)));
            return 0;
        }
        return 1;
    }
}
