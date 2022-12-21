package de.z0rdak.yawp.commands;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.builder.RequiredArgumentBuilder;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.commands.arguments.flag.RegionFlagArgumentType;
import de.z0rdak.yawp.commands.arguments.region.RegionArgumentType;
import de.z0rdak.yawp.config.server.RegionConfig;
import de.z0rdak.yawp.core.affiliation.PlayerContainer;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.flag.BooleanFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.*;
import de.z0rdak.yawp.core.stick.AbstractStick;
import de.z0rdak.yawp.core.stick.MarkerStick;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.LocalRegions;
import de.z0rdak.yawp.util.StickException;
import de.z0rdak.yawp.util.StickType;
import de.z0rdak.yawp.util.StickUtil;
import net.minecraft.command.CommandSource;
import net.minecraft.command.Commands;
import net.minecraft.command.ISuggestionProvider;
import net.minecraft.command.arguments.BlockPosArgument;
import net.minecraft.command.arguments.DimensionArgument;
import net.minecraft.command.arguments.EntityArgument;
import net.minecraft.command.arguments.TeamArgument;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.scoreboard.ScorePlayerTeam;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.text.IFormattableTextComponent;
import net.minecraft.util.text.StringTextComponent;
import net.minecraft.util.text.TextFormatting;
import net.minecraft.util.text.TranslationTextComponent;
import net.minecraft.world.World;

import java.util.*;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.util.CommandUtil.*;
import static de.z0rdak.yawp.util.MessageUtil.*;

public class RegionCommands {

    public static final LiteralArgumentBuilder<CommandSource> REGION_COMMAND = registerRegionCommands();
    public static Map<CommandSource, RegistryKey<World>> CommandSourceReferenceDims = new HashMap<>();

    private RegionCommands() {
    }

    public static LiteralArgumentBuilder<CommandSource> registerRegionCommands() {
        return literal(REGION)
                .then(Commands.argument(DIMENSION.toString(), DimensionArgument.dimension())
                        .then(regionCommands()));
    }

    /**
     * TODO: Command to invert enable and alert based on region state
     *
     * @return
     */
    private static RequiredArgumentBuilder<CommandSource, String> regionCommands() {
        List<String> affiliationList = Arrays.asList("member", "owner");
        return Commands.argument(REGION.toString(), StringArgumentType.word())
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
                                .then(Commands.argument(ALERT.toString(), BoolArgumentType.bool())
                                        .executes(ctx -> setAlertState(ctx.getSource(), getRegionArgument(ctx), getAlertArgument(ctx)))))
                        .then(literal(ENABLE)
                                // TODO: add default true and toggle cmd
                                .then(Commands.argument(ENABLE.toString(), BoolArgumentType.bool())
                                        .executes(ctx -> setEnableState(ctx.getSource(), getRegionArgument(ctx), getEnableArgument(ctx)))))
                        .then(literal(PRIORITY)
                                .then(Commands.argument(PRIORITY.toString(), IntegerArgumentType.integer())
                                        .executes(ctx -> setPriority(ctx.getSource(), getRegionArgument(ctx), getPriorityArgument(ctx))))
                                .then(literal(INC)
                                        .then(Commands.argument(PRIORITY.toString(), IntegerArgumentType.integer())
                                                .executes(ctx -> setPriority(ctx.getSource(), getRegionArgument(ctx), getPriorityArgument(ctx), 1))))
                                .then(literal(DEC)
                                        .then(Commands.argument(PRIORITY.toString(), IntegerArgumentType.integer())
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
                        .then(Commands.literal(AreaType.CUBOID.areaType)
                                .then(Commands.argument("pos1", BlockPosArgument.blockPos())
                                        .then(Commands.argument("pos2", BlockPosArgument.blockPos())
                                                .executes(ctx -> updateArea(ctx.getSource(), getRegionArgument(ctx), AreaType.CUBOID,
                                                        BlockPosArgument.getOrLoadBlockPos(ctx, "pos1"),
                                                        BlockPosArgument.getOrLoadBlockPos(ctx, "pos2")))))
                        ))
                // TODO: Only with marker
                //.then(literal(UPDATE)
                //        .then(Commands.argument(AREA.toString(), StringArgumentType.word())
                //                .suggests((ctx, builder) -> AreaArgumentType.areaType().listSuggestions(ctx, builder))
                //                .executes(ctx -> updateRegion(ctx.getSource(), getRegionArgument(ctx)))))
                .then(literal(ADD)
                        .then(literal(CommandConstants.PLAYER)
                                .then(Commands.argument(CommandConstants.AFFILIATION.toString(), StringArgumentType.word())
                                        .suggests((ctx, builder) -> ISuggestionProvider.suggest(affiliationList, builder))
                                        .then(Commands.argument(CommandConstants.PLAYER.toString(), EntityArgument.player())
                                                .executes(ctx -> addPlayer(ctx.getSource(), getPlayerArgument(ctx), getRegionArgument(ctx), getAffiliationArgument(ctx)))))
                                .then(Commands.argument(CommandConstants.AFFILIATION.toString(), StringArgumentType.word())
                                        .suggests((ctx, builder) -> ISuggestionProvider.suggest(affiliationList, builder))
                                        .then(Commands.argument(CommandConstants.PLAYER.toString(), EntityArgument.player())
                                                .executes(ctx -> addPlayer(ctx.getSource(), getPlayerArgument(ctx), getRegionArgument(ctx), getAffiliationArgument(ctx))))))
                        .then(literal(CommandConstants.TEAM)
                                .then(Commands.argument(CommandConstants.AFFILIATION.toString(), StringArgumentType.word())
                                        .suggests((ctx, builder) -> ISuggestionProvider.suggest(affiliationList, builder))
                                        .then(Commands.argument(CommandConstants.TEAM.toString(), TeamArgument.team())
                                                .executes(ctx -> addTeam(ctx.getSource(), getTeamArgument(ctx), getRegionArgument(ctx), getAffiliationArgument(ctx)))))
                                .then(Commands.argument(CommandConstants.AFFILIATION.toString(), StringArgumentType.word())
                                        .suggests((ctx, builder) -> ISuggestionProvider.suggest(affiliationList, builder))
                                        .then(Commands.argument(CommandConstants.TEAM.toString(), TeamArgument.team())
                                                .executes(ctx -> addTeam(ctx.getSource(), getTeamArgument(ctx), getRegionArgument(ctx), getAffiliationArgument(ctx))))))
                        .then(literal(FLAG)
                                .then(Commands.argument(FLAG.toString(), StringArgumentType.word())
                                        // TODO: Suggest only flags not present in the region
                                        .suggests((ctx, builder) -> RegionFlagArgumentType.flag().listSuggestions(ctx, builder))
                                        .executes(ctx -> addFlag(ctx.getSource(), getRegionArgument(ctx), getFlagArgument(ctx)))))
                        .then(literal(CHILD)
                                .then(Commands.argument(CHILD.toString(), StringArgumentType.word())
                                        // FIXME: Only list region which are able to be child regions
                                        // TODO: Introduce dedicated RegionChildArgumentType for this
                                        .suggests((ctx, builder) -> RegionArgumentType.region().listSuggestions(ctx, builder))
                                        .executes(ctx -> addChildren(ctx.getSource(), getRegionArgument(ctx), getChildRegionArgument(ctx))))))
                .then(literal(REMOVE)
                        .then(literal(CommandConstants.PLAYER)
                                .then(Commands.argument(CommandConstants.AFFILIATION.toString(), StringArgumentType.word())
                                        .suggests((ctx, builder) -> ISuggestionProvider.suggest(affiliationList, builder))
                                        .then(Commands.argument(CommandConstants.PLAYER.toString(), EntityArgument.player())
                                                .executes(ctx -> removePlayer(ctx.getSource(), getPlayerArgument(ctx), getRegionArgument(ctx), getAffiliationArgument(ctx)))))
                                .then(Commands.argument(CommandConstants.AFFILIATION.toString(), StringArgumentType.word())
                                        .suggests((ctx, builder) -> ISuggestionProvider.suggest(affiliationList, builder))
                                        .then(Commands.argument(CommandConstants.PLAYER.toString(), EntityArgument.player())
                                                .executes(ctx -> removePlayer(ctx.getSource(), getPlayerArgument(ctx), getRegionArgument(ctx), getAffiliationArgument(ctx))))))
                        .then(literal(CommandConstants.TEAM)
                                .then(Commands.argument(CommandConstants.AFFILIATION.toString(), StringArgumentType.word())
                                        .suggests((ctx, builder) -> ISuggestionProvider.suggest(affiliationList, builder))
                                        .then(Commands.argument(CommandConstants.TEAM.toString(), TeamArgument.team())
                                                .executes(ctx -> removeTeam(ctx.getSource(), getTeamArgument(ctx), getRegionArgument(ctx), getAffiliationArgument(ctx)))))
                                .then(Commands.argument(CommandConstants.AFFILIATION.toString(), StringArgumentType.word())
                                        .suggests((ctx, builder) -> ISuggestionProvider.suggest(affiliationList, builder))
                                        .then(Commands.argument(CommandConstants.TEAM.toString(), TeamArgument.team())
                                                .executes(ctx -> removeTeam(ctx.getSource(), getTeamArgument(ctx), getRegionArgument(ctx), getAffiliationArgument(ctx))))))
                        .then(literal(FLAG)
                                .then(Commands.argument(FLAG.toString(), StringArgumentType.word())
                                        // TODO: Suggest only flags present in the region
                                        .suggests((ctx, builder) -> RegionFlagArgumentType.flag().listSuggestions(ctx, builder))
                                        .executes(ctx -> removeFlag(ctx.getSource(), getRegionArgument(ctx), getFlagArgument(ctx)))))
                        .then(literal(CHILD)
                                .then(Commands.argument(CHILD.toString(), StringArgumentType.word())
                                        // FIXME: Only list region which are able to be child regions
                                        // TODO: Introduce dedicated RegionChildArgumentType for this
                                        .suggests((ctx, builder) -> RegionArgumentType.region().listSuggestions(ctx, builder))
                                        .executes(ctx -> removeChildren(ctx.getSource(), getRegionArgument(ctx), getChildRegionArgument(ctx))))))
                .then(literal(PARENT)
                        .then(literal(SET)
                                // FIXME: Only list region which are able to be parent regions
                                // TODO: Introduce dedicated RegionParentArgumentType for this
                                .then(Commands.argument(PARENT_REGION.toString(), StringArgumentType.word())
                                        .suggests((ctx, builder) -> RegionArgumentType.region().listSuggestions(ctx, builder))
                                        .executes(ctx -> setRegionParent(ctx.getSource(), RegionArgumentType.getRegion(ctx, REGION.toString()), RegionArgumentType.getRegion(ctx, PARENT_REGION.toString())))))
                        .then(literal(CLEAR)
                                .executes(ctx -> clearRegionParent(ctx.getSource(), RegionArgumentType.getRegion(ctx, REGION.toString())))))
                .then(literal(TELEPORT)
                        .executes(ctx -> teleport(ctx.getSource(), getRegionArgument(ctx)))
                        .then(Commands.argument(PLAYER.toString(), EntityArgument.player())
                                .executes(ctx -> teleport(ctx.getSource(), getRegionArgument(ctx), getPlayerArgument(ctx))))
                        .then(Commands.literal(SET.toString())
                                .then(Commands.argument(TARGET.toString(), BlockPosArgument.blockPos())
                                        .executes(ctx -> setTeleportPos(ctx.getSource(), getRegionArgument(ctx), BlockPosArgument.getOrLoadBlockPos(ctx, TARGET.toString()))))));
    }

    private static int updateArea(CommandSource src, IMarkableRegion region, AreaType areaType, BlockPos pos1, BlockPos pos2) {
        TranslationTextComponent updateAreaMsg = new TranslationTextComponent("cli.msg.info.region.spatial.area.update", buildRegionSpatialPropLink(region), buildRegionInfoLink(region));
        IProtectedRegion parent = region.getParent();
        switch (areaType) {
            case CUBOID:
                CuboidArea newArea = new CuboidArea(pos1, pos2);
                if (parent instanceof DimensionalRegion) {
                    CuboidRegion cuboidRegion = (CuboidRegion) region;
                    List<CuboidRegion> intersectionRegions = LocalRegions.getIntersectingRegionsFor(cuboidRegion, parent);
                    LocalRegions.calcNewPriorityFor(cuboidRegion, intersectionRegions, RegionConfig.DEFAULT_REGION_PRIORITY.get());
                }
                if (parent instanceof IMarkableRegion) {
                    CuboidRegion cuboidRegion = (CuboidRegion) region;
                    CuboidArea cuboidArea = (CuboidArea) cuboidRegion.getArea();
                    IMarkableRegion localParentRegion = (IMarkableRegion) parent;
                    // FIXME: This only work currently because we only have CuboidAreas
                    CuboidArea parentArea = (CuboidArea) localParentRegion.getArea();
                    if(parentArea.contains(cuboidArea)) {
                        List<CuboidRegion> intersectionRegions = LocalRegions.getIntersectingRegionsFor(cuboidRegion, localParentRegion);
                        LocalRegions.calcNewPriorityFor(cuboidRegion, intersectionRegions, localParentRegion.getPriority() + 1);
                    } else {
                        TranslationTextComponent updateAreaFailMsg = new TranslationTextComponent("cli.msg.info.region.spatial.area.update.fail", buildRegionSpatialPropLink(region), buildRegionInfoLink(region));
                        sendCmdFeedback(src, updateAreaFailMsg);
                        return 1;
                    }
                }
                region.setArea(newArea);
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

    private static int removeTeam(CommandSource src, ScorePlayerTeam team, IMarkableRegion region, String affiliation) {
        switch (affiliation) {
            case "member":
                if (!region.getMembers().containsTeam(team)) {
                    region.removeOwner(team);
                    RegionDataManager.save();
                    sendCmdFeedback(src, new TranslationTextComponent("cli.msg.info.region.affiliation.team.removed", team.getName(), region.getName()));
                }
                break;
            case "owner":
                if (!region.getOwners().containsTeam(team)) {
                    region.removeOwner(team);
                    RegionDataManager.save();
                    sendCmdFeedback(src, new TranslationTextComponent("cli.msg.info.region.affiliation.team.removed", team.getName(), region.getName()));
                }
                break;
            default:
                // TODO Create new affiliation with no permissions?
                return 1;
        }
        return 0;
    }

    private static int addTeam(CommandSource src, ScorePlayerTeam team, IMarkableRegion region, String affiliation) {
        switch (affiliation) {
            case "member":
                if (!region.getMembers().containsTeam(team)) {
                    region.addMember(team);
                    RegionDataManager.save();
                    sendCmdFeedback(src, new TranslationTextComponent("cli.msg.info.region.affiliation.team.added", team.getName(), affiliation, region.getName()));
                }
                break;
            case "owner":
                if (!region.getOwners().containsTeam(team)) {
                    region.addOwner(team);
                    RegionDataManager.save();
                    sendCmdFeedback(src, new TranslationTextComponent("cli.msg.info.region.affiliation.team.added", team.getName(), affiliation, region.getName()));
                }
                break;
            default:
                // TODO Create new affiliation with no permissions?
                return 1;
        }
        return 0;
    }

    private static int removePlayer(CommandSource src, ServerPlayerEntity player, IMarkableRegion region, String affiliation) {
        switch (affiliation) {
            case "member":
                if (!region.getMembers().containsPlayer(player.getUUID())) {
                    region.removeMember(player);
                    RegionDataManager.save();
                    sendCmdFeedback(src, new TranslationTextComponent("cli.msg.info.region.affiliation.player.removed", player.getScoreboardName(), region.getName()));
                }
                break;
            case "owner":
                if (!region.getOwners().containsPlayer(player.getUUID())) {
                    region.removeOwner(player);
                    RegionDataManager.save();
                    sendCmdFeedback(src, new TranslationTextComponent("cli.msg.info.region.affiliation.player.removed", player.getScoreboardName(), region.getName()));
                }
                break;
            default:
                // TODO Create new affiliation with no permissions?
                return 1;
        }
        return 0;
    }

    private static int addPlayer(CommandSource src, ServerPlayerEntity player, IMarkableRegion region, String affiliation) {
        switch (affiliation) {
            case "member":
                if (!region.getMembers().containsPlayer(player.getUUID())) {
                    region.addMember(player);
                    RegionDataManager.save();
                    sendCmdFeedback(src, new TranslationTextComponent("cli.msg.info.region.affiliation.player.added", player.getScoreboardName(), affiliation, region.getName()));
                }
                break;
            case "owner":
                if (!region.getOwners().containsPlayer(player.getUUID())) {
                    region.addOwner(player);
                    RegionDataManager.save();
                    sendCmdFeedback(src, new TranslationTextComponent("cli.msg.info.region.affiliation.player.added", player.getScoreboardName(), affiliation, region.getName()));
                }
                break;
            default:
                // TODO Create new affiliation with no permissions?
                return 1;
        }
        return 0;
    }

    private static int removeChildren(CommandSource src, IMarkableRegion region, IMarkableRegion child) {
        if (region.hasChild(child)) {
            region.removeChild(child);
            RegionDataManager.save();
            sendCmdFeedback(src, new TranslationTextComponent("cli.msg.info.region.children.remove", child.getName(), region.getName()));
            sendCmdFeedback(src, new TranslationTextComponent("cli.msg.info.region.parent.clear", child.getName()));
            return 0;
        }
        return -1;
    }

    private static int addChildren(CommandSource src, IMarkableRegion region, IMarkableRegion child) {
        if (!region.hasChild(child)) {
            region.addChild(child);
            RegionDataManager.save();
            sendCmdFeedback(src, new TranslationTextComponent("cli.msg.info.region.children.add", child.getName(), region.getName()));
            return 0;
        }
        return -1;
    }

    private static int clearRegionParent(CommandSource src, IMarkableRegion region) {
        if (region.getParent() != null) {
            region.setParent(null);
            RegionDataManager.save();
            sendCmdFeedback(src, new TranslationTextComponent("cli.msg.info.region.parent.clear", region.getName()));
        }
        return 0;
    }

    // FIXME: parent can be DimensionalRegion or AbstractMarkableRegion
    private static int setRegionParent(CommandSource src, IMarkableRegion region, IMarkableRegion parent) {
        if (region.getParent() != null) {
            if (!region.getParent().equals(parent)) {
                region.setParent(parent);
                RegionDataManager.save();
                sendCmdFeedback(src, new TranslationTextComponent("cli.msg.info.region.parent.set", region.getName(), parent.getName()));
            }
        } else {
            region.setParent(parent);
            RegionDataManager.save();
            sendCmdFeedback(src, new TranslationTextComponent("cli.msg.info.region.parent.set", region.getName(), parent.getName()));
        }
        return 0;
    }

    // Adds default flag for provided RegionFlag
    private static int addFlag(CommandSource src, IMarkableRegion region, RegionFlag flag) {
        if (!region.containsFlag(flag)) {
            switch (flag.type) {
                case BOOLEAN_FLAG:
                    region.addFlag(new BooleanFlag(flag));
                    break;
                case LIST_FLAG:
                    break;
                case INT_FLAG:
                    break;
            }
            RegionDataManager.save();
            sendCmdFeedback(src, new TranslationTextComponent("cli.msg.flags.added", flag.name, region.getName()));
            return 0;
        }
        return 1;
    }

    private static int removeFlag(CommandSource src, IMarkableRegion region, RegionFlag flag) {
        if (region.containsFlag(flag)) {
            region.removeFlag(flag.name);
            RegionDataManager.save();
            sendCmdFeedback(src, new TranslationTextComponent("cli.msg.flags.removed", flag.name, region.getName()));
            return 0;
        }
        return 1;
    }

    private static int setAlertState(CommandSource src, IMarkableRegion region, boolean mute) {
        boolean oldState = region.isMuted();
        region.setIsMuted(mute);
        RegionDataManager.save();
        if (oldState != region.isMuted()) {
            sendCmdFeedback(src, new TranslationTextComponent("cli.msg.info.region.state.alert.set.value", region.getName(), oldState, region.isMuted()));
        }
        return 0;
    }

    private static int setEnableState(CommandSource src, IMarkableRegion region, boolean enable) {
        boolean oldState = region.isActive();
        region.setIsActive(enable);
        RegionDataManager.save();
        if (oldState != region.isActive()) {
            sendCmdFeedback(src, new TranslationTextComponent("cli.msg.info.region.state.enable.set.value", region.getName(), oldState, region.isActive()));
        }
        return 0;
    }

    private static int setPriority(CommandSource src, IMarkableRegion region, int priority, int factor) {
        long newValue = (long) region.getPriority() + ((long) priority * factor);
        if (Integer.MAX_VALUE - newValue > 0) {
            return setPriority(src, region, (int) newValue);
        } else {
            sendCmdFeedback(src, new TranslationTextComponent("cli.msg.warn.region.state.priority.set.invalid", region.getName(), newValue));
            return -1;
        }
    }

    private static int setPriority(CommandSource src, IMarkableRegion region, int priority) {
        int oldPriority = region.getPriority();
        // TODO: Check priority of other regions in this area and increment/decrement priority if needed

        region.setPriority(priority);
        RegionDataManager.save();
        if (oldPriority != region.getPriority()) {
            sendCmdFeedback(src, new TranslationTextComponent("cli.msg.info.region.state.priority.set.value", region.getName(), oldPriority, region.getPriority()));
        }
        return 0;
    }

    private static int promptRegionInfo(CommandSource src, IMarkableRegion region) {
        // == Region [<name>] overview ==
        IFormattableTextComponent regionInfoHeader = new StringTextComponent(TextFormatting.BOLD + "== Region ")
                .append(buildRegionInfoLink(region))
                .append(new StringTextComponent(TextFormatting.BOLD + " overview =="));
        sendCmdFeedback(src, regionInfoHeader);

        // Flags: [n flag(s)][+]
        IFormattableTextComponent regionFlags = new TranslationTextComponent("cli.msg.info.region.flag")
                .append(": ")
                .append(buildFlagListLink(region));
        sendCmdFeedback(src, regionFlags);

        // Spatial: [=> Spatial <=]
        IFormattableTextComponent regionSpatialProps = new TranslationTextComponent("cli.msg.info.region.spatial")
                .append(": ")
                .append(buildRegionSpatialPropLink(region));
        sendCmdFeedback(src, regionSpatialProps);

        // Affiliations: [owners], [members], [<listAffiliations>]
        IFormattableTextComponent regionAffiliation = new TranslationTextComponent("cli.msg.info.region.affiliation")
                .append(": ")
                .append(buildRegionAffiliationLink(region));
        sendCmdFeedback(src, regionAffiliation);

        // Hierarchy: [parent][-|+], [children][+]
        IFormattableTextComponent regionHierarchy = new TranslationTextComponent("cli.msg.info.region.hierarchy")
                .append(": ")
                .append(buildRegionHierarchyLink(region));
        sendCmdFeedback(src, regionHierarchy);

        // State: [=> State <=]
        IFormattableTextComponent regionState = new TranslationTextComponent("cli.msg.info.region.state")
                .append(": ")
                .append(buildRegionStateLink(region));
        sendCmdFeedback(src, regionState);
        return 0;
    }

    private static int promptRegionChildren(CommandSource src, IMarkableRegion region) {
        sendCmdFeedback(src, buildRegionChildrenHeader(region));
        Collection<IProtectedRegion> children = region.getChildren().values();
        IFormattableTextComponent childRegionList = new StringTextComponent("");
        if (children.isEmpty()) {
            TranslationTextComponent noChildrenText = new TranslationTextComponent("cli.msg.info.region.children.empty", region.getName());
            childRegionList.append(noChildrenText);
            sendCmdFeedback(src, childRegionList);
        }
        children.forEach(child -> {
            TranslationTextComponent removeChildLink = new TranslationTextComponent("cli.msg.info.region.children.remove.link.text.entry",
                    buildRegionRemoveChildLink(region, child), buildRegionInfoLink(child));
            sendCmdFeedback(src, removeChildLink);
        });
        return 0;
    }

    /**
     * == Affiliation '%s' for '%s'==
     * Players: [n player(s)][+]
     * Teams: [m team(s)][+]
     */
    private static int promptRegionAffiliates(CommandSource src, IMarkableRegion region, String affiliation) {
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
        IFormattableTextComponent affiliationHeader = buildRegionAffiliationHeader(region, affiliation);
        IFormattableTextComponent players = buildRegionAffiliationPlayerListLink(region, affiliation, playerContainer);
        IFormattableTextComponent teams = buildRegionAffiliationTeamListLink(region, affiliation, playerContainer);
        sendCmdFeedback(src, affiliationHeader);
        sendCmdFeedback(src, players);
        sendCmdFeedback(src, teams);
        return 0;
    }

    private static int promptRegionAffiliationPlayerList(CommandSource src, IMarkableRegion region, String affiliation) {
        sendCmdFeedback(src, new TranslationTextComponent("cli.msg.info.region.affiliation.player.list", buildRegionInfoLink(region), affiliation));
        Set<String> playerNames = getAssociateList((AbstractRegion) region, affiliation, "player");
        IFormattableTextComponent playerList = new StringTextComponent("");
        if (playerNames.isEmpty()) {
            TranslationTextComponent noPlayersText = new TranslationTextComponent("cli.msg.info.region.affiliation.player.empty", affiliation, region.getName());
            playerList.append(noPlayersText);
            sendCmdFeedback(src, playerList);
        }
        playerNames.forEach(playerName -> {
            TranslationTextComponent removePlayerLink = new TranslationTextComponent("cli.msg.info.region.affiliation.player.remove.link.text.entry",
                    buildRegionRemovePlayerLink(region, playerName, affiliation), playerName);
            sendCmdFeedback(src, removePlayerLink);
        });
        return 0;
    }

    private static int promptRegionAffiliationTeamList(CommandSource src, IMarkableRegion region, String affiliation) {
        sendCmdFeedback(src, new TranslationTextComponent("cli.msg.info.region.affiliation.team.list", buildRegionInfoLink(region), affiliation));
        Set<String> teamNames = getAssociateList((AbstractRegion) region, affiliation, "team");
        IFormattableTextComponent teamList = new StringTextComponent("");
        if (teamNames.isEmpty()) {
            TranslationTextComponent noTeamText = new TranslationTextComponent("cli.msg.info.region.affiliation.team.empty",
                    affiliation, region.getName());
            teamList.append(noTeamText);
            sendCmdFeedback(src, teamList);
        }
        teamNames.forEach(teamName -> {
            TranslationTextComponent removeTeamLink = new TranslationTextComponent("cli.msg.info.region.affiliation.team.remove.link.text.entry",
                    buildRegionRemoveTeamLink(region, teamName, affiliation), teamName);
            sendCmdFeedback(src, removeTeamLink);
        });
        return 0;
    }

    /**
     * Prompt region spatial properties like teleport location and area.
     * == Region [<name>] spatial properties ==
     * Location: [dimInfo]@[tpCoords]
     * Area: [spatialProperties]
     *
     * @param src
     * @param region
     * @return
     */
    public static int promptRegionSpatialProperties(CommandSource src, IMarkableRegion region) {
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
    public static int promptRegionState(CommandSource src, IMarkableRegion region) {
        sendCmdFeedback(src, buildRegionStateHeader(region));
        sendCmdFeedback(src, composeRegionPriorityComponent(region));
        sendCmdFeedback(src, composeRegionEnableComponent(region));
        sendCmdFeedback(src, composeRegionAlertComponent(region));
        return 0;
    }

    public static int promptRegionFlags(CommandSource src, IMarkableRegion region) {
        sendCmdFeedback(src, new TranslationTextComponent("cli.msg.info.region.flag.header", buildRegionInfoLink(region)));
        if (region.getFlags().isEmpty()) {
            sendCmdFeedback(src, new TranslationTextComponent("cli.msg.info.region.flag.empty", region.getName()));
            return 1;
        }
        region.getFlags().stream()
                .sorted()
                .sorted((flag, flag1) -> (flag.isActive() && !flag1.isActive()) ? 1 : !flag.isActive() && flag1.isActive() ? -1 : 0)
                .forEach(flag -> {
                    IFormattableTextComponent removeFlagEntry = new StringTextComponent(" - ")
                            .append(buildRemoveFlagLink(flag, region))
                            .append(new StringTextComponent(" '" + flag.getFlagIdentifier() + "'"));
                    sendCmdFeedback(src, removeFlagEntry);
                });

        /*/
        region.getFlags().forEach(flag -> {
            IFormattableTextComponent removeFlagEntry = new StringTextComponent(" - ")
                    .append(buildRemoveFlagLink(flag, region))
                    .append(new StringTextComponent(" ").withStyle(TextFormatting.RESET))
                    .append(buildFlagInfoLink(flag, region))
                    .append(new StringTextComponent(" ").withStyle(TextFormatting.RESET))
                    .append(buildFlagInfoLinkDetail(flag, region));
            sendCmdFeedback(src, removeFlagEntry);
        });
         */
        return 0;
    }

    // TODO: Only with region marker
    // assumption: regions are only updated with the region marker when in the same dimension
    private static int updateRegion(CommandSource src, IMarkableRegion region) {
        try {
            PlayerEntity player = src.getPlayerOrException();
            ItemStack maybeStick = player.getMainHandItem();
            if (StickUtil.isVanillaStick(maybeStick)) {
                try {
                    AbstractStick abstractStick = StickUtil.getStick(maybeStick);
                    if (abstractStick.getStickType() == StickType.MARKER) {
                        MarkerStick marker = (MarkerStick) abstractStick;
                        // TODO:
                        //RegionDataManager.get().update(regionName, marker);
                    }
                } catch (StickException e) {
                    sendCmdFeedback(src, "CommandSource is not player. Aborting.. Needs RegionMarker with Block-NBT data in player hand");
                }
            }
        } catch (CommandSyntaxException e) {
            e.printStackTrace();
        }
        return 0;
    }

    // TODO
    private static int listRegionsAround(CommandSource source) {

        return 0;
    }

    private static int teleport(CommandSource src, IMarkableRegion region) {
        try {
            ServerPlayerEntity player = src.getPlayerOrException();
            src.getServer().getCommands().getDispatcher().execute(buildRegionTpCmd(region, player.getScoreboardName()), src);
            return 0;
        } catch (CommandSyntaxException e) {
            YetAnotherWorldProtector.LOGGER.warn("Unable to teleport command source to region.");
            return -1;
        }
    }

    private static int teleport(CommandSource src, IMarkableRegion region, PlayerEntity player) {
        try {
            src.getServer().getCommands().getDispatcher().execute(buildRegionTpCmd(region, player.getScoreboardName()), src);
            return 0;
        } catch (CommandSyntaxException e) {
            YetAnotherWorldProtector.LOGGER.warn("Error executing teleport command.");
            // TODO: error executing tp command
            return -1;
        }
    }

    private static int setTeleportPos(CommandSource src, IMarkableRegion region, BlockPos target) {
        if (!region.getTpTarget().equals(target)) {
            region.setTpTarget(target);
            RegionDataManager.save();
            IFormattableTextComponent newTpTargetLink = buildDimensionalBlockTpLink(region.getDim(), target);
            sendCmdFeedback(src, new TranslationTextComponent("cli.msg.info.region.spatial.location.teleport.set", region.getName(), newTpTargetLink));
            return 0;
        }
        return 1;
    }
}
