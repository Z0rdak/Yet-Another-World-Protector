package de.z0rdak.yawp.commands;

import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import de.z0rdak.yawp.commands.arguments.flag.RegionFlagArgumentType;
import de.z0rdak.yawp.config.server.FlagConfig;
import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.flag.BooleanFlag;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.group.GroupType;
import de.z0rdak.yawp.core.region.*;
import de.z0rdak.yawp.handler.flags.HandlerUtil;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.LocalRegions;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.command.CommandSource;
import net.minecraft.command.Commands;
import net.minecraft.command.ISuggestionProvider;
import net.minecraft.command.arguments.EntityArgument;
import net.minecraft.command.arguments.TeamArgument;
import net.minecraft.entity.Entity;
import net.minecraft.entity.item.ExperienceOrbEntity;
import net.minecraft.entity.merchant.villager.WanderingTraderEntity;
import net.minecraft.entity.monster.SlimeEntity;
import net.minecraft.entity.passive.GolemEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.scoreboard.Team;
import net.minecraft.util.text.IFormattableTextComponent;
import net.minecraft.util.text.StringTextComponent;
import net.minecraft.util.text.TextFormatting;
import net.minecraft.util.text.TranslationTextComponent;
import net.minecraft.world.server.ServerWorld;
import org.apache.commons.lang3.NotImplementedException;

import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.commands.arguments.ArgumentUtil.*;
import static de.z0rdak.yawp.util.MessageUtil.*;

public class CommandUtil {

    public final static String MEMBER = "members";
    public final static String OWNER = "owners";
    public final static List<String> GROUP_LIST = Arrays.asList(MEMBER, OWNER);

    public static LiteralArgumentBuilder<CommandSource> buildClearSubCommand(Function<CommandContext<CommandSource>, IProtectedRegion> regionSupplier, RegionType regionType) {
        return literal(CLEAR)
                .then(literal(FLAGS)
                        .executes(ctx -> CommandUtil.clearFlags(ctx, regionSupplier.apply(ctx), regionType))
                )
                .then(literal(PLAYERS)
                        .executes(ctx -> CommandUtil.clearPlayers(ctx, regionSupplier.apply(ctx), regionType))
                        .then(Commands.argument(GROUP.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> ISuggestionProvider.suggest(GROUP_LIST, builder))
                                .executes(ctx -> CommandUtil.clearPlayers(ctx, regionSupplier.apply(ctx), getGroupArgument(ctx), regionType)))
                )
                .then(literal(TEAMS)
                        .executes(ctx -> CommandUtil.clearTeams(ctx, regionSupplier.apply(ctx), regionType))
                        .then(Commands.argument(GROUP.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> ISuggestionProvider.suggest(GROUP_LIST, builder))
                                .executes(ctx -> CommandUtil.clearTeams(ctx, regionSupplier.apply(ctx), getGroupArgument(ctx), regionType)))
                )
                .then(literal(GROUP)
                        .then(Commands.argument(GROUP.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> ISuggestionProvider.suggest(GROUP_LIST, builder))
                                .executes(ctx -> CommandUtil.clearAffiliation(ctx, regionSupplier.apply(ctx), getGroupArgument(ctx), regionType)))
                );
    }

    public static LiteralArgumentBuilder<CommandSource> buildRemoveSubCommand(Function<CommandContext<CommandSource>, IProtectedRegion> regionSupplier, RegionType regionType) {
        return literal(REMOVE)
                .then(literal(PLAYER)
                        .then(Commands.argument(GROUP.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> ISuggestionProvider.suggest(GROUP_LIST, builder))
                                .then(Commands.argument(PLAYER.toString(), EntityArgument.players())
                                        .executes(ctx -> removePlayer(ctx, getPlayersArgument(ctx), regionSupplier.apply(ctx), getGroupArgument(ctx), regionType))))
                        .then(Commands.argument(GROUP.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> ISuggestionProvider.suggest(GROUP_LIST, builder))
                                .then(Commands.argument(PLAYER.toString(), EntityArgument.players())
                                        .executes(ctx -> removePlayer(ctx, getPlayersArgument(ctx), regionSupplier.apply(ctx), getGroupArgument(ctx), regionType)))))
                .then(literal(TEAM)
                        .then(Commands.argument(GROUP.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> ISuggestionProvider.suggest(GROUP_LIST, builder))
                                .then(Commands.argument(TEAM.toString(), TeamArgument.team())
                                        .executes(ctx -> removeTeam(ctx, getTeamArgument(ctx), regionSupplier.apply(ctx), getGroupArgument(ctx), regionType))))
                        .then(Commands.argument(GROUP.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> ISuggestionProvider.suggest(GROUP_LIST, builder))
                                .then(Commands.argument(TEAM.toString(), TeamArgument.team())
                                        .executes(ctx -> removeTeam(ctx, getTeamArgument(ctx), regionSupplier.apply(ctx), getGroupArgument(ctx), regionType)))))
                .then(literal(FLAG)
                        .then(Commands.argument(FLAG.toString(), StringArgumentType.greedyString())
                                .suggests((ctx, builder) -> RegionFlagArgumentType.flag().listSuggestions(ctx, builder))
                                .executes(ctx -> removeFlag(ctx, regionSupplier.apply(ctx), getFlagArguments(ctx), regionType))));
    }

    public static LiteralArgumentBuilder<CommandSource> buildAddSubCommand(Function<CommandContext<CommandSource>, IProtectedRegion> regionSupplier, RegionType regionType) {
        return literal(ADD)
                .then(literal(PLAYER)
                        .then(Commands.argument(GROUP.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> ISuggestionProvider.suggest(GROUP_LIST, builder))
                                .then(Commands.argument(PLAYER.toString(), EntityArgument.players())
                                        .executes(ctx -> addPlayer(ctx, getPlayersArgument(ctx), regionSupplier.apply(ctx), getGroupArgument(ctx), regionType))))
                        .then(Commands.argument(GROUP.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> ISuggestionProvider.suggest(GROUP_LIST, builder))
                                .then(Commands.argument(PLAYER.toString(), EntityArgument.players())
                                        .executes(ctx -> addPlayer(ctx, getPlayersArgument(ctx), regionSupplier.apply(ctx), getGroupArgument(ctx), regionType)))))
                .then(literal(TEAM)
                        .then(Commands.argument(GROUP.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> ISuggestionProvider.suggest(GROUP_LIST, builder))
                                .then(Commands.argument(TEAM.toString(), TeamArgument.team())
                                        .executes(ctx -> addTeam(ctx, getTeamArgument(ctx), regionSupplier.apply(ctx), getGroupArgument(ctx), regionType))))
                        .then(Commands.argument(GROUP.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> ISuggestionProvider.suggest(GROUP_LIST, builder))
                                .then(Commands.argument(TEAM.toString(), TeamArgument.team())
                                        .executes(ctx -> addTeam(ctx, getTeamArgument(ctx), regionSupplier.apply(ctx), getGroupArgument(ctx), regionType)))))
                .then(literal(FLAG)
                        .then(Commands.argument(FLAG.toString(), StringArgumentType.greedyString())
                                .suggests((ctx, builder) -> RegionFlagArgumentType.flag().listSuggestions(ctx, builder))
                                .executes(ctx -> addFlag(ctx, regionSupplier.apply(ctx), getFlagArguments(ctx), RegionType.LOCAL))));
    }

    public static LiteralArgumentBuilder<CommandSource> buildListSubCommand(Function<CommandContext<CommandSource>, IProtectedRegion> regionSupplier, RegionType regionType) {
        return literal(LIST)
                .then(literal(FLAG)
                        .executes(ctx -> promptFlagList(ctx, regionSupplier.apply(ctx), 0, regionType))
                        .then(Commands.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                .executes(ctx -> promptFlagList(ctx, regionSupplier.apply(ctx), getPageNoArgument(ctx), regionType))))
                .then(literal(GROUP)
                        .then(Commands.argument(GROUP.toString(), StringArgumentType.string())
                                .suggests((ctx, builder) -> ISuggestionProvider.suggest(RegionCommands.GROUP_LIST, builder))
                                .executes(ctx -> CommandUtil.promptGroupLinks(ctx, regionSupplier.apply(ctx), getGroupArgument(ctx), regionType))
                                .then(literal(TEAM)
                                        .executes(ctx -> CommandUtil.promptGroupList(ctx, regionSupplier.apply(ctx), getGroupArgument(ctx), GroupType.TEAM, 0, regionType))
                                        .then(Commands.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                                .executes(ctx -> CommandUtil.promptGroupList(ctx, regionSupplier.apply(ctx), getGroupArgument(ctx), GroupType.TEAM, getPageNoArgument(ctx), regionType)))
                                )
                                .then(literal(PLAYER)
                                        .executes(ctx -> CommandUtil.promptGroupList(ctx, regionSupplier.apply(ctx), getGroupArgument(ctx), GroupType.PLAYER, 0, regionType))
                                        .then(Commands.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                                .executes(ctx -> CommandUtil.promptGroupList(ctx, regionSupplier.apply(ctx), getGroupArgument(ctx), GroupType.PLAYER, getPageNoArgument(ctx), regionType)))
                                )
                        )
                );
    }

    /**
     * Prompt the common region state to the command issuer.
     * == [state] for [<name>] ==
     * Enabled: [true|false]
     * Alert: [on|off]
     */
    public static void promptRegionState(CommandContext<CommandSource> ctx, IProtectedRegion region, RegionType regionType) {
        sendCmdFeedback(ctx.getSource(), buildHeader(new TranslationTextComponent("cli.msg.info.header.for", buildRegionStateLink(region, regionType), buildRegionInfoLink(region, regionType))));
        sendCmdFeedback(ctx.getSource(), buildInfoComponent("cli.msg.info.region.state.enable", buildRegionEnableComponent(region, regionType)));
        sendCmdFeedback(ctx.getSource(), buildInfoComponent("cli.msg.info.region.state.alert", buildRegionAlertToggleLink(region, regionType)));
    }

    /**
     * == Affiliation '%s' for '%s'==
     * Players: [n player(s)][+]
     * Teams: [m team(s)][+]
     */
    public static int promptGroupLinks(CommandContext<CommandSource> ctx, IProtectedRegion region, String group, RegionType regionType) {
        if (!GROUP_LIST.contains(group)) {
            sendCmdFeedback(ctx.getSource(), new TranslationTextComponent("cli.msg.global.info.group.invalid", group).withStyle(TextFormatting.RED));
            return -1;
        }
        sendCmdFeedback(ctx.getSource(), buildGroupHeader(region, group, regionType));
        sendCmdFeedback(ctx.getSource(), buildGroupPlayerListLink(region, group, regionType));
        sendCmdFeedback(ctx.getSource(), buildGroupTeamListLink(region, group, regionType));
        return 0;
    }

    public static int promptFlagList(CommandContext<CommandSource> ctx, IProtectedRegion region, int pageNo, RegionType regionType) {
        String cmd = "";
        switch (regionType) {
            case GLOBAL:
                cmd = buildCommandStr(GLOBAL.toString(), LIST.toString(), FLAG.toString());
                break;
            case DIMENSION:
                cmd = buildCommandStr(DIM.toString(), region.getName(), LIST.toString(), FLAG.toString());
                break;
            case LOCAL:
                cmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), FLAG.toString());
                break;
        }
        List<IFlag> flags = LocalRegions.getSortedFlags(region);
        if (flags.isEmpty()) {
            sendCmdFeedback(ctx.getSource(), new TranslationTextComponent("cli.msg.info.region.flag.empty", buildRegionInfoLink(region, regionType)));
            return 1;
        }
        List<IFormattableTextComponent> flagPagination = buildPaginationComponents(
                buildRegionFlagInfoHeader(region, regionType),
                cmd, buildRemoveFlagEntries(region, flags, regionType), pageNo,
                new StringTextComponent(" - ").append(buildAddFlagLink(region, regionType)));
        flagPagination.forEach(line -> sendCmdFeedback(ctx.getSource(), line));
        return 0;
    }

    public static int promptGroupList(CommandContext<CommandSource> ctx, IProtectedRegion region, String group, GroupType groupType, int pageNo, RegionType regionType) {
        String cmd = "";
        String dim = region.getDim().location().toString();
        switch (regionType) {
            case GLOBAL:
                cmd = buildCommandStr(GLOBAL.toString(), LIST.toString(), GROUP.toString(), group, groupType.name);
                break;
            case DIMENSION:
                cmd = buildCommandStr(DIM.toString(), dim, LIST.toString(), GROUP.toString(), group, groupType.name);
                break;
            case LOCAL:
                cmd = buildCommandStr(REGION.toString(), dim, region.getName(), LIST.toString(), GROUP.toString(), group, groupType.name);
                break;
        }
        if (!GROUP_LIST.contains(group)) {
            sendCmdFeedback(ctx.getSource(), new TranslationTextComponent("cli.msg.region.info.group.invalid", group).withStyle(TextFormatting.RED));
            return -1;
        }
        List<String> groupNames = getGroupList(region, group, groupType);
        if (groupNames.isEmpty()) {
            sendCmdFeedback(ctx.getSource(), new TranslationTextComponent("cli.msg.info.region.group." + groupType.name + ".empty",
                    group, buildRegionInfoLink(region, regionType)));
            return 1;
        }
        List<IFormattableTextComponent> affiliatePagination = buildPaginationComponents(
                buildGroupHeader(region, group, groupType, regionType),
                cmd, buildRemoveGroupEntries(region, groupNames, groupType, group, regionType),
                pageNo,
                new StringTextComponent(" - ").append(buildAddToGroupLink(region, group, groupType, regionType)));
        affiliatePagination.forEach(line -> sendCmdFeedback(ctx.getSource(), line));
        return 0;
    }

    public static int setActiveState(CommandContext<CommandSource> ctx, IProtectedRegion region, RegionType regionType, boolean activate) {
        region.setIsActive(activate);
        RegionDataManager.save();
        IFormattableTextComponent undoLink = buildRegionActionUndoLink(ctx.getInput(), String.valueOf(!activate), String.valueOf(activate));
        TranslationTextComponent msg = new TranslationTextComponent("cli.msg.info.region.state.enable.set.value",
                buildRegionInfoLink(region, regionType), region.isActive() ? "active" : "inactive");
        sendCmdFeedback(ctx.getSource(), msg.append(" ").append(undoLink));
        return 0;
    }

    public static int setAlertState(CommandContext<CommandSource> ctx, IProtectedRegion region, RegionType regionType, boolean showAlert) {
        boolean oldState = !region.isMuted();
        region.setIsMuted(showAlert);
        RegionDataManager.save();
        if (oldState == region.isMuted()) {
            IFormattableTextComponent undoLink = buildRegionActionUndoLink(ctx.getInput(), String.valueOf(showAlert), String.valueOf(!showAlert));
            TranslationTextComponent msg = new TranslationTextComponent("cli.msg.info.state.alert.set.value",
                    buildRegionInfoLink(region, regionType), region.isMuted() ? "muted" : "active");
            sendCmdFeedback(ctx.getSource(), msg.append(" ").append(undoLink));
        }
        return 0;
    }

    public static int removeTeam(CommandContext<CommandSource> src, Team team, IProtectedRegion region, String group, RegionType regionType) {
        if (!GROUP_LIST.contains(group)) {
            sendCmdFeedback(src.getSource(), new TranslationTextComponent("cli.msg.region.info.group.invalid", group).withStyle(TextFormatting.RED));
            return -1;
        }
        IFormattableTextComponent undoLink = buildRegionActionUndoLink(src.getInput(), REMOVE, ADD);
        IFormattableTextComponent teamInfo = buildAffiliateInfo(region, team.getName(), GroupType.TEAM);
        if (region.getGroup(group).hasTeam(team.getName())) {
            region.removeTeam(team.getName(), group);
            RegionDataManager.save();
            // TODO: group in msg
            TranslationTextComponent msg = new TranslationTextComponent("cli.msg.info.region.group.team.removed", group, teamInfo,
                    buildRegionInfoLink(region, regionType));
            sendCmdFeedback(src.getSource(), msg.append(" ").append(undoLink));
            return 0;
        }
        // TODO: group in msg
        TranslationTextComponent msg = new TranslationTextComponent("cli.msg.info.region.group.team.not-present", group, teamInfo,
                buildRegionInfoLink(region, regionType));
        sendCmdFeedback(src.getSource(), msg);
        return 1;
    }

    public static int removePlayer(CommandContext<CommandSource> ctx, Collection<ServerPlayerEntity> players, IProtectedRegion region, String group, RegionType regionType) {
        players.forEach(player -> CommandUtil.removePlayer(ctx, player, region, regionType, group));
        return 0;
    }

    public static int removePlayer(CommandContext<CommandSource> ctx, String playerName, IProtectedRegion region, String group, RegionType regionType) {
        return CommandUtil.removePlayer(ctx, playerName, region, regionType, group);
    }

    public static int removePlayer(CommandContext<CommandSource> src, PlayerEntity player, IProtectedRegion region, RegionType regionType, String group) {
        if (!GROUP_LIST.contains(group)) {
            sendCmdFeedback(src.getSource(), new TranslationTextComponent("cli.msg.region.info.group.invalid", group).withStyle(TextFormatting.RED));
            return -1;
        }
        IFormattableTextComponent undoLink = buildRegionActionUndoLink(src.getInput(), REMOVE, ADD);
        IFormattableTextComponent playerInfo = buildAffiliateInfo(region, player.getScoreboardName(), GroupType.PLAYER);
        if (region.getGroup(group).hasPlayer(player.getUUID())) {
            region.removePlayer(player.getUUID(), group);
            // TODO: group in msg
            TranslationTextComponent msg = new TranslationTextComponent("cli.msg.info.region.group.player.removed", group, playerInfo,
                    buildRegionInfoLink(region, regionType));
            sendCmdFeedback(src.getSource(), msg.append(" ").append(undoLink));
            RegionDataManager.save();
            return 0;
        }
        // TODO: group in msg
        TranslationTextComponent msg = new TranslationTextComponent("cli.msg.info.region.group.player.not-present", group, playerInfo,
                buildRegionInfoLink(region, regionType));
        sendCmdFeedback(src.getSource(), msg);
        return 1;
    }

    public static int removePlayer(CommandContext<CommandSource> ctx, String playerName, IProtectedRegion region, RegionType regionType, String group) {
        // TODO
        return -1;
    }

    public static int removePlayer(CommandContext<CommandSource> ctx, UUID playerUuid, IProtectedRegion region, RegionType regionType, String group) {
        // TODO
        return -1;
    }

    public static int addPlayer(CommandContext<CommandSource> ctx, Collection<ServerPlayerEntity> players, IProtectedRegion region, String group, RegionType regionType) {
        players.forEach(player -> CommandUtil.addPlayer(ctx, player, region, regionType, group));
        return 0;
    }

    public static int addPlayer(CommandContext<CommandSource> src, PlayerEntity player, IProtectedRegion region, RegionType regionType, String group) {
        if (!GROUP_LIST.contains(group)) {
            sendCmdFeedback(src.getSource(), new TranslationTextComponent("cli.msg.region.info.group.invalid", group).withStyle(TextFormatting.RED));
            return -1;
        }
        IFormattableTextComponent undoLink = buildRegionActionUndoLink(src.getInput(), ADD, REMOVE);
        if (!region.hasPlayer(player.getUUID(), group)) {
            region.addPlayer(player, group);
            RegionDataManager.save();
            // TODO: group in msg
            TranslationTextComponent msg = new TranslationTextComponent("cli.msg.info.region.group.player.added",
                    player.getScoreboardName(), group, buildRegionInfoLink(region, regionType));
            sendCmdFeedback(src.getSource(), msg.append(" ").append(undoLink));
            return 0;
        }
        // TODO: group in msg
        TranslationTextComponent msg = new TranslationTextComponent("cli.msg.info.region.group.player.present",
                player.getScoreboardName(), buildRegionInfoLink(region, regionType));
        sendCmdFeedback(src.getSource(), msg);
        return 1;
    }

    public static int addTeam(CommandContext<CommandSource> ctx, Team team, IProtectedRegion region, String group, RegionType regionType) {
        if (!GROUP_LIST.contains(group)) {
            sendCmdFeedback(ctx.getSource(), new TranslationTextComponent("cli.msg.region.info.group.invalid", group).withStyle(TextFormatting.RED));
            return -1;
        }
        IFormattableTextComponent teamHoverInfo = buildTeamHoverComponent(team);
        if (!region.hasTeam(team.getName(), group)) {
            region.addTeam(team.getName(), group);
            RegionDataManager.save();
            IFormattableTextComponent undoLink = buildRegionActionUndoLink(ctx.getInput(), ADD, REMOVE);
            // TODO: group in msg
            TranslationTextComponent msg = new TranslationTextComponent("cli.msg.info.region.group.team.added",
                    teamHoverInfo, region.getDim().location().toString(), group);
            sendCmdFeedback(ctx.getSource(), msg.append(" ").append(undoLink));
            return 0;
        }
        // TODO: group in msg
        TranslationTextComponent msg = new TranslationTextComponent("cli.msg.info.region.group.team.present",
                teamHoverInfo, buildRegionInfoLink(region, regionType), group);
        sendCmdFeedback(ctx.getSource(), msg);
        return 1;
    }

    public static int removeFlag(CommandContext<CommandSource> ctx, IProtectedRegion region, Set<RegionFlag> flags, RegionType regionType) {
        flags.forEach(flag -> CommandUtil.removeRegionFlag(ctx, region, regionType, flag));
        return 0;
    }


    public static int removeRegionFlag(CommandContext<CommandSource> ctx, IProtectedRegion region, RegionType regionType, RegionFlag flag) {
        if (region.containsFlag(flag)) {
            region.removeFlag(flag.name);
            RegionDataManager.save();
            IFormattableTextComponent msg = new TranslationTextComponent("cli.msg.flag.removed", flag.name,
                    buildRegionInfoLink(region, regionType));
            IFormattableTextComponent undoLink = buildRegionActionUndoLink(ctx.getInput(), REMOVE, ADD);
            sendCmdFeedback(ctx.getSource(), msg.append(" ").append(undoLink));
            return 0;
        } else {
            IFormattableTextComponent msg = new TranslationTextComponent("cli.msg.flag.not-present", flag.name,
                    buildRegionInfoLink(region, regionType));
            sendCmdFeedback(ctx.getSource(), msg);
            return 1;
        }
    }

    // TODO: Lang keys
    public static int clearFlags(CommandContext<CommandSource> ctx, IProtectedRegion region, RegionType regionType) {
        int amount = region.getFlags().size();
        if (amount == 0) {
            IFormattableTextComponent feedbackMsg = new TranslationTextComponent("cli.msg.region.info.flags.empty", buildRegionInfoLink(region, regionType));
            sendCmdFeedback(ctx.getSource(), feedbackMsg);
            return 1;
        }
        Collection<IFlag> flags = region.getFlags();
        // TODO: Undo action - this would require a addFlag command which allows to add multiple
        region.getFlags().clear();
        IFormattableTextComponent feedbackMsg = new TranslationTextComponent("cli.msg.info.flags.cleared", buildRegionInfoLink(region, regionType), amount);
        sendCmdFeedback(ctx.getSource(), feedbackMsg);
        RegionDataManager.save();
        return 0;
    }

    public static int clearPlayers(CommandContext<CommandSource> ctx, IProtectedRegion region, RegionType regionType) {
        return clearPlayers(ctx, region, MEMBER, regionType) + clearPlayers(ctx, region, OWNER, regionType);
    }

    public static int clearPlayers(CommandContext<CommandSource> ctx, IProtectedRegion region, String groupName, RegionType regionType) {
        // TODO: Undo action - this would require a addplayer command which allows to add multiple (offline)
        int amount = region.getGroup(groupName).getPlayers().size();
        if (amount == 0) {
            IFormattableTextComponent feedbackMsg = new TranslationTextComponent("cli.msg.region.info.players.empty", buildRegionInfoLink(region, regionType), groupName);
            sendCmdFeedback(ctx.getSource(), feedbackMsg);
            return 1;
        }
        region.getGroup(groupName).clearPlayers();
        IFormattableTextComponent feedbackMsg = new TranslationTextComponent("cli.msg.region.info.players.cleared", buildRegionInfoLink(region, regionType), amount, groupName);
        sendCmdFeedback(ctx.getSource(), feedbackMsg);
        RegionDataManager.save();
        return 0;
    }

    public static int clearTeams(CommandContext<CommandSource> ctx, IProtectedRegion region, RegionType regionType) {
        return clearPlayers(ctx, region, MEMBER, regionType) + clearPlayers(ctx, region, OWNER, regionType);
    }

    public static int clearTeams(CommandContext<CommandSource> ctx, IProtectedRegion region, String groupName, RegionType regionType) {
        // TODO: Undo action
        int amount = region.getGroup(groupName).getTeams().size();
        if (amount == 0) {
            IFormattableTextComponent feedbackMsg = new TranslationTextComponent("cli.msg.region.info.teams.empty", buildRegionInfoLink(region, regionType), groupName);
            sendCmdFeedback(ctx.getSource(), feedbackMsg);
            return 1;
        }
        region.getGroup(groupName).clearTeams();
        IFormattableTextComponent feedbackMsg = new TranslationTextComponent("cli.msg.region.info.teams.cleared", buildRegionInfoLink(region, regionType), amount, groupName);
        sendCmdFeedback(ctx.getSource(), feedbackMsg);
        RegionDataManager.save();
        return 0;
    }


    public static int clearAffiliation(CommandContext<CommandSource> ctx, IProtectedRegion region, String groupName, RegionType regionType) {
        return CommandUtil.clearTeams(ctx, region, groupName, regionType) + CommandUtil.clearPlayers(ctx, region, groupName, regionType);
    }

    public static int addFlag(CommandContext<CommandSource> ctx, IProtectedRegion region, Set<RegionFlag> flags, RegionType regionType) {
        flags.forEach(flag -> CommandUtil.addRegionFlag(ctx, region, flag, regionType));
        return 0;
    }

    public static int addRegionFlag(CommandContext<CommandSource> ctx, IProtectedRegion region, RegionFlag flag, RegionType regionType) {
        if (!region.containsFlag(flag)) {
            IFlag iFlag;
            switch (flag.type) {
                case BOOLEAN_FLAG:
                    iFlag = new BooleanFlag(flag);
                    break;
                case LIST_FLAG:
                case INT_FLAG:
                    throw new NotImplementedException("Not yet implemented = " + flag.name);
                default:
                    throw new IllegalArgumentException("Unexpected value = " + flag.getClass().getName());
            }
            // TODO: More general: Trigger for adding flags?
            if (flag.name.contains("spawning") && FlagConfig.removeEntitiesEnabled()) {
                removeInvolvedEntities(ctx, region, flag);
            }
            region.addFlag(iFlag);
            RegionDataManager.save();
            IFormattableTextComponent flagLink = buildFlagInfoLink(region, iFlag, regionType);
            IFormattableTextComponent msg = new TranslationTextComponent("cli.msg.flag.added",
                    flagLink, buildRegionInfoLink(region, regionType));
            IFormattableTextComponent undoLink = buildRegionActionUndoLink(ctx.getInput(), ADD, REMOVE);
            sendCmdFeedback(ctx.getSource(), msg.append(" ").append(undoLink));
            return 0;
        } else {
            IFormattableTextComponent msg = new TranslationTextComponent("cli.msg.flag.present", flag.name,
                    buildRegionInfoLink(region, regionType));
            sendCmdFeedback(ctx.getSource(), msg);
            return 1;
        }
    }

    public static void removeInvolvedEntities(CommandContext<CommandSource> src, IProtectedRegion region, RegionFlag flag) {
        // FIXME: Level is where the command source is, not the target level of the region
        ServerWorld level = src.getSource().getLevel();
        Predicate<? super Entity> entityFilter = getEntityFilterForFlag(flag);
        if (region instanceof DimensionalRegion) {
            List<Entity> entities = getEntitiesToRemove(level, entityFilter, flag);
            entities.forEach(level::despawn);
        }
        if (region instanceof IMarkableRegion) {
            List<Entity> entities = getEntitiesToRemove(level, (IMarkableRegion) region, entityFilter);
            entities.forEach(level::despawn);
        }
        if (region instanceof GlobalRegion) {
            Map<ServerWorld, List<Entity>> entities = getEntitiesToRemove((GlobalRegion) region, entityFilter);
            entities.forEach((world, entityList) -> {
                entityList.forEach(world::despawn);
            });
        }
    }

    private static Predicate<? super Entity> getEntityFilterForFlag(RegionFlag flag) {
        switch (flag) {
            case SPAWNING_ALL:
                // FIXME: does not remove ExperienceOrbEntity
                return e -> !(e instanceof PlayerEntity);
            case SPAWNING_MONSTER:
                return HandlerUtil::isMonster;
            case SPAWNING_ANIMAL:
                return HandlerUtil::isAnimal;
            case SPAWNING_GOLEM:
                return e -> e instanceof GolemEntity;
            case SPAWNING_TRADER:
                return e -> e instanceof WanderingTraderEntity;
            case SPAWNING_SLIME:
                return e -> e instanceof SlimeEntity;
            case SPAWNING_VILLAGER:
                return HandlerUtil::isVillager;
            case SPAWNING_XP:
                return e -> e instanceof ExperienceOrbEntity;
            default:
                return e -> false;
        }
    }

    private static List<Entity> getEntitiesToRemove(ServerWorld level, IMarkableRegion region, Predicate<? super Entity> entityFilter) {
        // TODO: make this work with areas of different shapes by manually implementing it
        // TODO: Use the predicate to determine if the entity is within the region
        // area.intersects(entity.getBoundingBox()) ...
        return level.getEntities((Entity) null, ((CuboidArea) region.getArea()).getArea(), entityFilter);
    }

    private static Map<ServerWorld, List<Entity>> getEntitiesToRemove(GlobalRegion region, Predicate<? super Entity> entityFilter) {
        // TODO: Exclude entities from Local Regions or DimensionalRegions with flag
        return new HashMap<>();
    }

    private static List<Entity> getEntitiesToRemove(ServerWorld level, Predicate<? super Entity> entityFilter, RegionFlag flag) {
        List<Entity> entities = level.getEntities(null, entityFilter);
        // don't consider entities, which are currently in a Local Region which doesn't have the flag
        // FIXME: after flags can be negated (either flag is not existent or deactivated...)
        return entities.stream()
                .filter(e -> isInRegionWithoutFlag(level, flag, e))
                .collect(Collectors.toList());
    }

    private static boolean isInRegionWithoutFlag(ServerWorld level, RegionFlag flag, Entity e) {
        return LocalRegions.getRegionWithoutFlag(flag, e.blockPosition(), level.dimension()) == null;
    }

    public static int promptRegionInfo(CommandContext<CommandSource> ctx, IProtectedRegion region, RegionType regionType) {
        // == Region [<name>] overview ==
        sendCmdFeedback(ctx.getSource(), buildRegionOverviewHeader(region, regionType));
        // Flags: [n flag(s)][+]
        sendCmdFeedback(ctx.getSource(), buildInfoComponent("cli.msg.info.region.flag", buildFlagListLink(region, regionType)));
        if (regionType == RegionType.LOCAL) {
            IMarkableRegion markableRegion = (IMarkableRegion) region;
            // Area: [Area]
            sendCmdFeedback(ctx.getSource(), buildInfoComponent("cli.msg.info.region.area", buildRegionAreaLink(markableRegion)));
        }
        // Groups: [owners], [members], [<listAffiliations>]
        sendCmdFeedback(ctx.getSource(), buildInfoComponent("cli.msg.info.region.group", buildGroupLinks(region, regionType)));

        promptRegionChildrenInfo(ctx, region, regionType);
        // State: [State]
        sendCmdFeedback(ctx.getSource(), buildInfoComponent("cli.msg.info.region.state", buildRegionStateLink(region, regionType)));
        return 0;
    }

    public static void promptRegionChildrenInfo(CommandContext<CommandSource> ctx, IProtectedRegion region, RegionType regionType) {

        switch (regionType) {
            case GLOBAL: {
                // [n dimensions(s)]
                sendCmdFeedback(ctx.getSource(), buildInfoComponent("cli.msg.info.dim.region", buildRegionChildrenLink(region, RegionType.GLOBAL)));
            }
            break;
            case DIMENSION: {
                // Regions: [global], [n children], [n regions][+],
                MessageUtil.buildRegionChildrenLink(region, regionType);

                IFormattableTextComponent globalRegionLink = buildRegionInfoLink(region, RegionType.GLOBAL, new TranslationTextComponent("cli.msg.info.region.global.link.hover"));

                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(region.getDim());

                IFormattableTextComponent regionsAndChildren = new TranslationTextComponent("cli.msg.dim.info.region")
                        .append(": ")
                        .append(globalRegionLink)
                        .append(new StringTextComponent(", ").withStyle(TextFormatting.RESET))
                        .append(buildRegionChildrenLink(region, regionType))
                        .append(new StringTextComponent(", ").withStyle(TextFormatting.RESET))
                        .append(buildDimRegionsLink(dimCache));
                sendCmdFeedback(ctx.getSource(), regionsAndChildren);
            }
            break;
            case LOCAL: {
                // Hierarchy: [parent][x], [n children][+]
                IFormattableTextComponent regionHierarchy = new TranslationTextComponent("cli.msg.info.region.hierarchy")
                        .append(": ")
                        .append(buildRegionInfoLink(region, regionType, new TranslationTextComponent("cli.msg.info.region.link.text", region.getName())))
                        .append(buildParentClearLink((IMarkableRegion) region))
                        .append(new StringTextComponent(", ").withStyle(TextFormatting.RESET))
                        .append(buildRegionChildrenLink(region, regionType));
                sendCmdFeedback(ctx.getSource(), regionHierarchy);
            }
            break;
            case TEMPLATE:
                break;
            default:
                throw new IllegalArgumentException("");
        }
    }

}
