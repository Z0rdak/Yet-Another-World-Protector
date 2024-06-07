package de.z0rdak.yawp.commands;

import com.mojang.authlib.GameProfile;
import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.commands.arguments.flag.IFlagArgumentType;
import de.z0rdak.yawp.commands.arguments.region.RegionArgumentType;
import de.z0rdak.yawp.config.server.FlagConfig;
import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.flag.BooleanFlag;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.group.GroupType;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.core.region.RegionType;
import de.z0rdak.yawp.handler.flags.HandlerUtil;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.MojangApiHelper;
import net.minecraft.command.CommandSource;
import net.minecraft.command.argument.DimensionArgumentType;
import net.minecraft.command.argument.EntityArgumentType;
import net.minecraft.command.argument.TeamArgumentType;
import net.minecraft.command.argument.UuidArgumentType;
import net.minecraft.entity.Entity;
import net.minecraft.entity.ExperienceOrbEntity;
import net.minecraft.entity.mob.MobEntity;
import net.minecraft.entity.mob.SlimeEntity;
import net.minecraft.entity.passive.IronGolemEntity;
import net.minecraft.entity.passive.SnowGolemEntity;
import net.minecraft.entity.passive.WanderingTraderEntity;
import net.minecraft.registry.RegistryKey;
import net.minecraft.registry.RegistryKeys;
import net.minecraft.scoreboard.Team;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.command.CommandManager;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.server.command.ServerCommandSource;
import net.minecraft.server.network.ServerPlayerEntity;
import net.minecraft.server.world.ServerWorld;
import net.minecraft.text.MutableText;
import net.minecraft.text.Text;
import net.minecraft.util.Formatting;
import net.minecraft.util.TypeFilter;
import net.minecraft.world.World;
import org.apache.commons.lang3.NotImplementedException;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.commands.arguments.ArgumentUtil.*;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.processCheck;
import static de.z0rdak.yawp.util.ChatComponentBuilder.*;
import static de.z0rdak.yawp.util.MessageSender.sendCmdFeedback;

public class CommandUtil {

    public final static String MEMBER = "members";
    public final static String OWNER = "owners";
    public final static List<String> GROUP_LIST = Arrays.asList(MEMBER, OWNER);

    public static LiteralArgumentBuilder<ServerCommandSource> buildClearSubCommand(Function<CommandContext<ServerCommandSource>, IProtectedRegion> regionSupplier) {
        return literal(CLEAR)
                .then(literal(FLAGS)
                        .executes(ctx -> CommandUtil.clearFlags(ctx, regionSupplier.apply(ctx)))
                )
                .then(literal(PLAYERS)
                        .executes(ctx -> CommandUtil.clearPlayers(ctx, regionSupplier.apply(ctx)))
                        .then(CommandManager.argument(GROUP.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> CommandSource.suggestMatching(GROUP_LIST, builder))
                                .executes(ctx -> CommandUtil.clearPlayers(ctx, regionSupplier.apply(ctx), getGroupArgument(ctx))))
                )
                .then(literal(TEAMS)
                        .executes(ctx -> CommandUtil.clearTeams(ctx, regionSupplier.apply(ctx)))
                        .then(CommandManager.argument(GROUP.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> CommandSource.suggestMatching(GROUP_LIST, builder))
                                .executes(ctx -> CommandUtil.clearTeams(ctx, regionSupplier.apply(ctx), getGroupArgument(ctx))))
                )
                .then(literal(GROUP)
                        .then(CommandManager.argument(GROUP.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> CommandSource.suggestMatching(GROUP_LIST, builder))
                                .executes(ctx -> CommandUtil.clearGroups(ctx, regionSupplier.apply(ctx), getGroupArgument(ctx))))
                );
    }

    public static LiteralArgumentBuilder<ServerCommandSource> buildRemoveSubCommand(Function<CommandContext<ServerCommandSource>, IProtectedRegion> regionSupplier) {
        return literal(REMOVE)
                .then(literal(PLAYER)
                        .then(CommandManager.argument(GROUP.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> CommandSource.suggestMatching(GROUP_LIST, builder))
                                .then(CommandManager.argument(PLAYER.toString(), EntityArgumentType.players())
                                        .executes(ctx -> removePlayers(ctx, getPlayersArgument(ctx), regionSupplier.apply(ctx), getGroupArgument(ctx))))
                                .then(literal(BY_UUID)
                                        .then(CommandManager.argument(PLAYER_UUID.toString(), UuidArgumentType.uuid())
                                                .suggests((ctx, builder) -> CommandSource.suggestMatching(regionSupplier.apply(ctx).getGroup(getGroupArgument(ctx)).getPlayers().keySet().stream().map(UUID::toString).collect(Collectors.toList()), builder))
                                                .executes(ctx -> removePlayerByUUID(ctx, getPlayerUUIDArgument(ctx), regionSupplier.apply(ctx), getGroupArgument(ctx)))))
                                .then(literal(BY_NAME)
                                        .then(CommandManager.argument(PLAYER_NAMES.toString(), StringArgumentType.greedyString())
                                                .suggests((ctx, builder) -> CommandSource.suggestMatching(regionSupplier.apply(ctx).getGroup(getGroupArgument(ctx)).getPlayers().values(), builder))
                                                .executes(ctx -> removePlayersByName(ctx, getPlayerNamesArgument(ctx), regionSupplier.apply(ctx), getGroupArgument(ctx)))))
                        )
                )
                .then(literal(TEAM)
                        .then(CommandManager.argument(GROUP.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> CommandSource.suggestMatching(GROUP_LIST, builder))
                                .then(CommandManager.argument(TEAM.toString(), TeamArgumentType.team())
                                        .executes(ctx -> removeTeam(ctx, getTeamArgument(ctx), regionSupplier.apply(ctx), getGroupArgument(ctx))))))
                .then(literal(FLAG)
                        .then(CommandManager.argument(FLAG.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> IFlagArgumentType.flag().listSuggestions(ctx, builder))
                                .executes(ctx -> removeRegionFlag(ctx, regionSupplier.apply(ctx), getFlagArgument(ctx)))
                        )
                )
                .then(literal(FLAGS)
                        .then(CommandManager.argument(FLAGS.toString(), StringArgumentType.greedyString())
                                .suggests((ctx, builder) -> IFlagArgumentType.flag().listSuggestions(ctx, builder))
                                .executes(ctx -> removeFlags(ctx, regionSupplier.apply(ctx), getFlagArguments(ctx))))
                );
    }

    public static LiteralArgumentBuilder<ServerCommandSource> buildAddSubCommand(Function<CommandContext<ServerCommandSource>, IProtectedRegion> regionSupplier) {
        return literal(ADD)
                .then(literal(PLAYER)
                        .then(CommandManager.argument(GROUP.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> CommandSource.suggestMatching(GROUP_LIST, builder))
                                .then(CommandManager.argument(PLAYER.toString(), EntityArgumentType.players())
                                        .executes(ctx -> addPlayers(ctx, getPlayersArgument(ctx), regionSupplier.apply(ctx), getGroupArgument(ctx))))
                                .then(literal(BY_UUID)
                                        .then(CommandManager.argument(PLAYER_UUID.toString(), UuidArgumentType.uuid())
                                                .executes(ctx -> addPlayerByUuid(ctx, getPlayerUUIDArgument(ctx), regionSupplier.apply(ctx), getGroupArgument(ctx)))))
                                .then(literal(BY_NAME)
                                        .then(CommandManager.argument(PLAYER_NAMES.toString(), StringArgumentType.greedyString())
                                                .executes(ctx -> addPlayersByName(ctx, getPlayerNamesArgument(ctx), regionSupplier.apply(ctx), getGroupArgument(ctx)))))
                        )
                )
                .then(literal(TEAM)
                        .then(CommandManager.argument(GROUP.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> CommandSource.suggestMatching(GROUP_LIST, builder))
                                .then(CommandManager.argument(TEAM.toString(), TeamArgumentType.team())
                                        .executes(ctx -> addTeam(ctx, getTeamArgument(ctx), regionSupplier.apply(ctx), getGroupArgument(ctx))))))
                .then(literal(FLAG)
                        .then(CommandManager.argument(FLAG.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> IFlagArgumentType.flag().listSuggestions(ctx, builder))
                                .executes(ctx -> addFlag(ctx, regionSupplier.apply(ctx), getFlagArgument(ctx)))
                                .then(CommandManager.argument(STATE.toString(), StringArgumentType.word())
                                        .suggests((ctx, builder) -> CommandSource.suggestMatching(FlagState.ValidFlagStates(), builder))
                                        .executes(ctx -> addFlag(ctx, regionSupplier.apply(ctx), getFlagArgument(ctx), getFlagStateArgument(ctx), false))
                                        .then(CommandManager.argument(OVERRIDE.toString(), BoolArgumentType.bool())
                                                .executes(ctx -> addFlag(ctx, regionSupplier.apply(ctx), getFlagArgument(ctx), getFlagStateArgument(ctx), getOverrideArgument(ctx))))
                                )
                        )
                )
                .then(literal(FLAGS)
                        .then(CommandManager.argument(FLAGS.toString(), StringArgumentType.greedyString())
                                .suggests((ctx, builder) -> IFlagArgumentType.flag().listSuggestions(ctx, builder))
                                .executes(ctx -> addFlags(ctx, regionSupplier.apply(ctx), getFlagArguments(ctx))))
                )
                .then(literal(ALL_FLAGS)
                        .executes(ctx -> addAllFlags(ctx, regionSupplier.apply(ctx)))
                );
    }

    public static LiteralArgumentBuilder<ServerCommandSource> buildListSubCommand(Function<CommandContext<ServerCommandSource>, IProtectedRegion> regionSupplier) {
        return literal(LIST)
                .then(literal(CHILDREN)
                        .executes(ctx -> promptRegionChildren(ctx, regionSupplier.apply(ctx), 0))
                        .then(CommandManager.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                .executes(ctx -> promptRegionChildren(ctx, regionSupplier.apply(ctx), getPageNoArgument(ctx)))))
                .then(literal(FLAG)
                        .executes(ctx -> promptFlagList(ctx, regionSupplier.apply(ctx), 0))
                        .then(CommandManager.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                .executes(ctx -> promptFlagList(ctx, regionSupplier.apply(ctx), getPageNoArgument(ctx)))))
                .then(literal(REGION_FLAG)
                        .executes(ctx -> promptRegionFlagList(ctx, regionSupplier.apply(ctx), 0))
                        .then(CommandManager.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                .executes(ctx -> promptRegionFlagList(ctx, regionSupplier.apply(ctx), getPageNoArgument(ctx)))))
                .then(literal(GROUP)
                        .then(CommandManager.argument(GROUP.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> CommandSource.suggestMatching(CommandUtil.GROUP_LIST, builder))
                                .executes(ctx -> CommandUtil.promptGroupLinks(ctx, regionSupplier.apply(ctx), getGroupArgument(ctx)))
                                .then(literal(TEAM)
                                        .executes(ctx -> CommandUtil.promptGroupList(ctx, regionSupplier.apply(ctx), getGroupArgument(ctx), GroupType.TEAM, 0))
                                        .then(CommandManager.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                                .executes(ctx -> CommandUtil.promptGroupList(ctx, regionSupplier.apply(ctx), getGroupArgument(ctx), GroupType.TEAM, getPageNoArgument(ctx))))
                                )
                                .then(literal(PLAYER)
                                        .executes(ctx -> CommandUtil.promptGroupList(ctx, regionSupplier.apply(ctx), getGroupArgument(ctx), GroupType.PLAYER, 0))
                                        .then(CommandManager.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                                .executes(ctx -> CommandUtil.promptGroupList(ctx, regionSupplier.apply(ctx), getGroupArgument(ctx), GroupType.PLAYER, getPageNoArgument(ctx))))
                                )
                        )
                );
    }

    public static LiteralArgumentBuilder<ServerCommandSource> buildCopySubCommand(Function<CommandContext<ServerCommandSource>, IProtectedRegion> srcSupplier) {
        return literal(COPY)
                .then(literal(FLAGS)
                        .then(literal(TO_LOCAL)
                                .then(CommandManager.argument(TARGET_DIM.toString(), DimensionArgumentType.dimension())
                                        .then(CommandManager.argument(TARGET_REGION.toString(), StringArgumentType.word())
                                                .suggests((ctx, builder) -> RegionArgumentType.region().listRegionsInTargetDim(ctx, builder))
                                                .executes(ctx -> copyRegionFlags(ctx, srcSupplier.apply(ctx), getTargetLocalRegionArgument(ctx))))))
                        .then(literal(TO_DIM)
                                .then(CommandManager.argument(TARGET_DIM.toString(), DimensionArgumentType.dimension())
                                        .executes(ctx -> copyRegionFlags(ctx, srcSupplier.apply(ctx), getTargetDimRegionArgument(ctx).getDimensionalRegion()))))
                )
                .then(literal(PLAYERS)
                        .then(literal(TO_LOCAL)
                                .then(CommandManager.argument(TARGET_DIM.toString(), DimensionArgumentType.dimension())
                                        .then(CommandManager.argument(TARGET_REGION.toString(), StringArgumentType.word())
                                                .suggests((ctx, builder) -> RegionArgumentType.region().listRegionsInTargetDim(ctx, builder))
                                                .then(CommandManager.argument(GROUP.toString(), StringArgumentType.word())
                                                        .suggests((ctx, builder) -> CommandSource.suggestMatching(GROUP_LIST, builder))
                                                        .executes(ctx -> copyRegionPlayers(ctx, srcSupplier.apply(ctx), getTargetLocalRegionArgument(ctx), getGroupArgument(ctx)))))))
                        .then(literal(TO_DIM)
                                .then(CommandManager.argument(TARGET_DIM.toString(), DimensionArgumentType.dimension())
                                        .then(CommandManager.argument(GROUP.toString(), StringArgumentType.word())
                                                .suggests((ctx, builder) -> CommandSource.suggestMatching(GROUP_LIST, builder))
                                                .executes(ctx -> copyRegionPlayers(ctx, srcSupplier.apply(ctx), getTargetDimRegionArgument(ctx).getDimensionalRegion(), getGroupArgument(ctx)))))))
                .then(literal(STATE)
                        .then(literal(TO_LOCAL)
                                .then(CommandManager.argument(TARGET_DIM.toString(), DimensionArgumentType.dimension())
                                        .then(CommandManager.argument(TARGET_REGION.toString(), StringArgumentType.word())
                                                .suggests((ctx, builder) -> RegionArgumentType.region().listRegionsInTargetDim(ctx, builder))
                                                .executes(ctx -> copyRegionState(ctx, srcSupplier.apply(ctx), getTargetLocalRegionArgument(ctx))))))
                        .then(literal(TO_DIM)
                                .then(CommandManager.argument(TARGET_DIM.toString(), DimensionArgumentType.dimension())
                                        .executes(ctx -> copyRegionState(ctx, srcSupplier.apply(ctx), getTargetDimRegionArgument(ctx).getDimensionalRegion()))))
                );
    }

    /**
     * Prompt the common region state to the command issuer.
     * == [state] for [<name>] ==
     * Enabled: [true|false] | [all-off] [all-on]
     * Alert: [on|off] | [all-off] [all-on]
     */
    public static int promptRegionState(CommandContext<ServerCommandSource> ctx, IProtectedRegion region) {
        sendCmdFeedback(ctx.getSource(), buildHeader(Text.translatableWithFallback("cli.msg.info.header.for", "== %s for %s ==", buildRegionStateLink(region), buildRegionInfoLink(region))));
        MutableText regionEnableComponent = buildRegionEnableComponent(region);
        MutableText regionAlertComponent = buildRegionAlertToggleLink(region);
        if (region.getRegionType() == RegionType.DIMENSION) {
            MutableText enableComp = Text.translatable("%s | %s", regionEnableComponent, buildAllLocalEnableComponent(getDimCacheArgument(ctx)));
            MutableText alertComp = Text.translatable("%s | %s", regionAlertComponent, buildAllLocalAlertToggleLink(getDimCacheArgument(ctx)));
            sendCmdFeedback(ctx.getSource(), buildInfoComponent("cli.msg.info.region.state.enable", "Enabled", enableComp));
            sendCmdFeedback(ctx.getSource(), buildInfoComponent("cli.msg.info.region.state.alert", "Alert", alertComp));
            return 0;
        }
        if (region.getRegionType() == RegionType.LOCAL) {
            sendCmdFeedback(ctx.getSource(), buildInfoComponent("cli.msg.info.region.state.name", "Name", buildRegionRenameLink(region)));
        }
        MutableText enableComp = buildInfoComponent("cli.msg.info.region.state.enable", "Enabled", regionEnableComponent);
        MutableText alertComp = buildInfoComponent("cli.msg.info.region.state.alert", "Alert", regionAlertComponent);
        sendCmdFeedback(ctx.getSource(), Text.translatable("%s, %s", enableComp, alertComp));
        return 0;
    }

    /**
     * == Group '%s' for '%s'==
     * Players: [n player(s)][+]
     * Teams: [m team(s)][+]
     */
    public static int promptGroupLinks(CommandContext<ServerCommandSource> ctx, IProtectedRegion region, String group) {
        if (!GROUP_LIST.contains(group)) {
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.region.info.group.invalid", "cli.msg.region.info.group.invalid", group).formatted(Formatting.RED));
            return -1;
        }
        sendCmdFeedback(ctx.getSource(), buildGroupHeader(region, group));
        sendCmdFeedback(ctx.getSource(), buildGroupPlayerListLink(region, group));
        sendCmdFeedback(ctx.getSource(), buildGroupTeamListLink(region, group));
        return 0;
    }

    /**
     * List of flags of the provided region and its parents
     */
    public static int promptFlagList(CommandContext<ServerCommandSource> ctx, IProtectedRegion region, int pageNo) {
        String cmd = "";
        switch (region.getRegionType()) {
            case GLOBAL:
                cmd = buildCommandStr(GLOBAL.toString(), LIST.toString(), FLAG.toString());
                break;
            case DIMENSION:
                cmd = buildCommandStr(DIM.toString(), region.getName(), LIST.toString(), FLAG.toString());
                break;
            case LOCAL:
                cmd = buildCommandStr(LOCAL.toString(), region.getDim().getValue().toString(), region.getName(), LIST.toString(), FLAG.toString());
                break;
        }

        List<MutableText> flagEntries = buildFlagEntries(region);
        if (flagEntries.isEmpty()) {
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.info.region.flag.empty", "No flags defined in %s", buildRegionInfoLink(region)));
            return 1;
        }
        MutableText flagListLink = buildResponsibleFlagListLink(region);
        List<MutableText> flagPagination = buildPaginationComponents(
                buildRegionFlagInfoHeader(region, flagListLink), cmd, flagEntries, pageNo,
                Text.translatableWithFallback(" - %s", " - %s", buildAddFlagLink(region)));
        flagPagination.forEach(line -> sendCmdFeedback(ctx.getSource(), line));
        return 0;
    }

    /**
     * List of flags only of the provided region
     */
    public static int promptRegionFlagList(CommandContext<ServerCommandSource> ctx, IProtectedRegion region, int pageNo) {
        String cmd = "";
        switch (region.getRegionType()) {
            case GLOBAL:
                cmd = buildCommandStr(GLOBAL.toString(), LIST.toString(), REGION_FLAG.toString());
                break;
            case DIMENSION:
                cmd = buildCommandStr(DIM.toString(), region.getName(), LIST.toString(), REGION_FLAG.toString());
                break;
            case LOCAL:
                cmd = buildCommandStr(LOCAL.toString(), region.getDim().getValue().toString(), region.getName(), LIST.toString(), REGION_FLAG.toString());
                break;
        }

        List<MutableText> flagEntries = buildRegionFlagEntries(region);
        if (flagEntries.isEmpty()) {
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.info.region.flag.empty", "No flags defined in %s", buildRegionInfoLink(region)));
            return 1;
        }
        MutableText flagListLink = buildRegionFlagListLink(region);
        List<MutableText> flagPagination = buildPaginationComponents(
                buildRegionFlagInfoHeader(region, flagListLink), cmd, flagEntries, pageNo,
                Text.translatableWithFallback(" - %s", " - %s", buildAddFlagLink(region)));
        flagPagination.forEach(line -> sendCmdFeedback(ctx.getSource(), line));
        return 0;
    }

    public static int promptRegionChildren(CommandContext<ServerCommandSource> ctx, IProtectedRegion region, int pageNo) {
        String listChildrenCmd = "";
        MutableText addChildRegionLink = Text.literal("");
        List<IProtectedRegion> children = new ArrayList<>(region.getChildren().values());
        if (region.getChildren().isEmpty()) {
            MutableText noChildrenText = Text.translatableWithFallback("cli.msg.info.region.children.empty", "No children defined in region %s", buildRegionInfoLink(region));
            sendCmdFeedback(ctx.getSource(), noChildrenText);
            return 0;
        }
        switch (region.getRegionType()) {
            case GLOBAL:
                listChildrenCmd = buildCommandStr(GLOBAL.toString(), LIST.toString(), CHILDREN.toString());
                break;
            case DIMENSION:
                listChildrenCmd = buildCommandStr(DIM.toString(), region.getName(), LIST.toString(), CHILDREN.toString());
                break;
            case LOCAL:
                listChildrenCmd = buildCommandStr(LOCAL.toString(), region.getDim().getValue().toString(), region.getName(), LIST.toString(), CHILDREN.toString());
                addChildRegionLink = Text.translatableWithFallback(" - %s", " - %s", buildRegionAddChildrenLink(region));
                break;
            default:
                throw new NotImplementedException("Region type not implemented yet");
        }
        MutableText regionListHeader = buildHeader(Text.translatableWithFallback("cli.msg.info.header.in", "== %s in %s ==", buildRegionListChildrenLink(region), buildRegionInfoLink(region)));
        List<MutableText> regionPagination = buildPaginationComponents(
                regionListHeader,
                listChildrenCmd,
                buildRemoveRegionEntries(region, children),
                pageNo,
                addChildRegionLink);
        regionPagination.forEach(line -> sendCmdFeedback(ctx.getSource(), line));
        return 0;
    }

    public static int promptGroupList(CommandContext<ServerCommandSource> ctx, IProtectedRegion region, String group, GroupType groupType, int pageNo) {
        String cmd = "";
        String dim = region.getDim().getValue().toString();
        switch (region.getRegionType()) {
            case GLOBAL:
                cmd = buildCommandStr(GLOBAL.toString(), LIST.toString(), GROUP.toString(), group, groupType.name);
                break;
            case DIMENSION:
                cmd = buildCommandStr(DIM.toString(), dim, LIST.toString(), GROUP.toString(), group, groupType.name);
                break;
            case LOCAL:
                cmd = buildCommandStr(LOCAL.toString(), dim, region.getName(), LIST.toString(), GROUP.toString(), group, groupType.name);
                break;
        }
        if (!GROUP_LIST.contains(group)) {
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.region.info.group.invalid", group).formatted(Formatting.RED));
            return -1;
        }
        List<String> groupNames = getGroupList(region, group, groupType);
        if (groupNames.isEmpty()) {
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.info.region.group." + groupType.name + ".empty",
                    group, buildRegionInfoLink(region)));
            return 1;
        }
        List<MutableText> groupMemberPagination = buildPaginationComponents(
                buildGroupHeader(region, group, groupType),
                cmd, buildRemoveGroupMemberEntries(region, groupNames, groupType, group),
                pageNo,
                Text.translatableWithFallback(" - %s", " - %s", buildAddToGroupLink(region, group, groupType)));
        groupMemberPagination.forEach(line -> sendCmdFeedback(ctx.getSource(), line));
        return 0;
    }

    public static int setActiveState(CommandContext<ServerCommandSource> ctx, IProtectedRegion region, boolean activate) {
        region.setIsActive(activate);
        RegionDataManager.save();
        MutableText undoLink = buildRegionActionUndoLink(ctx.getInput(), String.valueOf(!activate), String.valueOf(activate));
        MutableText msg = Text.translatableWithFallback("cli.msg.info.region.state.enable.set.value", "Region state of %s is now: %s",
                buildRegionInfoLink(region), region.isActive() ? "active" : "inactive");
        sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("%s %s", "%s %s", "%s %s", msg, undoLink));
        return 0;
    }

    public static int setAlertState(CommandContext<ServerCommandSource> ctx, IProtectedRegion region, boolean showAlert) {
        region.setIsMuted(!showAlert);
        RegionDataManager.save();
        MutableText undoLink = buildRegionActionUndoLink(ctx.getInput(), String.valueOf(!showAlert), String.valueOf(showAlert));
        MutableText msg = Text.translatableWithFallback("cli.msg.info.state.alert.set.value", "Flag messages of %s are now: %s",
                buildRegionInfoLink(region), region.isMuted() ? "muted" : "active");
        sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("%s %s", "%s %s", "%s %s", msg, undoLink));
        return 0;
    }

    public static int removeTeam(CommandContext<ServerCommandSource> ctx, Team team, IProtectedRegion region, String group) {
        if (!GROUP_LIST.contains(group)) {
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.region.info.group.invalid", group).formatted(Formatting.RED));
            return -1;
        }
        MutableText undoLink = buildRegionActionUndoLink(ctx.getInput(), REMOVE, ADD);
        MutableText teamInfo = buildGroupInfo(region, team.getName(), GroupType.TEAM);
        if (region.getGroup(group).hasTeam(team.getName())) {
            region.removeTeam(team.getName(), group);
            RegionDataManager.save();
            MutableText msg = Text.translatableWithFallback("cli.msg.info.region.group.team.removed", "Removed team '%s' (group '%s') from %s", teamInfo, group,
                    buildRegionInfoLink(region));
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("%s %s", "%s %s", "%s %s", msg, undoLink));
            return 0;
        }
        MutableText msg = Text.translatableWithFallback("cli.msg.info.region.group.team.not-present", "Team '%s' (group '%s') is not present in %s", teamInfo, group,
                buildRegionInfoLink(region));
        sendCmdFeedback(ctx.getSource(), msg);
        return 1;
    }


    private static int removePlayersByName(CommandContext<ServerCommandSource> ctx, Collection<String> playerNames, IProtectedRegion region, String group) {
        if (!GROUP_LIST.contains(group)) {
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.region.info.group.invalid", group).formatted(Formatting.RED));
            return -1;
        }
        playerNames.forEach(playerName -> CommandUtil.removePlayer(ctx, playerName, region, group));
        return 0;
    }

    private static void removePlayer(CommandContext<ServerCommandSource> ctx, String playerName, IProtectedRegion region, String group) {
        region.getGroup(group).getPlayers().entrySet()
                .stream()
                .filter(e -> e.getValue().equals(playerName))
                .findFirst()
                .ifPresent(e -> removePlayer(ctx, e.getKey(), playerName, region, group));
    }

    private static int removePlayerByUUID(CommandContext<ServerCommandSource> ctx, UUID playerUuid, IProtectedRegion region, String group) {
        if (!GROUP_LIST.contains(group)) {
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.region.info.group.invalid", group).formatted(Formatting.RED));
            return -1;
        }
        if (region.getGroup(group).hasPlayer(playerUuid)) {
            String playerName = region.getGroup(group).getPlayers().get(playerUuid);
            return removePlayer(ctx, playerUuid, playerName, region, group);
        }
        return 1;
    }


    private static int removePlayers(CommandContext<ServerCommandSource> ctx, Collection<ServerPlayerEntity> players, IProtectedRegion region, String group) {
        players.forEach(player -> CommandUtil.removePlayer(ctx, player, region, group));
        return 0;
    }

    private static int removePlayer(CommandContext<ServerCommandSource> ctx, PlayerEntity player, IProtectedRegion region, String group) {
        if (!GROUP_LIST.contains(group)) {
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.region.info.group.invalid", group).formatted(Formatting.RED));
            return -1;
        }
        if (region.getGroup(group).hasPlayer(player.getUuid())) {
            String playerName = region.getGroup(group).getPlayers().get(player.getUuid());
            return removePlayer(ctx, player.getUuid(), playerName, region, group);
        }
        return 1;
    }

    public static int removePlayer(CommandContext<ServerCommandSource> ctx, UUID playerUuid, String playerName, IProtectedRegion region, String group) {
        MutableText playerInfo = buildGroupInfo(region, playerName, GroupType.PLAYER);
        MutableText undoLink = buildRegionActionUndoLink(ctx.getInput(), REMOVE, ADD);
        if (region.getGroup(group).hasPlayer(playerUuid)) {
            region.removePlayer(playerUuid, group);
            MutableText msg = Text.translatableWithFallback("cli.msg.info.region.group.player.removed", "Removed player '%s' (group '%s') from %s", playerInfo, group,
                    buildRegionInfoLink(region));
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("%s %s", "%s %s", msg, undoLink));
            RegionDataManager.save();
            return 0;
        }

        MutableText msg = Text.translatableWithFallback("cli.msg.info.region.group.player.not-present", "Player '%s' (group '%s') is not present in %s", playerInfo, group,
                buildRegionInfoLink(region));
        sendCmdFeedback(ctx.getSource(), msg);
        return 1;
    }

    private static int addPlayerByUuid(CommandContext<ServerCommandSource> ctx, UUID playerUuid, IProtectedRegion region, String group) {
        if (!GROUP_LIST.contains(group)) {
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.region.info.group.invalid", group).formatted(Formatting.RED));
            return -1;
        }
        Optional<GameProfile> cachedProfile = MojangApiHelper.lookupGameProfileInCache(ctx, playerUuid);
        if (cachedProfile.isPresent()) {
            GameProfile profile = cachedProfile.get();
            if (profile.isComplete()) {
                MutableText cacheSuccess = Text.translatableWithFallback("cli.msg.info.player.lookup.cache.success", playerUuid.toString());
                sendCmdFeedback(ctx.getSource(), cacheSuccess);
                return addPlayer(ctx, profile.getId(), profile.getName(), region, group);
            }
            return -1;
        } else {
            MutableText cacheMiss = Text.translatableWithFallback("cli.msg.info.player.lookup.pending", "No cache entry existing for '%s'. Looking up %s at Mojang...",
                    playerUuid.toString(), "uuid");
            sendCmdFeedback(ctx.getSource(), cacheMiss);
            CompletableFuture.runAsync(() -> MojangApiHelper.getGameProfileInfo(playerUuid, (gameProfile) -> {
                if (gameProfile != null) {
                    MutableText lookupSuccess = Text.translatableWithFallback("cli.msg.info.player.lookup.api.success", playerUuid.toString());
                    sendCmdFeedback(ctx.getSource(), lookupSuccess);
                    addPlayer(ctx, gameProfile.getId(), gameProfile.getName(), region, group);
                } else {
                    MutableText lookupFailed = Text.translatableWithFallback("cli.msg.info.player.lookup.api.failed", playerUuid.toString());
                    sendCmdFeedback(ctx.getSource(), lookupFailed);
                }
            }));
            return 0;
        }
    }

    public static int addPlayersByName(CommandContext<ServerCommandSource> ctx, List<String> playerNames, IProtectedRegion region, String group) {
        if (!GROUP_LIST.contains(group)) {
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.region.info.group.invalid", group).formatted(Formatting.RED));
            return -1;
        }
        playerNames.forEach(name -> CommandUtil.addPlayerByName(ctx, name, region, group));
        return 0;
    }

    private static int addPlayerByName(CommandContext<ServerCommandSource> ctx, String playerName, IProtectedRegion region, String group) {
        Optional<GameProfile> cachedProfile = MojangApiHelper.lookupGameProfileInCache(ctx, playerName);
        if (cachedProfile.isPresent()) {
            GameProfile profile = cachedProfile.get();
            if (profile.isComplete()) {
                MutableText cacheSuccess = Text.translatableWithFallback("cli.msg.info.player.lookup.cache.success", playerName);
                sendCmdFeedback(ctx.getSource(), cacheSuccess);
                return addPlayer(ctx, profile.getId(), profile.getName(), region, group);
            }
            return -1;
        } else {
            MutableText cacheMiss = Text.translatableWithFallback("cli.msg.info.player.lookup.pending", "No cache entry existing for '%s'. Looking up %s at Mojang...",
                    playerName, "name");
            sendCmdFeedback(ctx.getSource(), cacheMiss);
            CompletableFuture.runAsync(() -> MojangApiHelper.getGameProfileInfo(playerName, (gameProfile) -> {
                if (gameProfile != null) {
                    MutableText lookupSuccess = Text.translatableWithFallback("cli.msg.info.player.lookup.api.success", "Successfully retrieved game profile info for '%s' from Mojang", playerName);
                    sendCmdFeedback(ctx.getSource(), lookupSuccess);
                    addPlayer(ctx, gameProfile.getId(), gameProfile.getName(), region, group);
                } else {
                    MutableText lookupFailed = Text.translatableWithFallback("cli.msg.info.player.lookup.api.failed", "Unable to retrieve game profile info for '%s' from Mojang", playerName);
                    sendCmdFeedback(ctx.getSource(), lookupFailed);
                }
            }));
            return 0;
        }
    }

    public static int addPlayers(CommandContext<ServerCommandSource> ctx, Collection<ServerPlayerEntity> players, IProtectedRegion region, String group) {
        players.forEach(player -> CommandUtil.addPlayer(ctx, player, region, group));
        return 0;
    }

    public static int addPlayer(CommandContext<ServerCommandSource> ctx, PlayerEntity player, IProtectedRegion region, String group) {
        if (!GROUP_LIST.contains(group)) {
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.region.info.group.invalid", group).formatted(Formatting.RED));
            return -1;
        }
        return addPlayer(ctx, player.getUuid(), player.getEntityName(), region, group);
    }

    private static int addPlayer(CommandContext<ServerCommandSource> ctx, UUID uuid, String name, IProtectedRegion region, String group) {
        MutableText regionInfoLink = buildRegionInfoLink(region);
        MutableText undoLink = buildRegionActionUndoLink(ctx.getInput(), ADD, REMOVE);
        if (!region.hasPlayer(uuid, group)) {
            region.addPlayer(uuid, name, group);
            RegionDataManager.save();
            MutableText msg = Text.translatableWithFallback("cli.msg.info.region.group.player.added", "Added player '%s' as '%s' to %s", name, group, regionInfoLink);
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("%s %s", "%s %s", msg, undoLink));
            return 0;
        }
        MutableText msg = Text.translatableWithFallback("cli.msg.info.region.group.player.present", "Player '%s' (group '%s') already present in %s", name, group, regionInfoLink);
        sendCmdFeedback(ctx.getSource(), msg);
        return 1;
    }

    public static int addTeam(CommandContext<ServerCommandSource> ctx, Team team, IProtectedRegion region, String group) {
        if (!GROUP_LIST.contains(group)) {
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.region.info.group.invalid", group).formatted(Formatting.RED));
            return -1;
        }
        MutableText regionInfoLink = buildRegionInfoLink(region);
        MutableText teamHoverInfo = buildTeamHoverComponent(team);
        if (!region.hasTeam(team.getName(), group)) {
            region.addTeam(team.getName(), group);
            RegionDataManager.save();
            MutableText undoLink = buildRegionActionUndoLink(ctx.getInput(), ADD, REMOVE);
            MutableText msg = Text.translatableWithFallback("cli.msg.info.region.group.team.added", "Added team '%s' as '%s' to region %s",
                    teamHoverInfo, group, regionInfoLink);
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("%s %s", "%s %s", msg, undoLink));
            return 0;
        }
        MutableText msg = Text.translatableWithFallback("cli.msg.info.region.group.team.present", "Team '%s' (group '%s') already present in %s",
                teamHoverInfo, group, regionInfoLink);
        sendCmdFeedback(ctx.getSource(), msg);
        return 1;
    }

    public static int removeFlags(CommandContext<ServerCommandSource> ctx, IProtectedRegion region, Set<RegionFlag> flags) {
        flags.forEach(flag -> CommandUtil.removeRegionFlag(ctx, region, flag));
        return 0;
    }

    public static int removeRegionFlag(CommandContext<ServerCommandSource> ctx, IProtectedRegion region, RegionFlag flag) {
        if (region.containsFlag(flag)) {
            region.removeFlag(flag.name);
            RegionDataManager.save();
            MutableText msg = Text.translatableWithFallback("cli.msg.flag.removed", "Removed flag '%s' from %s", flag.name,
                    buildRegionInfoLink(region));
            MutableText undoLink = buildRegionActionUndoLink(ctx.getInput(), REMOVE, ADD);
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("%s %s", "%s %s", msg, undoLink));
            return 0;
        } else {
            MutableText msg = Text.translatableWithFallback("cli.msg.flag.not-present", "Flag '%s' is not present in %s", flag.name,
                    buildRegionInfoLink(region));
            sendCmdFeedback(ctx.getSource(), msg);
            return 1;
        }
    }

    public static int copyRegionFlags(CommandContext<ServerCommandSource> ctx, IProtectedRegion srcRegion, IProtectedRegion targetRegion) {
        if (srcRegion.getFlags().isEmpty()) {
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.info.region.flag.empty", "No flags defined in %s", buildRegionInfoLink(srcRegion)));
            return 1;
        }
        // get the flags which are in the srcRegion but not in the targetRegion
        Set<IFlag> flagsToCopy = srcRegion.getFlags().stream()
                .filter(flag -> !targetRegion.containsFlag(flag.getName()))
                .collect(Collectors.toSet());
        // flagsToCopy.forEach(region::addFlag);
        srcRegion.getFlags().forEach(targetRegion::addFlag);
        RegionDataManager.save();
        sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.copy.region.flags", "Copied %s flag(s) from region %s to %s", flagsToCopy.size(), buildRegionInfoLink(srcRegion), buildRegionInfoLink(targetRegion)));
        return 0;
    }

    public static int copyRegionState(CommandContext<ServerCommandSource> ctx, IProtectedRegion srcRegion, IProtectedRegion targetRegion) {
        targetRegion.setIsActive(srcRegion.isActive());
        targetRegion.setIsMuted(srcRegion.isMuted());
        if (srcRegion instanceof IMarkableRegion regionSource && targetRegion instanceof IMarkableRegion regionTarget) {
            regionTarget.setPriority(regionSource.getPriority());
        }
        RegionDataManager.save();
        sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.copy.region.state", "Copied state from region %s to %s", buildRegionInfoLink(srcRegion), buildRegionInfoLink(targetRegion)));
        return 0;
    }

    public static int copyRegionPlayers(CommandContext<ServerCommandSource> ctx, IProtectedRegion srcRegion, IProtectedRegion targetRegion, String group) {
        if (!srcRegion.getGroup(group).getPlayers().isEmpty()) {
            // get the players which are in the srcRegion but not in the targetRegion
            Set<Map.Entry<UUID, String>> playerEntriesToCopy = srcRegion.getGroup(group).getPlayers().entrySet().stream()
                    .filter(entry -> !targetRegion.getGroup(group).getPlayers().containsKey(entry.getKey()))
                    .collect(Collectors.toSet());
            //playerEntriesToCopy.forEach(entry -> region.getGroup(group).addPlayer(entry.getKey(), entry.getValue()));

            srcRegion.getGroup(group).getPlayers().forEach((uuid, name) -> targetRegion.getGroup(group).addPlayer(uuid, name));
            RegionDataManager.save();
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.copy.region.players", "Copied %s player(s) of group '%s' from %s to %s", playerEntriesToCopy.size(), group, buildRegionInfoLink(srcRegion), buildRegionInfoLink(targetRegion)));
            return 0;
        }
        sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.info.region.group.player.empty", "No players defined as '%s' in %s", group, buildRegionInfoLink(srcRegion)));
        return 1;
    }

    public static int copyRegionPlayers(CommandContext<ServerCommandSource> ctx, IProtectedRegion region, IProtectedRegion srcRegion) {
        return copyRegionPlayers(ctx, region, srcRegion, CommandConstants.MEMBER.toString())
                + copyRegionPlayers(ctx, region, srcRegion, CommandConstants.OWNER.toString());
    }

    public static int clearFlags(CommandContext<ServerCommandSource> ctx, IProtectedRegion region) {
        int amount = region.getFlags().size();
        if (amount == 0) {
            MutableText feedbackMsg = Text.translatableWithFallback("cli.msg.info.region.flag.empty", "No flags defined in %s", buildRegionInfoLink(region));
            sendCmdFeedback(ctx.getSource(), feedbackMsg);
            return 1;
        }
        Collection<IFlag> flags = region.getFlags(); // TODO: Undo action link
        region.getFlags().clear();
        MutableText feedbackMsg = Text.translatableWithFallback("cli.msg.info.region.flag.cleared", "Removed %s flag(s) from %s", amount, buildRegionInfoLink(region));
        sendCmdFeedback(ctx.getSource(), feedbackMsg);
        RegionDataManager.save();
        return 0;
    }

    public static int clearPlayers(CommandContext<ServerCommandSource> ctx, IProtectedRegion region) {
        return clearPlayers(ctx, region, MEMBER) + clearPlayers(ctx, region, OWNER);
    }

    public static int clearPlayers(CommandContext<ServerCommandSource> ctx, IProtectedRegion region, String groupName) {
        int amount = region.getGroup(groupName).getPlayers().size();
        if (amount == 0) {
            MutableText feedbackMsg = Text.translatableWithFallback("cli.msg.info.region.players.empty", "No players (group '%s') present in %s", buildRegionInfoLink(region), groupName);
            sendCmdFeedback(ctx.getSource(), feedbackMsg);
            return 1;
        }
        region.getGroup(groupName).clearPlayers();
        MutableText feedbackMsg = Text.translatableWithFallback("cli.msg.info.region.players.cleared", "Cleared %s players of group '%s' for %s\",", buildRegionInfoLink(region), amount, groupName);
        sendCmdFeedback(ctx.getSource(), feedbackMsg);
        RegionDataManager.save();
        return 0;
    }

    public static int clearTeams(CommandContext<ServerCommandSource> ctx, IProtectedRegion region) {
        return clearPlayers(ctx, region, MEMBER) + clearPlayers(ctx, region, OWNER);
    }

    public static int clearTeams(CommandContext<ServerCommandSource> ctx, IProtectedRegion region, String groupName) {
        int amount = region.getGroup(groupName).getTeams().size();
        if (amount == 0) {
            MutableText feedbackMsg = Text.translatableWithFallback("cli.msg.info.region.teams.empty", "No teams (group '%s') present in %s", buildRegionInfoLink(region), groupName);
            sendCmdFeedback(ctx.getSource(), feedbackMsg);
            return 1;
        }
        region.getGroup(groupName).clearTeams();
        MutableText feedbackMsg = Text.translatableWithFallback("cli.msg.info.region.teams.cleared", "Cleared %s teams of group '%s' for %s", buildRegionInfoLink(region), amount, groupName);
        sendCmdFeedback(ctx.getSource(), feedbackMsg);
        RegionDataManager.save();
        return 0;
    }


    public static int clearGroups(CommandContext<ServerCommandSource> ctx, IProtectedRegion region, String groupName) {
        return CommandUtil.clearTeams(ctx, region, groupName) + CommandUtil.clearPlayers(ctx, region, groupName);
    }

    public static int addAllFlags(CommandContext<ServerCommandSource> ctx, IProtectedRegion region) {
        return addFlags(ctx, region, RegionFlag.getFlags());
    }

    public static int addFlag(CommandContext<ServerCommandSource> ctx, IProtectedRegion region, RegionFlag flag) {
        return addFlag(ctx, region, flag, FlagState.DENIED, false);
    }

    public static int addFlag(CommandContext<ServerCommandSource> ctx, IProtectedRegion region, RegionFlag flag, FlagState state, boolean override) {
        return addRegionFlag(ctx, region, flag, state, override);
    }

    public static int addFlags(CommandContext<ServerCommandSource> ctx, IProtectedRegion region, Set<RegionFlag> flags) {
        flags.forEach(flag -> CommandUtil.addRegionFlag(ctx, region, flag));
        return 0;
    }

    public static int addRegionFlag(CommandContext<ServerCommandSource> ctx, IProtectedRegion region, RegionFlag flag, FlagState state, boolean override) {
        if (region.getRegionType() == RegionType.LOCAL && flag == RegionFlag.ENTER_DIM) {
            MutableText msg = Text.literal("Flag 'enter-dim' is currently not supported for local regions.");
            sendCmdFeedback(ctx.getSource(), msg);
            return 1;
        }
        if (!region.containsFlag(flag)) {
            IFlag iFlag;
            switch (flag.type) {
                case BOOLEAN_FLAG:
                    iFlag = new BooleanFlag(flag, state, override);
                    break;
                case LIST_FLAG:
                case INT_FLAG:
                    throw new NotImplementedException("Not yet implemented = " + flag.name);
                default:
                    throw new IllegalArgumentException("Unexpected value = " + flag.getClass().getName());
            }
            if (flag.name.contains("spawning") && FlagConfig.removeEntitiesEnabled()) {
                removeInvolvedEntities(ctx, region, flag);
            }
            region.addFlag(iFlag);
            RegionDataManager.save();
            MutableText flagLink = buildFlagInfoLink(region, iFlag);
            MutableText msg = Text.translatableWithFallback("cli.msg.flag.added", "Added flag '%s' to %s",
                    flagLink, buildRegionInfoLink(region));
            MutableText undoLink = buildRegionActionUndoLink(ctx.getInput(), ADD, REMOVE);
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("%s %s", "%s %s", msg, undoLink));
            return 0;
        } else {
            MutableText msg = Text.translatableWithFallback("cli.msg.flag.present", "Flag '%s' is already present in %s", flag.name,
                    buildRegionInfoLink(region));
            sendCmdFeedback(ctx.getSource(), msg);
            return 1;
        }
    }

    public static int addRegionFlag(CommandContext<ServerCommandSource> ctx, IProtectedRegion region, RegionFlag flag) {
        return addRegionFlag(ctx, region, flag, FlagState.DENIED, false);
    }

    public static void removeInvolvedEntities(CommandContext<ServerCommandSource> ctx, IProtectedRegion region, RegionFlag flag) {
        RegistryKey<World> dimKey = RegistryKey.of(RegistryKeys.WORLD, region.getDim().getValue());
        MinecraftServer server = ctx.getSource().getServer();
        Predicate<? super Entity> entityFilter = getEntityFilterForFlag(flag);
        switch (region.getRegionType()) {
            case GLOBAL: {
                server.getWorlds().forEach(world -> {
                    List<Entity> entitiesToRemove = getEntitiesToRemove(world, entityFilter, flag);
                    entitiesToRemove.forEach(e -> e.setRemoved(Entity.RemovalReason.DISCARDED));
                });
            }
            break;
            case DIMENSION: {
                ServerWorld regionWorld = server.getWorld(dimKey);
                if (regionWorld != null) {
                    List<Entity> entitiesToRemove = getEntitiesToRemove(regionWorld, entityFilter, flag);
                    entitiesToRemove.forEach(e -> e.setRemoved(Entity.RemovalReason.DISCARDED));
                }
            }
            break;
            case LOCAL: {
                ServerWorld regionWorld = server.getWorld(dimKey);
                if (regionWorld != null) {
                    List<Entity> entitiesToRemove = getEntitiesToRemove(regionWorld, (IMarkableRegion) region, entityFilter);
                    entitiesToRemove.forEach(e -> e.setRemoved(Entity.RemovalReason.DISCARDED));
                }
            }
            break;
        }
    }

    private static boolean hasEnabledPersistenceFlag(Entity e) {
        if (e instanceof MobEntity mob) {
            return mob.isPersistent();
        }
        return false;
    }

    private static boolean isNotPersistent(Entity e) {
        return !hasEnabledPersistenceFlag(e) && !e.hasCustomName();
    }

    private static Predicate<? super Entity> getEntityFilterForFlag(RegionFlag flag) {
        switch (flag) {
            case SPAWNING_ALL:
                return e -> !(e instanceof PlayerEntity);
            case SPAWNING_MONSTER:
                return HandlerUtil::isMonster;
            case SPAWNING_ANIMAL:
                return HandlerUtil::isAnimal;
            case SPAWNING_GOLEM:
                return e -> e instanceof SnowGolemEntity || e instanceof IronGolemEntity;
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

    /**
     * Get all entities in the region which are not persistent and match the entityFilter
     */
    private static List<Entity> getEntitiesToRemove(ServerWorld level, IMarkableRegion region, Predicate<? super Entity> entityFilter) {
        // TODO: could be optimized by getting the chunks around the area only to check
        List<? extends Entity> entities = level.getEntitiesByType(TypeFilter.instanceOf(Entity.class), entityFilter);
        return entities.stream()
                .filter(e -> region.getArea().containsOther(new CuboidArea(e.getBoundingBox())))
                .filter(CommandUtil::isNotPersistent)
                .collect(Collectors.toList());
    }

    private static List<Entity> getEntitiesToRemove(ServerWorld level, Predicate<? super Entity> entityFilter, RegionFlag flag) {
        List<? extends Entity> entities = level.getEntitiesByType(TypeFilter.instanceOf(Entity.class), entityFilter);
        return entities.stream()
                .filter(e -> !isProtectedByRegion(level, flag, e)) // That's O(enemyCount * regionCount) complexity, not considering the recursion for the flag check
                .filter(CommandUtil::isNotPersistent)
                .collect(Collectors.toList());
    }

    private static boolean isProtectedByRegion(ServerWorld level, RegionFlag flag, Entity e) {
        FlagCheckEvent checkEvent = new FlagCheckEvent(e.getBlockPos(), flag, level.getRegistryKey(), null);
        FlagState flagState = processCheck(checkEvent, null, null);
        return flagState == FlagState.ALLOWED;
    }

    public static int promptRegionInfo(CommandContext<ServerCommandSource> ctx, IProtectedRegion region) {
        // == Region [<name>] overview ==
        sendCmdFeedback(ctx.getSource(), buildRegionOverviewHeader(region));
        // Flags: [n] | [m] flag(s)] [+]
        sendCmdFeedback(ctx.getSource(), buildInfoComponent("cli.msg.info.region.flag", "Flags", buildFlagsListLink(region)));
        if (region.getRegionType() == RegionType.LOCAL) {
            IMarkableRegion markableRegion = (IMarkableRegion) region;
            // Area: [Area]
            sendCmdFeedback(ctx.getSource(), buildInfoComponent("cli.msg.info.region.area", "Area", buildRegionAreaLink(markableRegion)));
        }
        // Groups: [owners], [members], [<listGroups>]
        sendCmdFeedback(ctx.getSource(), buildInfoComponent("cli.msg.info.region.group", "Groups", buildGroupLinks(region)));
        // Regions: [global], [n children], [n regions][+],
        // Dimensions: [n dimensions(s)]
        // Parent: [parent][x], [n children][+]
        promptRegionChildrenInfo(ctx, region);
        // State: [State]
        sendCmdFeedback(ctx.getSource(), buildInfoComponent("cli.msg.info.region.state", "State", buildRegionStateLink(region)));
        return 0;
    }

    public static void promptRegionChildrenInfo(CommandContext<ServerCommandSource> ctx, IProtectedRegion region) {
        MutableText listChildrenLink = buildRegionListChildrenLink(region);
        switch (region.getRegionType()) {
            case GLOBAL: {
                // Dimensions: [n dimensions(s)]
                sendCmdFeedback(ctx.getSource(), buildInfoComponent("cli.msg.info.dimensions", "Dimensions", listChildrenLink));
            }
            break;
            case DIMENSION: {
                // Parent: [global], [n children], [n regions] [+]
                MutableText globalRegionLink = buildRegionInfoLink(region.getParent(), Text.translatableWithFallback("cli.msg.info.region.global.link.hover", "Show global region info"));
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(region.getDim());
                MutableText hierarchyLinks = Text.translatable("%s, %s, %s", globalRegionLink, buildDimRegionsLink(dimCache), listChildrenLink);
                sendCmdFeedback(ctx.getSource(), buildInfoComponent("cli.msg.info.region.hierarchy", "Hierarchy", hierarchyLinks));
            }
            break;
            case LOCAL: {
                // Parent: [parent] [x], [n children] [+]
                MutableText parentClearLink = buildRegionRemoveChildLink(region.getParent(), region); // buildParentClearLink((IMarkableRegion) region);
                MutableText hierarchyLinks = Text.literal("");
                if (region.getParent().getRegionType() == RegionType.DIMENSION) {
                    // don't show removal link, since it's not possible to remove the parent
                    hierarchyLinks = Text.translatableWithFallback("%s, %s", "%s, %s", buildRegionInfoLink(region.getParent()), listChildrenLink);
                }
                if (region.getParent().getRegionType() == RegionType.LOCAL) {
                    hierarchyLinks = Text.translatableWithFallback("%s %s, %s", "%s %s, %s", buildRegionInfoLink(region.getParent()), parentClearLink, listChildrenLink);
                }
                sendCmdFeedback(ctx.getSource(), buildInfoComponent("cli.msg.info.region.parent", "Parent", hierarchyLinks));
            }
            break;
            case TEMPLATE:
                break;
            default:
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        }
    }

}
