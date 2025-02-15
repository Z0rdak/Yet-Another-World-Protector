package de.z0rdak.yawp.commands;

import com.mojang.authlib.GameProfile;
import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import de.z0rdak.yawp.api.commands.CommandConstants;
import de.z0rdak.yawp.api.events.flag.FlagEvent;
import de.z0rdak.yawp.api.permission.Permissions;
import de.z0rdak.yawp.commands.arguments.flag.IFlagArgumentType;
import de.z0rdak.yawp.commands.arguments.region.RegionArgumentType;
import de.z0rdak.yawp.core.flag.BooleanFlag;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.group.GroupType;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.core.region.RegionType;
import de.z0rdak.yawp.data.region.RegionDataManager;
import de.z0rdak.yawp.platform.Services;
import de.z0rdak.yawp.util.ChatLinkBuilder;
import de.z0rdak.yawp.util.MojangApiHelper;
import de.z0rdak.yawp.util.text.Messages;
import de.z0rdak.yawp.util.text.messages.multiline.MultiLineMessage;
import de.z0rdak.yawp.util.text.messages.pagination.*;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.SharedSuggestionProvider;
import net.minecraft.commands.arguments.DimensionArgument;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.commands.arguments.TeamArgument;
import net.minecraft.commands.arguments.UuidArgument;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.scores.Team;
import org.apache.commons.lang3.NotImplementedException;
import org.apache.commons.lang3.StringUtils;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.function.Function;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.api.commands.CommandConstants.*;
import static de.z0rdak.yawp.commands.arguments.ArgumentUtil.*;
import static de.z0rdak.yawp.util.ChatComponentBuilder.*;
import static de.z0rdak.yawp.api.MessageSender.sendCmdFeedback;
import static de.z0rdak.yawp.api.MessageSender.sendError;
import static net.minecraft.ChatFormatting.RED;

public class CommandUtil {

    public static LiteralArgumentBuilder<CommandSourceStack> buildClearSubCommand(Function<CommandContext<CommandSourceStack>, IProtectedRegion> regionSupplier) {
        return literal(CLEAR)
                .then(literal(FLAGS)
                        .executes(ctx -> CommandUtil.clearFlags(ctx, regionSupplier.apply(ctx)))
                )
                .then(literal(PLAYERS)
                        .executes(ctx -> CommandUtil.clearPlayers(ctx, regionSupplier.apply(ctx)))
                        .then(Commands.argument(GROUP.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(Permissions.GROUP_LIST, builder))
                                .executes(ctx -> CommandUtil.clearPlayers(ctx, regionSupplier.apply(ctx), getGroupArgument(ctx))))
                )
                .then(literal(TEAMS)
                        .executes(ctx -> CommandUtil.clearTeams(ctx, regionSupplier.apply(ctx)))
                        .then(Commands.argument(GROUP.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(Permissions.GROUP_LIST, builder))
                                .executes(ctx -> CommandUtil.clearTeams(ctx, regionSupplier.apply(ctx), getGroupArgument(ctx))))
                )
                .then(literal(GROUP)
                        .then(Commands.argument(GROUP.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(Permissions.GROUP_LIST, builder))
                                .executes(ctx -> CommandUtil.clearGroups(ctx, regionSupplier.apply(ctx), getGroupArgument(ctx))))
                );
    }

    public static LiteralArgumentBuilder<CommandSourceStack> buildRemoveSubCommand(Function<CommandContext<CommandSourceStack>, IProtectedRegion> regionSupplier) {
        return literal(REMOVE)
                .then(literal(PLAYER)
                        .then(Commands.argument(GROUP.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(Permissions.GROUP_LIST, builder))
                                .then(Commands.argument(PLAYER.toString(), EntityArgument.players())
                                        .executes(ctx -> removePlayers(ctx, getPlayersArgument(ctx), regionSupplier.apply(ctx), getGroupArgument(ctx))))
                                .then(literal(BY_UUID)
                                        .then(Commands.argument(PLAYER_UUID.toString(), UuidArgument.uuid())
                                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(regionSupplier.apply(ctx).getGroup(getGroupArgument(ctx)).getPlayers().keySet().stream().map(UUID::toString).collect(Collectors.toList()), builder))
                                                .executes(ctx -> removePlayerByUUID(ctx, getPlayerUUIDArgument(ctx), regionSupplier.apply(ctx), getGroupArgument(ctx)))))
                                .then(literal(BY_NAME)
                                        .then(Commands.argument(PLAYER_NAMES.toString(), StringArgumentType.greedyString())
                                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(regionSupplier.apply(ctx).getGroup(getGroupArgument(ctx)).getPlayers().values(), builder))
                                                .executes(ctx -> removePlayersByName(ctx, getPlayerNamesArgument(ctx), regionSupplier.apply(ctx), getGroupArgument(ctx)))))
                        )
                )
                .then(literal(TEAM)
                        .then(Commands.argument(GROUP.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(Permissions.GROUP_LIST, builder))
                                .then(Commands.argument(TEAM.toString(), TeamArgument.team())
                                        .executes(ctx -> removeTeam(ctx, getTeamArgument(ctx), regionSupplier.apply(ctx), getGroupArgument(ctx))))))
                .then(literal(FLAG)
                        .then(Commands.argument(FLAG.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> IFlagArgumentType.flag().listSuggestions(ctx, builder))
                                .executes(ctx -> removeRegionFlag(ctx, regionSupplier.apply(ctx), getFlagArgument(ctx)))
                        )
                )
                .then(literal(FLAGS)
                        .then(Commands.argument(FLAGS.toString(), StringArgumentType.greedyString())
                                .suggests((ctx, builder) -> IFlagArgumentType.flag().listSuggestions(ctx, builder))
                                .executes(ctx -> removeFlags(ctx, regionSupplier.apply(ctx), getFlagArguments(ctx))))
                );
    }

    public static LiteralArgumentBuilder<CommandSourceStack> buildAddSubCommand(Function<CommandContext<CommandSourceStack>, IProtectedRegion> regionSupplier) {
        return literal(ADD)
                .then(literal(PLAYER)
                        .then(Commands.argument(GROUP.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(Permissions.GROUP_LIST, builder))
                                .then(Commands.argument(PLAYER.toString(), EntityArgument.players())
                                        .executes(ctx -> addPlayers(ctx, getPlayersArgument(ctx), regionSupplier.apply(ctx), getGroupArgument(ctx))))
                                .then(literal(BY_UUID)
                                        .then(Commands.argument(PLAYER_UUID.toString(), UuidArgument.uuid())
                                                .executes(ctx -> addPlayerByUuid(ctx, getPlayerUUIDArgument(ctx), regionSupplier.apply(ctx), getGroupArgument(ctx)))))
                                .then(literal(BY_NAME)
                                        .then(Commands.argument(PLAYER_NAMES.toString(), StringArgumentType.greedyString())
                                                .executes(ctx -> addPlayersByName(ctx, getPlayerNamesArgument(ctx), regionSupplier.apply(ctx), getGroupArgument(ctx)))))
                        )
                )
                .then(literal(TEAM)
                        .then(Commands.argument(GROUP.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(Permissions.GROUP_LIST, builder))
                                .then(Commands.argument(TEAM.toString(), TeamArgument.team())
                                        .executes(ctx -> addTeam(ctx, getTeamArgument(ctx), regionSupplier.apply(ctx), getGroupArgument(ctx))))))
                .then(literal(FLAG)
                        .then(Commands.argument(FLAG.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> IFlagArgumentType.flag().listSuggestions(ctx, builder))
                                .executes(ctx -> addFlag(ctx, regionSupplier.apply(ctx), getFlagArgument(ctx)))
                                .then(Commands.argument(STATE.toString(), StringArgumentType.word())
                                        .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(FlagState.ValidFlagStates(), builder))
                                        .executes(ctx -> addFlag(ctx, regionSupplier.apply(ctx), getFlagArgument(ctx), getFlagStateArgument(ctx), false))
                                        .then(Commands.argument(OVERRIDE.toString(), BoolArgumentType.bool())
                                                .executes(ctx -> addFlag(ctx, regionSupplier.apply(ctx), getFlagArgument(ctx), getFlagStateArgument(ctx), getOverrideArgument(ctx))))
                                )
                        )
                )
                .then(literal(FLAGS)
                        .then(Commands.argument(FLAGS.toString(), StringArgumentType.greedyString())
                                .suggests((ctx, builder) -> IFlagArgumentType.flag().listSuggestions(ctx, builder))
                                .executes(ctx -> addFlags(ctx, regionSupplier.apply(ctx), getFlagArguments(ctx))))
                )
                .then(literal(ALL_FLAGS)
                        .executes(ctx -> addAllFlags(ctx, regionSupplier.apply(ctx)))
                );
    }

    public static LiteralArgumentBuilder<CommandSourceStack> buildListSubCommand(Function<CommandContext<CommandSourceStack>, IProtectedRegion> regionSupplier) {
        return literal(LIST)
                .then(literal(CHILDREN)
                        .executes(ctx -> promptRegionChildren(ctx, regionSupplier.apply(ctx), 0))
                        .then(Commands.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                .executes(ctx -> promptRegionChildren(ctx, regionSupplier.apply(ctx), getPageNoArgument(ctx)))))
                .then(literal(FLAG)
                        .executes(ctx -> promptFlagList(ctx, regionSupplier.apply(ctx), 0))
                        .then(Commands.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                .executes(ctx -> promptFlagList(ctx, regionSupplier.apply(ctx), getPageNoArgument(ctx)))))
                .then(literal(REGION_FLAG)
                        .executes(ctx -> promptRegionFlagList(ctx, regionSupplier.apply(ctx), 0))
                        .then(Commands.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                .executes(ctx -> promptRegionFlagList(ctx, regionSupplier.apply(ctx), getPageNoArgument(ctx)))))
                .then(literal(GROUP)
                        .then(Commands.argument(GROUP.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(Permissions.GROUP_LIST, builder))
                                .executes(ctx -> CommandUtil.promptGroupLinks(ctx, regionSupplier.apply(ctx), getGroupArgument(ctx)))
                                .then(literal(TEAM)
                                        .executes(ctx -> CommandUtil.promptGroupList(ctx, regionSupplier.apply(ctx), getGroupArgument(ctx), GroupType.TEAM, 0))
                                        .then(Commands.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                                .executes(ctx -> CommandUtil.promptGroupList(ctx, regionSupplier.apply(ctx), getGroupArgument(ctx), GroupType.TEAM, getPageNoArgument(ctx))))
                                )
                                .then(literal(PLAYER)
                                        .executes(ctx -> CommandUtil.promptGroupList(ctx, regionSupplier.apply(ctx), getGroupArgument(ctx), GroupType.PLAYER, 0))
                                        .then(Commands.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                                .executes(ctx -> CommandUtil.promptGroupList(ctx, regionSupplier.apply(ctx), getGroupArgument(ctx), GroupType.PLAYER, getPageNoArgument(ctx))))
                                )
                        )
                );
    }

    public static LiteralArgumentBuilder<CommandSourceStack> buildCopySubCommand(Function<CommandContext<CommandSourceStack>, IProtectedRegion> srcSupplier) {
        return literal(COPY)
                .then(literal(FLAGS)
                        .then(literal(TO_LOCAL)
                                .then(Commands.argument(TARGET_DIM.toString(), DimensionArgument.dimension())
                                        .then(Commands.argument(TARGET_REGION.toString(), StringArgumentType.word())
                                                .suggests((ctx, builder) -> RegionArgumentType.region().listRegionsInTargetDim(ctx, builder))
                                                .executes(ctx -> copyRegionFlags(ctx, srcSupplier.apply(ctx), getTargetLocalRegionArgument(ctx))))))
                        .then(literal(TO_DIM)
                                .then(Commands.argument(TARGET_DIM.toString(), DimensionArgument.dimension())
                                        .executes(ctx -> copyRegionFlags(ctx, srcSupplier.apply(ctx), getTargetDimRegionArgument(ctx).getDimensionalRegion()))))
                )
                .then(literal(PLAYERS)
                        .then(literal(TO_LOCAL)
                                .then(Commands.argument(TARGET_DIM.toString(), DimensionArgument.dimension())
                                        .then(Commands.argument(TARGET_REGION.toString(), StringArgumentType.word())
                                                .suggests((ctx, builder) -> RegionArgumentType.region().listRegionsInTargetDim(ctx, builder))
                                                .then(Commands.argument(GROUP.toString(), StringArgumentType.word())
                                                        .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(Permissions.GROUP_LIST, builder))
                                                        .executes(ctx -> copyRegionPlayers(ctx, srcSupplier.apply(ctx), getTargetLocalRegionArgument(ctx), getGroupArgument(ctx)))))))
                        .then(literal(TO_DIM)
                                .then(Commands.argument(TARGET_DIM.toString(), DimensionArgument.dimension())
                                        .then(Commands.argument(GROUP.toString(), StringArgumentType.word())
                                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(Permissions.GROUP_LIST, builder))
                                                .executes(ctx -> copyRegionPlayers(ctx, srcSupplier.apply(ctx), getTargetDimRegionArgument(ctx).getDimensionalRegion(), getGroupArgument(ctx)))))))
                .then(literal(STATE)
                        .then(literal(TO_LOCAL)
                                .then(Commands.argument(TARGET_DIM.toString(), DimensionArgument.dimension())
                                        .then(Commands.argument(TARGET_REGION.toString(), StringArgumentType.word())
                                                .suggests((ctx, builder) -> RegionArgumentType.region().listRegionsInTargetDim(ctx, builder))
                                                .executes(ctx -> copyRegionState(ctx, srcSupplier.apply(ctx), getTargetLocalRegionArgument(ctx))))))
                        .then(literal(TO_DIM)
                                .then(Commands.argument(TARGET_DIM.toString(), DimensionArgument.dimension())
                                        .executes(ctx -> copyRegionState(ctx, srcSupplier.apply(ctx), getTargetDimRegionArgument(ctx).getDimensionalRegion()))))
                );
    }

    public static int promptRegionInfo(CommandContext<CommandSourceStack> ctx, IProtectedRegion region) {
        MultiLineMessage.send(ctx.getSource(),  MultiLineMessage.regionInfo(region));
        return 0;
    }

    public static int promptRegionState(CommandContext<CommandSourceStack> ctx, IProtectedRegion region) {
        MultiLineMessage.send(ctx.getSource(), MultiLineMessage.regionState(region));
        return 0;
    }

    /**
     * == Group '%s' for '%s'==
     * Players: [n player(s)][+]
     * Teams: [m team(s)][+]
     */
    public static int promptGroupLinks(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, String group) {
        if (!Permissions.GROUP_LIST.contains(group)) {
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.region.info.group.invalid", "cli.msg.region.info.group.invalid", group).withStyle(RED));
            return -1;
        }
        sendCmdFeedback(ctx.getSource(), buildGroupListHeader(region, group));
        sendCmdFeedback(ctx.getSource(), ChatLinkBuilder.buildGroupPlayerListLink(region, group));
        sendCmdFeedback(ctx.getSource(), ChatLinkBuilder.buildGroupTeamListLink(region, group));
        return 0;
    }

    /**
     * List of flags of the provided region and its parents
     */
    public static int promptFlagList(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, int pageNo) {
        try {
            int paginationSize = Services.REGION_CONFIG.getPaginationSize();
            ResponsibleFlagPagination flagPagination = new ResponsibleFlagPagination(region, pageNo, paginationSize);
            MultiLineMessage.send(ctx.getSource(), flagPagination);
        } catch (InvalidPageNumberException e) {
            sendError(ctx.getSource(), e.getError());
            return -1;
        }
        return 0;
    }

    /**
     * List of flags only of the provided region
     */
    public static int promptRegionFlagList(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, int pageNo) {
        try {
            int paginationSize = Services.REGION_CONFIG.getPaginationSize();
            RegionFlagPagination flagPagination = new RegionFlagPagination(region, pageNo, paginationSize);
            MultiLineMessage.send(ctx.getSource(), flagPagination);
        } catch (InvalidPageNumberException e) {
            sendError(ctx.getSource(), e.getError());
            return -1;
        }
        return 0;
    }

    public static int promptRegionChildren(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, int pageNo) {
        try {
            int paginationSize = Services.REGION_CONFIG.getPaginationSize();
            ChildRegionPagination childRegionPagination = new ChildRegionPagination(region, region.getChildren().values().stream().toList(), pageNo, paginationSize);
            MultiLineMessage.send(ctx.getSource(), childRegionPagination);
        } catch (InvalidPageNumberException e) {
            sendError(ctx.getSource(), e.getError());
            return -1;
        }

        return 0;
    }

    public static int promptGroupList(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, String group, GroupType groupType, int pageNo) {
        if (!Permissions.GROUP_LIST.contains(group)) {
            sendError(ctx.getSource(), buildInvalidGroupMsg(group));
            return -1;
        }
        List<String> groupNames = getGroupList(region, group, groupType);
        try {
            int paginationSize = Services.REGION_CONFIG.getPaginationSize();
            GroupMemberPagination childRegionPagination = new GroupMemberPagination(region, group, groupType, groupNames, pageNo, paginationSize);
            MultiLineMessage.send(ctx.getSource(), childRegionPagination);
        } catch (InvalidPageNumberException e) {
            sendError(ctx.getSource(), e.getError());
            return -1;
        }
        return 0;
    }

    public static int setActiveState(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, boolean activate) {
        region.setIsActive(activate);
        RegionDataManager.save();
        MutableComponent undoLink = ChatLinkBuilder.buildRegionActionUndoLink(ctx.getInput(), String.valueOf(!activate), String.valueOf(activate));
        MutableComponent msg = Component.translatableWithFallback("cli.msg.info.region.state.enable.set.value", "Region state of %s is now: %s",
                ChatLinkBuilder.buildRegionInfoLink(region), region.isActive() ? "active" : "inactive");
        sendCmdFeedback(ctx.getSource(), Messages.substitutable("%s %s", msg, undoLink));
        return 0;
    }

    public static int setAlertState(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, boolean showAlert) {
        region.setIsMuted(!showAlert);
        RegionDataManager.save();
        MutableComponent undoLink = ChatLinkBuilder.buildRegionActionUndoLink(ctx.getInput(), String.valueOf(!showAlert), String.valueOf(showAlert));
        MutableComponent msg = Component.translatableWithFallback("cli.msg.info.state.alert.set.value", "Flag messages of %s are now: %s",
                ChatLinkBuilder.buildRegionInfoLink(region), region.isMuted() ? "muted" : "active");
        sendCmdFeedback(ctx.getSource(), Messages.substitutable("%s %s", msg, undoLink));
        return 0;
    }

    private static MutableComponent buildInvalidGroupMsg(String group) {
        return Component.translatableWithFallback("cli.msg.region.info.group.invalid", "Group '%s' is not defined!", group);
    }

    public static int removeTeam(CommandContext<CommandSourceStack> ctx, Team team, IProtectedRegion region, String group) {
        if (!Permissions.GROUP_LIST.contains(group)) {
            sendError(ctx.getSource(), buildInvalidGroupMsg(group));
            return -1;
        }
        MutableComponent undoLink = ChatLinkBuilder.buildRegionActionUndoLink(ctx.getInput(), REMOVE, ADD);
        MutableComponent teamInfo = buildGroupInfo(region, team.getName(), GroupType.TEAM);
        if (region.getGroup(group).hasTeam(team.getName())) {
            region.removeTeam(team.getName(), group);
            RegionDataManager.save();
            MutableComponent msg = Component.translatableWithFallback("cli.msg.info.region.group.team.removed", "Removed team '%s' (group '%s') from %s", teamInfo, group,
                    ChatLinkBuilder.buildRegionInfoLink(region));
            sendCmdFeedback(ctx.getSource(), Messages.substitutable("%s %s", msg, undoLink));
            return 0;
        }
        MutableComponent msg = Component.translatableWithFallback("cli.msg.info.region.group.team.not-present", "Team '%s' (group '%s') is not present in %s", teamInfo, group,
                ChatLinkBuilder.buildRegionInfoLink(region));
        sendCmdFeedback(ctx.getSource(), msg);
        return 1;
    }


    private static int removePlayersByName(CommandContext<CommandSourceStack> ctx, Collection<String> playerNames, IProtectedRegion region, String group) {
        if (!Permissions.GROUP_LIST.contains(group)) {
            sendError(ctx.getSource(), buildInvalidGroupMsg(group));
            return -1;
        }
        playerNames.forEach(playerName -> CommandUtil.removePlayer(ctx, playerName, region, group));
        return 0;
    }

    private static void removePlayer(CommandContext<CommandSourceStack> ctx, String playerName, IProtectedRegion region, String group) {
        region.getGroup(group).getPlayers().entrySet()
                .stream()
                .filter(e -> e.getValue().equals(playerName))
                .findFirst()
                .ifPresent(e -> removePlayer(ctx, e.getKey(), playerName, region, group));
    }

    private static int removePlayerByUUID(CommandContext<CommandSourceStack> ctx, UUID playerUuid, IProtectedRegion region, String group) {
        if (!Permissions.GROUP_LIST.contains(group)) {
            sendError(ctx.getSource(), buildInvalidGroupMsg(group));
            return -1;
        }
        if (region.getGroup(group).hasPlayer(playerUuid)) {
            String playerName = region.getGroup(group).getPlayers().get(playerUuid);
            return removePlayer(ctx, playerUuid, playerName, region, group);
        }
        return 1;
    }


    private static int removePlayers(CommandContext<CommandSourceStack> ctx, Collection<ServerPlayer> players, IProtectedRegion region, String group) {
        players.forEach(player -> CommandUtil.removePlayer(ctx, player, region, group));
        return 0;
    }

    private static int removePlayer(CommandContext<CommandSourceStack> ctx, Player player, IProtectedRegion region, String group) {
        if (!Permissions.GROUP_LIST.contains(group)) {
            sendError(ctx.getSource(), buildInvalidGroupMsg(group));
            return -1;
        }
        if (region.getGroup(group).hasPlayer(player.getUUID())) {
            String playerName = region.getGroup(group).getPlayers().get(player.getUUID());
            return removePlayer(ctx, player.getUUID(), playerName, region, group);
        }
        return 1;
    }

    public static int removePlayer(CommandContext<CommandSourceStack> ctx, UUID playerUuid, String playerName, IProtectedRegion region, String group) {
        MutableComponent playerInfo = buildGroupInfo(region, playerName, GroupType.PLAYER);
        MutableComponent undoLink = ChatLinkBuilder.buildRegionActionUndoLink(ctx.getInput(), REMOVE, ADD);
        if (region.getGroup(group).hasPlayer(playerUuid)) {
            region.removePlayer(playerUuid, group);
            MutableComponent msg = Component.translatableWithFallback("cli.msg.info.region.group.player.removed", "Removed player '%s' (group '%s') from %s", playerInfo, group,
                    ChatLinkBuilder.buildRegionInfoLink(region));
            sendCmdFeedback(ctx.getSource(), Messages.substitutable("%s %s", msg, undoLink));
            RegionDataManager.save();
            return 0;
        }

        MutableComponent msg = Component.translatableWithFallback("cli.msg.info.region.group.player.not-present", "Player '%s' (group '%s') is not present in %s", playerInfo, group,
                ChatLinkBuilder.buildRegionInfoLink(region));
        sendCmdFeedback(ctx.getSource(), msg);
        return 1;
    }

    private static int addPlayerByUuid(CommandContext<CommandSourceStack> ctx, UUID playerUuid, IProtectedRegion region, String group) {
        if (!Permissions.GROUP_LIST.contains(group)) {
            sendError(ctx.getSource(), buildInvalidGroupMsg(group));
            return -1;
        }
        Optional<GameProfile> cachedProfile = MojangApiHelper.lookupGameProfileInCache(ctx, playerUuid);
        if (cachedProfile.isPresent()) {
            GameProfile profile = cachedProfile.get();
            var isComplete = profile.getId() != null && StringUtils.isNotBlank(profile.getName());
            if (isComplete) {
                MutableComponent cacheSuccess = Component.translatableWithFallback("cli.msg.info.player.lookup.cache.success", "Found entry for '%s' in the cache", playerUuid.toString());
                sendCmdFeedback(ctx.getSource(), cacheSuccess);
                return addPlayer(ctx, profile.getId(), profile.getName(), region, group);
            }
            return -1;
        } else {
            MutableComponent cacheMiss = Component.translatableWithFallback("cli.msg.info.player.lookup.pending", "No cache entry existing for '%s'. Looking up %s at Mojang...",
                    playerUuid.toString(), "uuid");
            sendCmdFeedback(ctx.getSource(), cacheMiss);
            CompletableFuture.runAsync(() -> MojangApiHelper.getGameProfileInfo(playerUuid, (gameProfile) -> {
                if (gameProfile != null) {
                    MutableComponent lookupSuccess = Component.translatableWithFallback("cli.msg.info.player.lookup.api.success", "Successfully retrieved game profile info for '%s' from Mojang", playerUuid.toString());
                    sendCmdFeedback(ctx.getSource(), lookupSuccess);
                    addPlayer(ctx, gameProfile.getId(), gameProfile.getName(), region, group);
                } else {
                    MutableComponent lookupFailed = Component.translatableWithFallback("cli.msg.info.player.lookup.api.failed", "Unable to retrieve game profile info for '%s' from Mojang", playerUuid.toString());
                    sendCmdFeedback(ctx.getSource(), lookupFailed);
                }
            }));
            return 0;
        }
    }

    public static int addPlayersByName(CommandContext<CommandSourceStack> ctx, List<String> playerNames, IProtectedRegion region, String group) {
        if (!Permissions.GROUP_LIST.contains(group)) {
            sendError(ctx.getSource(), buildInvalidGroupMsg(group));
            return -1;
        }
        playerNames.forEach(name -> CommandUtil.addPlayerByName(ctx, name, region, group));
        return 0;
    }

    private static int addPlayerByName(CommandContext<CommandSourceStack> ctx, String playerName, IProtectedRegion region, String group) {
        Optional<GameProfile> cachedProfile = MojangApiHelper.lookupGameProfileInCache(ctx, playerName);
        if (cachedProfile.isPresent()) {
            GameProfile profile = cachedProfile.get();
            var isComplete = profile.getId() != null && StringUtils.isNotBlank(profile.getName());
            if (isComplete) {
                MutableComponent cacheSuccess = Component.translatableWithFallback("cli.msg.info.player.lookup.cache.success", "Found entry for '%s' in the cache", playerName);
                sendCmdFeedback(ctx.getSource(), cacheSuccess);
                return addPlayer(ctx, profile.getId(), profile.getName(), region, group);
            }
            return -1;
        } else {
            MutableComponent cacheMiss = Component.translatableWithFallback("cli.msg.info.player.lookup.pending", "No cache entry existing for '%s'. Looking up %s at Mojang...",
                    playerName, "name");
            sendCmdFeedback(ctx.getSource(), cacheMiss);
            CompletableFuture.runAsync(() -> MojangApiHelper.getGameProfileInfo(playerName, (gameProfile) -> {
                if (gameProfile != null) {
                    MutableComponent lookupSuccess = Component.translatableWithFallback("cli.msg.info.player.lookup.api.success", "Successfully retrieved game profile info for '%s' from Mojang", playerName);
                    sendCmdFeedback(ctx.getSource(), lookupSuccess);
                    addPlayer(ctx, gameProfile.getId(), gameProfile.getName(), region, group);
                } else {
                    MutableComponent lookupFailed = Component.translatableWithFallback("cli.msg.info.player.lookup.api.failed", "Unable to retrieve game profile info for '%s' from Mojang", playerName);
                    sendCmdFeedback(ctx.getSource(), lookupFailed);
                }
            }));
            return 0;
        }
    }

    public static int addPlayers(CommandContext<CommandSourceStack> ctx, Collection<ServerPlayer> players, IProtectedRegion region, String group) {
        players.forEach(player -> CommandUtil.addPlayer(ctx, player, region, group));
        return 0;
    }

    public static int addPlayer(CommandContext<CommandSourceStack> ctx, Player player, IProtectedRegion region, String group) {
        if (!Permissions.GROUP_LIST.contains(group)) {
            sendError(ctx.getSource(), buildInvalidGroupMsg(group));
            return -1;
        }
        return addPlayer(ctx, player.getUUID(), player.getScoreboardName(), region, group);
    }

    private static int addPlayer(CommandContext<CommandSourceStack> ctx, UUID uuid, String name, IProtectedRegion region, String group) {
        MutableComponent regionInfoLink = ChatLinkBuilder.buildRegionInfoLink(region);
        MutableComponent undoLink = ChatLinkBuilder.buildRegionActionUndoLink(ctx.getInput(), ADD, REMOVE);
        if (!region.hasPlayer(uuid, group)) {
            region.addPlayer(uuid, name, group);
            RegionDataManager.save();
            MutableComponent msg = Component.translatableWithFallback("cli.msg.info.region.group.player.added", "Added player '%s' as '%s' to %s", name, group, regionInfoLink);
            sendCmdFeedback(ctx.getSource(), Messages.substitutable("%s %s", msg, undoLink));
            return 0;
        }
        MutableComponent msg = Component.translatableWithFallback("cli.msg.info.region.group.player.present", "Player '%s' (group '%s') already present in %s", name, group, regionInfoLink);
        sendCmdFeedback(ctx.getSource(), msg);
        return 1;
    }

    public static int addTeam(CommandContext<CommandSourceStack> ctx, Team team, IProtectedRegion region, String group) {
        if (!Permissions.GROUP_LIST.contains(group)) {
            sendError(ctx.getSource(), buildInvalidGroupMsg(group));
            return -1;
        }
        MutableComponent regionInfoLink = ChatLinkBuilder.buildRegionInfoLink(region);
        MutableComponent teamHoverInfo = buildTeamHoverComponent(team);
        if (!region.hasTeam(team.getName(), group)) {
            region.addTeam(team.getName(), group);
            RegionDataManager.save();
            MutableComponent undoLink = ChatLinkBuilder.buildRegionActionUndoLink(ctx.getInput(), ADD, REMOVE);
            MutableComponent msg = Component.translatableWithFallback("cli.msg.info.region.group.team.added", "Added team '%s' as '%s' to region %s",
                    teamHoverInfo, group, regionInfoLink);
            sendCmdFeedback(ctx.getSource(), Messages.substitutable("%s %s", msg, undoLink));
            return 0;
        }
        MutableComponent msg = Component.translatableWithFallback("cli.msg.info.region.group.team.present", "Team '%s' (group '%s') already present in %s",
                teamHoverInfo, group, regionInfoLink);
        sendCmdFeedback(ctx.getSource(), msg);
        return 1;
    }

    public static int removeFlags(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, Set<RegionFlag> flags) {
        flags.forEach(flag -> CommandUtil.removeRegionFlag(ctx, region, flag));
        return 0;
    }

    public static int removeRegionFlag(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, RegionFlag flag) {
        if (region.containsFlag(flag)) {
            IFlag iFlag = region.getFlag(flag.name);
            FlagEvent.RemoveFlagEvent removeFlagEvent = new FlagEvent.RemoveFlagEvent(ctx.getSource(), region, iFlag);
            Services.EVENT.post(removeFlagEvent);
            region.removeFlag(flag.name);
            RegionDataManager.save();
            MutableComponent msg = Component.translatableWithFallback("cli.msg.flag.removed", "Removed flag '%s' from %s", flag.name,
                    ChatLinkBuilder.buildRegionInfoLink(region));
            MutableComponent undoLink = ChatLinkBuilder.buildRegionActionUndoLink(ctx.getInput(), REMOVE, ADD);
            sendCmdFeedback(ctx.getSource(), Messages.substitutable("%s %s", msg, undoLink));
            return 0;
        } else {
            MutableComponent msg = Component.translatableWithFallback("cli.msg.flag.not-present", "Flag '%s' is not present in %s", flag.name,
                    ChatLinkBuilder.buildRegionInfoLink(region));
            sendCmdFeedback(ctx.getSource(), msg);
            return 1;
        }
    }

    public static int copyRegionFlags(CommandContext<CommandSourceStack> ctx, IProtectedRegion srcRegion, IProtectedRegion targetRegion) {
        if (srcRegion.getFlags().isEmpty()) {
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.flag.empty", "No flags defined in %s", ChatLinkBuilder.buildRegionInfoLink(srcRegion)));
            return 1;
        }
        // get the flags which are in the srcRegion but not in the targetRegion
        Set<IFlag> flagsToCopy = srcRegion.getFlags().flags().stream()
                .filter(flag -> !targetRegion.containsFlag(flag.getName()))
                .collect(Collectors.toSet());
        // flagsToCopy.forEach(region::addFlag);
        srcRegion.getFlags().flags().forEach(targetRegion::addFlag);
        RegionDataManager.save();
        sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.copy.region.flags", "Copied %s flag(s) from region %s to %s", flagsToCopy.size(), ChatLinkBuilder.buildRegionInfoLink(srcRegion), ChatLinkBuilder.buildRegionInfoLink(targetRegion)));
        return 0;
    }

    public static int copyRegionState(CommandContext<CommandSourceStack> ctx, IProtectedRegion srcRegion, IProtectedRegion targetRegion) {
        targetRegion.setIsActive(srcRegion.isActive());
        targetRegion.setIsMuted(srcRegion.isMuted());
        if (srcRegion instanceof IMarkableRegion regionSource && targetRegion instanceof IMarkableRegion regionTarget) {
            regionTarget.setPriority(regionSource.getPriority());
        }
        RegionDataManager.save();
        sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.copy.region.state", "Copied state from region %s to %s", ChatLinkBuilder.buildRegionInfoLink(srcRegion), ChatLinkBuilder.buildRegionInfoLink(targetRegion)));
        return 0;
    }

    public static int copyRegionPlayers(CommandContext<CommandSourceStack> ctx, IProtectedRegion srcRegion, IProtectedRegion targetRegion, String group) {
        if (!srcRegion.getGroup(group).getPlayers().isEmpty()) {
            // get the players which are in the srcRegion but not in the targetRegion
            Set<Map.Entry<UUID, String>> playerEntriesToCopy = srcRegion.getGroup(group).getPlayers().entrySet().stream()
                    .filter(entry -> !targetRegion.getGroup(group).getPlayers().containsKey(entry.getKey()))
                    .collect(Collectors.toSet());
            //playerEntriesToCopy.forEach(entry -> region.getGroup(group).addPlayer(entry.getKey(), entry.getValue()));

            srcRegion.getGroup(group).getPlayers().forEach((uuid, name) -> targetRegion.getGroup(group).addPlayer(uuid, name));
            RegionDataManager.save();
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.copy.region.players", "Copied %s player(s) of group '%s' from %s to %s", playerEntriesToCopy.size(), group, ChatLinkBuilder.buildRegionInfoLink(srcRegion), ChatLinkBuilder.buildRegionInfoLink(targetRegion)));
            return 0;
        }
        sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.group.player.empty", "No players defined as '%s' in %s", group, ChatLinkBuilder.buildRegionInfoLink(srcRegion)));
        return 1;
    }

    public static int copyRegionPlayers(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, IProtectedRegion srcRegion) {
        return copyRegionPlayers(ctx, region, srcRegion, CommandConstants.MEMBER.toString())
                + copyRegionPlayers(ctx, region, srcRegion, CommandConstants.OWNER.toString());
    }

    public static int clearFlags(CommandContext<CommandSourceStack> ctx, IProtectedRegion region) {
        int amount = region.getFlags().size();
        if (amount == 0) {
            MutableComponent feedbackMsg = Component.translatableWithFallback("cli.msg.info.region.flag.empty", "No flags defined in %s", ChatLinkBuilder.buildRegionInfoLink(region));
            sendCmdFeedback(ctx.getSource(), feedbackMsg);
            return 1;
        }
        Collection<IFlag> flags = region.getFlags().flags(); // TODO: Undo action link
        region.getFlags().clear();
        MutableComponent feedbackMsg = Component.translatableWithFallback("cli.msg.info.region.flag.cleared", "Removed %s flag(s) from %s", amount, ChatLinkBuilder.buildRegionInfoLink(region));
        sendCmdFeedback(ctx.getSource(), feedbackMsg);
        RegionDataManager.save();
        return 0;
    }

    public static int clearPlayers(CommandContext<CommandSourceStack> ctx, IProtectedRegion region) {
        return clearPlayers(ctx, region, Permissions.MEMBER) + clearPlayers(ctx, region, Permissions.OWNER);
    }

    public static int clearPlayers(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, String groupName) {
        int amount = region.getGroup(groupName).getPlayers().size();
        if (amount == 0) {
            MutableComponent feedbackMsg = Component.translatableWithFallback("cli.msg.info.region.players.empty", "No players (group '%s') present in %s", ChatLinkBuilder.buildRegionInfoLink(region), groupName);
            sendCmdFeedback(ctx.getSource(), feedbackMsg);
            return 1;
        }
        region.getGroup(groupName).clearPlayers();
        MutableComponent feedbackMsg = Component.translatableWithFallback("cli.msg.info.region.players.cleared", "Cleared %s players of group '%s' for %s\",", ChatLinkBuilder.buildRegionInfoLink(region), amount, groupName);
        sendCmdFeedback(ctx.getSource(), feedbackMsg);
        RegionDataManager.save();
        return 0;
    }

    public static int clearTeams(CommandContext<CommandSourceStack> ctx, IProtectedRegion region) {
        return clearPlayers(ctx, region, Permissions.MEMBER) + clearPlayers(ctx, region, Permissions.OWNER);
    }

    public static int clearTeams(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, String groupName) {
        int amount = region.getGroup(groupName).getTeams().size();
        if (amount == 0) {
            MutableComponent feedbackMsg = Component.translatableWithFallback("cli.msg.info.region.teams.empty", "No teams (group '%s') present in %s", ChatLinkBuilder.buildRegionInfoLink(region), groupName);
            sendCmdFeedback(ctx.getSource(), feedbackMsg);
            return 1;
        }
        region.getGroup(groupName).clearTeams();
        MutableComponent feedbackMsg = Component.translatableWithFallback("cli.msg.info.region.teams.cleared", "Cleared %s teams of group '%s' for %s", ChatLinkBuilder.buildRegionInfoLink(region), amount, groupName);
        sendCmdFeedback(ctx.getSource(), feedbackMsg);
        RegionDataManager.save();
        return 0;
    }


    public static int clearGroups(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, String groupName) {
        return CommandUtil.clearTeams(ctx, region, groupName) + CommandUtil.clearPlayers(ctx, region, groupName);
    }

    public static int addAllFlags(CommandContext<CommandSourceStack> ctx, IProtectedRegion region) {
        return addFlags(ctx, region, RegionFlag.getFlags());
    }

    public static int addFlag(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, RegionFlag flag) {
        return addFlag(ctx, region, flag, FlagState.DENIED, false);
    }

    public static int addFlag(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, RegionFlag flag, FlagState state, boolean override) {
        return addRegionFlag(ctx, region, flag, state, override);
    }

    public static int addFlags(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, Set<RegionFlag> flags) {
        flags.forEach(flag -> CommandUtil.addRegionFlag(ctx, region, flag));
        return 0;
    }

    public static int addRegionFlag(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, RegionFlag flag, FlagState state, boolean override) {
        if (region.getRegionType() == RegionType.LOCAL && flag == RegionFlag.ENTER_DIM) {
            MutableComponent msg = Component.literal("Flag 'enter-dim' is currently not supported for local regions.");
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

            FlagEvent.AddFlagEvent addFlagEvent = new FlagEvent.AddFlagEvent(ctx.getSource(), region, iFlag);
            Services.EVENT.post(addFlagEvent);
            region.addFlag(iFlag);
            RegionDataManager.save();
            MutableComponent flagLink = ChatLinkBuilder.buildFlagInfoLink(region, iFlag);
            MutableComponent msg = Component.translatableWithFallback("cli.msg.flag.added", "Added flag '%s' to %s",
                    flagLink, ChatLinkBuilder.buildRegionInfoLink(region));
            MutableComponent undoLink = ChatLinkBuilder.buildRegionActionUndoLink(ctx.getInput(), ADD, REMOVE);
            sendCmdFeedback(ctx.getSource(), Messages.substitutable("%s %s", msg, undoLink));
            return 0;
        } else {
            MutableComponent msg = Component.translatableWithFallback("cli.msg.flag.present", "Flag '%s' is already present in %s", flag.name,
                    ChatLinkBuilder.buildRegionInfoLink(region));
            sendCmdFeedback(ctx.getSource(), msg);
            return 1;
        }
    }

    public static int addRegionFlag(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, RegionFlag flag) {
        return addRegionFlag(ctx, region, flag, FlagState.DENIED, false);
    }
}
