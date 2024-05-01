package de.z0rdak.yawp.commands;

import com.mojang.authlib.GameProfile;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import de.z0rdak.yawp.commands.arguments.flag.IFlagArgumentType;
import de.z0rdak.yawp.commands.arguments.region.RegionArgumentType;
import de.z0rdak.yawp.config.server.FlagConfig;
import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.flag.BooleanFlag;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.group.GroupType;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.core.region.RegionType;
import de.z0rdak.yawp.handler.flags.HandlerUtil;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.LocalRegions;
import de.z0rdak.yawp.util.MessageUtil;
import de.z0rdak.yawp.util.MojangApiHelper;
import net.minecraft.ChatFormatting;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.SharedSuggestionProvider;
import net.minecraft.commands.arguments.DimensionArgument;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.commands.arguments.TeamArgument;
import net.minecraft.commands.arguments.UuidArgument;
import net.minecraft.core.Registry;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.resources.ResourceKey;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.ExperienceOrb;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.animal.AbstractGolem;
import net.minecraft.world.entity.animal.IronGolem;
import net.minecraft.world.entity.animal.SnowGolem;
import net.minecraft.world.entity.monster.Slime;
import net.minecraft.world.entity.npc.WanderingTrader;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.minecraft.world.scores.Team;
import org.apache.commons.lang3.NotImplementedException;

import java.util.*;
import java.util.concurrent.CompletableFuture;
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

    public static LiteralArgumentBuilder<CommandSourceStack> buildClearSubCommand(Function<CommandContext<CommandSourceStack>, IProtectedRegion> regionSupplier) {
        return literal(CLEAR)
                .then(literal(FLAGS)
                        .executes(ctx -> CommandUtil.clearFlags(ctx, regionSupplier.apply(ctx)))
                )
                .then(literal(PLAYERS)
                        .executes(ctx -> CommandUtil.clearPlayers(ctx, regionSupplier.apply(ctx)))
                        .then(Commands.argument(GROUP.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(GROUP_LIST, builder))
                                .executes(ctx -> CommandUtil.clearPlayers(ctx, regionSupplier.apply(ctx), getGroupArgument(ctx))))
                )
                .then(literal(TEAMS)
                        .executes(ctx -> CommandUtil.clearTeams(ctx, regionSupplier.apply(ctx)))
                        .then(Commands.argument(GROUP.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(GROUP_LIST, builder))
                                .executes(ctx -> CommandUtil.clearTeams(ctx, regionSupplier.apply(ctx), getGroupArgument(ctx))))
                )
                .then(literal(GROUP)
                        .then(Commands.argument(GROUP.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(GROUP_LIST, builder))
                                .executes(ctx -> CommandUtil.clearGroups(ctx, regionSupplier.apply(ctx), getGroupArgument(ctx))))
                );
    }

    public static LiteralArgumentBuilder<CommandSourceStack> buildRemoveSubCommand(Function<CommandContext<CommandSourceStack>, IProtectedRegion> regionSupplier) {
        return literal(REMOVE)
                .then(literal(PLAYER)
                        .then(Commands.argument(GROUP.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(GROUP_LIST, builder))
                                .then(Commands.argument(PLAYER.toString(), EntityArgument.players())
                                        .executes(ctx -> removePlayers(ctx, getPlayersArgument(ctx), regionSupplier.apply(ctx), getGroupArgument(ctx))))
                                .then(literal(BY_UUID)
                                        .then(Commands.argument(PLAYER_UUID.toString(), UuidArgument.uuid())
                                                .executes(ctx -> removePlayerByUUID(ctx, getPlayerUUIDArgument(ctx), regionSupplier.apply(ctx), getGroupArgument(ctx)))))
                                .then(literal(BY_NAME)
                                        .then(Commands.argument(PLAYER_NAMES.toString(), StringArgumentType.greedyString())
                                                .executes(ctx -> removePlayersByName(ctx, getPlayerNamesArgument(ctx), regionSupplier.apply(ctx), getGroupArgument(ctx)))))
                        )
                )
                .then(literal(TEAM)
                        .then(Commands.argument(GROUP.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(GROUP_LIST, builder))
                                .then(Commands.argument(TEAM.toString(), TeamArgument.team())
                                        .executes(ctx -> removeTeam(ctx, getTeamArgument(ctx), regionSupplier.apply(ctx), getGroupArgument(ctx))))))
                .then(literal(FLAG)
                        .then(Commands.argument(FLAG.toString(), StringArgumentType.greedyString())
                                .suggests((ctx, builder) -> IFlagArgumentType.flag().listSuggestions(ctx, builder))
                                .executes(ctx -> removeFlag(ctx, regionSupplier.apply(ctx), getFlagArguments(ctx)))));
    }

    public static LiteralArgumentBuilder<CommandSourceStack> buildAddSubCommand(Function<CommandContext<CommandSourceStack>, IProtectedRegion> regionSupplier) {
        return literal(ADD)
                .then(literal(PLAYER)
                        .then(Commands.argument(GROUP.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(GROUP_LIST, builder))
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
                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(GROUP_LIST, builder))
                                .then(Commands.argument(TEAM.toString(), TeamArgument.team())
                                        .executes(ctx -> addTeam(ctx, getTeamArgument(ctx), regionSupplier.apply(ctx), getGroupArgument(ctx))))))
                .then(literal(FLAG)
                        .then(Commands.argument(FLAG.toString(), StringArgumentType.greedyString())
                                .suggests((ctx, builder) -> IFlagArgumentType.flag().listSuggestions(ctx, builder))
                                .executes(ctx -> addFlag(ctx, regionSupplier.apply(ctx), getFlagArguments(ctx)))));
    }

    public static LiteralArgumentBuilder<CommandSourceStack> buildListSubCommand(Function<CommandContext<CommandSourceStack>, IProtectedRegion> regionSupplier) {
        return literal(LIST)
                .then(literal(FLAG)
                        .executes(ctx -> promptFlagList(ctx, regionSupplier.apply(ctx), 0))
                        .then(Commands.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                .executes(ctx -> promptFlagList(ctx, regionSupplier.apply(ctx), getPageNoArgument(ctx)))))
                .then(literal(GROUP)
                        .then(Commands.argument(GROUP.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(CommandUtil.GROUP_LIST, builder))
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
                                                        .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(GROUP_LIST, builder))
                                                        .executes(ctx -> copyRegionPlayers(ctx, srcSupplier.apply(ctx), getTargetLocalRegionArgument(ctx), getGroupArgument(ctx)))))))
                        .then(literal(TO_DIM)
                                .then(Commands.argument(TARGET_DIM.toString(), DimensionArgument.dimension())
                                        .then(Commands.argument(GROUP.toString(), StringArgumentType.word())
                                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(GROUP_LIST, builder))
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

    /**
     * Prompt the common region state to the command issuer.
     * == [state] for [<name>] ==
     * Enabled: [true|false] | [all-off] [all-on]
     * Alert: [on|off] | [all-off] [all-on]
     */
    public static int promptRegionState(CommandContext<CommandSourceStack> ctx, IProtectedRegion region) {
        sendCmdFeedback(ctx.getSource(), buildHeader(new TranslatableComponent("cli.msg.info.header.for", buildRegionStateLink(region), buildRegionInfoLink(region))));
        MutableComponent regionEnableComponent = buildRegionEnableComponent(region);
        MutableComponent regionAlertComponent = buildRegionAlertToggleLink(region);
        if (region.getRegionType() == RegionType.DIMENSION) {
            regionEnableComponent.append(" | ").append(buildAllLocalEnableComponent(getDimCacheArgument(ctx)));
            regionAlertComponent.append(" | ").append(buildAllLocalAlertToggleLink(getDimCacheArgument(ctx)));
        }
        sendCmdFeedback(ctx.getSource(), buildInfoComponent("cli.msg.info.region.state.enable", regionEnableComponent));
        sendCmdFeedback(ctx.getSource(), buildInfoComponent("cli.msg.info.region.state.alert", regionAlertComponent));
        return 0;
    }

    /**
     * == Group '%s' for '%s'==
     * Players: [n player(s)][+]
     * Teams: [m team(s)][+]
     */
    public static int promptGroupLinks(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, String group) {
        if (!GROUP_LIST.contains(group)) {
            sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.global.info.group.invalid", group).withStyle(ChatFormatting.RED));
            return -1;
        }
        sendCmdFeedback(ctx.getSource(), buildGroupHeader(region, group));
        sendCmdFeedback(ctx.getSource(), buildGroupPlayerListLink(region, group));
        sendCmdFeedback(ctx.getSource(), buildGroupTeamListLink(region, group));
        return 0;
    }

    public static int promptFlagList(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, int pageNo) {
        String cmd = "";
        switch (region.getRegionType()) {
            case GLOBAL:
                cmd = buildCommandStr(GLOBAL.toString(), LIST.toString(), FLAG.toString());
                break;
            case DIMENSION:
                cmd = buildCommandStr(DIM.toString(), region.getName(), LIST.toString(), FLAG.toString());
                break;
            case LOCAL:
                cmd = buildCommandStr(LOCAL.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), FLAG.toString());
                break;
        }
        List<IFlag> flags = LocalRegions.getSortedFlags(region);
        if (flags.isEmpty()) {
            sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.info.region.flag.empty", buildRegionInfoLink(region)));
            return 1;
        }
        List<MutableComponent> flagPagination = buildPaginationComponents(
                buildRegionFlagInfoHeader(region),
                cmd, buildRemoveFlagEntries(region, flags), pageNo,
                new TextComponent(" - ").append(buildAddFlagLink(region)));
        flagPagination.forEach(line -> sendCmdFeedback(ctx.getSource(), line));
        return 0;
    }

    public static int promptGroupList(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, String group, GroupType groupType, int pageNo) {
        String cmd = "";
        String dim = region.getDim().location().toString();
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
            sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.region.info.group.invalid", group).withStyle(ChatFormatting.RED));
            return -1;
        }
        List<String> groupNames = getGroupList(region, group, groupType);
        if (groupNames.isEmpty()) {
            sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.info.region.group." + groupType.name + ".empty",
                    group, buildRegionInfoLink(region)));
            return 1;
        }
        List<MutableComponent> groupMemberPagination = buildPaginationComponents(
                buildGroupHeader(region, group, groupType),
                cmd, buildRemoveGroupMemberEntries(region, groupNames, groupType, group),
                pageNo,
                new TextComponent(" - ").append(buildAddToGroupLink(region, group, groupType)));
        groupMemberPagination.forEach(line -> sendCmdFeedback(ctx.getSource(), line));
        return 0;
    }

    public static int setActiveState(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, boolean activate) {
        region.setIsActive(activate);
        RegionDataManager.save();
        MutableComponent undoLink = buildRegionActionUndoLink(ctx.getInput(), String.valueOf(!activate), String.valueOf(activate));
        TranslatableComponent msg = new TranslatableComponent("cli.msg.info.region.state.enable.set.value",
                buildRegionInfoLink(region), region.isActive() ? "active" : "inactive");
        sendCmdFeedback(ctx.getSource(), msg.append(" ").append(undoLink));
        return 0;
    }

    public static int setAlertState(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, boolean showAlert) {
        boolean oldState = !region.isMuted();
        region.setIsMuted(showAlert);
        RegionDataManager.save();
        if (oldState == region.isMuted()) {
            MutableComponent undoLink = buildRegionActionUndoLink(ctx.getInput(), String.valueOf(showAlert), String.valueOf(!showAlert));
            TranslatableComponent msg = new TranslatableComponent("cli.msg.info.state.alert.set.value",
                    buildRegionInfoLink(region), region.isMuted() ? "muted" : "active");
            sendCmdFeedback(ctx.getSource(), msg.append(" ").append(undoLink));
        }
        return 0;
    }

    public static int removeTeam(CommandContext<CommandSourceStack> src, Team team, IProtectedRegion region, String group) {
        if (!GROUP_LIST.contains(group)) {
            sendCmdFeedback(src.getSource(), new TranslatableComponent("cli.msg.region.info.group.invalid", group).withStyle(ChatFormatting.RED));
            return -1;
        }
        MutableComponent undoLink = buildRegionActionUndoLink(src.getInput(), REMOVE, ADD);
        MutableComponent teamInfo = buildGroupInfo(region, team.getName(), GroupType.TEAM);
        if (region.getGroup(group).hasTeam(team.getName())) {
            region.removeTeam(team.getName(), group);
            RegionDataManager.save();
            TranslatableComponent msg = new TranslatableComponent("cli.msg.info.region.group.team.removed", teamInfo, group,
                    buildRegionInfoLink(region));
            sendCmdFeedback(src.getSource(), msg.append(" ").append(undoLink));
            return 0;
        }
        TranslatableComponent msg = new TranslatableComponent("cli.msg.info.region.group.team.not-present", teamInfo, group,
                buildRegionInfoLink(region));
        sendCmdFeedback(src.getSource(), msg);
        return 1;
    }


    private static int removePlayersByName(CommandContext<CommandSourceStack> ctx, Collection<String> playerNames, IProtectedRegion region, String group) {
        if (!GROUP_LIST.contains(group)) {
            sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.region.info.group.invalid", group).withStyle(ChatFormatting.RED));
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
        if (!GROUP_LIST.contains(group)) {
            sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.region.info.group.invalid", group).withStyle(ChatFormatting.RED));
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
        if (!GROUP_LIST.contains(group)) {
            sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.region.info.group.invalid", group).withStyle(ChatFormatting.RED));
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
        MutableComponent undoLink = buildRegionActionUndoLink(ctx.getInput(), REMOVE, ADD);
        if (region.getGroup(group).hasPlayer(playerUuid)) {
            region.removePlayer(playerUuid, group);
            TranslatableComponent msg = new TranslatableComponent("cli.msg.info.region.group.player.removed", playerInfo, group,
                    buildRegionInfoLink(region));
            sendCmdFeedback(ctx.getSource(), msg.append(" ").append(undoLink));
            RegionDataManager.save();
            return 0;
        }

        TranslatableComponent msg = new TranslatableComponent("cli.msg.info.region.group.player.not-present", playerInfo, group,
                buildRegionInfoLink(region));
        sendCmdFeedback(ctx.getSource(), msg);
        return 1;
    }

    private static int addPlayerByUuid(CommandContext<CommandSourceStack> ctx, UUID playerUuid, IProtectedRegion region, String group) {
        if (!GROUP_LIST.contains(group)) {
            sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.region.info.group.invalid", group).withStyle(ChatFormatting.RED));
            return -1;
        }
        GameProfile cachedProfile = MojangApiHelper.lookupGameProfileInCache(ctx, playerUuid);
        if (cachedProfile != null && cachedProfile.isComplete()) {
            TranslatableComponent cacheSuccess = new TranslatableComponent("cli.msg.info.player.lookup.cache.success", playerUuid.toString());
            sendCmdFeedback(ctx.getSource(), cacheSuccess);
            return addPlayer(ctx, cachedProfile.getId(), cachedProfile.getName(), region, group);
        }
        TranslatableComponent cacheMiss = new TranslatableComponent("cli.msg.info.player.lookup.pending",
                playerUuid.toString(), "uuid");
        sendCmdFeedback(ctx.getSource(), cacheMiss);
        CompletableFuture.runAsync(() -> MojangApiHelper.getGameProfileInfo(playerUuid, (gameProfile) -> {
            if (gameProfile != null) {
                TranslatableComponent lookupSuccess = new TranslatableComponent("cli.msg.info.player.lookup.api.success", playerUuid.toString());
                sendCmdFeedback(ctx.getSource(), lookupSuccess);
                addPlayer(ctx, gameProfile.getId(), gameProfile.getName(), region, group);
            } else {
                TranslatableComponent lookupFailed = new TranslatableComponent("cli.msg.info.player.lookup.api.failed", playerUuid.toString());
                sendCmdFeedback(ctx.getSource(), lookupFailed);
            }
        }));
        return 0;
    }

    public static int addPlayersByName(CommandContext<CommandSourceStack> ctx, List<String> playerNames, IProtectedRegion region, String group) {
        if (!GROUP_LIST.contains(group)) {
            sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.region.info.group.invalid", group).withStyle(ChatFormatting.RED));
            return -1;
        }
        playerNames.forEach(name -> CommandUtil.addPlayerByName(ctx, name, region, group));
        return 0;
    }

    private static int addPlayerByName(CommandContext<CommandSourceStack> ctx, String playerName, IProtectedRegion region, String group) {
        GameProfile cachedProfile = MojangApiHelper.lookupGameProfileInCache(ctx, playerName);
        if (cachedProfile != null && cachedProfile.isComplete()) {
            TranslatableComponent cacheSuccess = new TranslatableComponent("cli.msg.info.player.lookup.cache.success", playerName);
            sendCmdFeedback(ctx.getSource(), cacheSuccess);
            return addPlayer(ctx, cachedProfile.getId(), cachedProfile.getName(), region, group);
        }
        TranslatableComponent cacheMiss = new TranslatableComponent("cli.msg.info.player.lookup.pending",
                playerName, "name");
        sendCmdFeedback(ctx.getSource(), cacheMiss);
        CompletableFuture.runAsync(() -> MojangApiHelper.getGameProfileInfo(playerName, (gameProfile) -> {
            if (gameProfile != null) {
                TranslatableComponent lookupSuccess = new TranslatableComponent("cli.msg.info.player.lookup.api.success", playerName);
                sendCmdFeedback(ctx.getSource(), lookupSuccess);
                addPlayer(ctx, gameProfile.getId(), gameProfile.getName(), region, group);
            } else {
                TranslatableComponent lookupFailed = new TranslatableComponent("cli.msg.info.player.lookup.api.failed", playerName);
                sendCmdFeedback(ctx.getSource(), lookupFailed);
            }
        }));
        return 0;
    }

    public static int addPlayers(CommandContext<CommandSourceStack> ctx, Collection<ServerPlayer> players, IProtectedRegion region, String group) {
        players.forEach(player -> CommandUtil.addPlayer(ctx, player, region, group));
        return 0;
    }

    public static int addPlayer(CommandContext<CommandSourceStack> src, Player player, IProtectedRegion region, String group) {
        if (!GROUP_LIST.contains(group)) {
            sendCmdFeedback(src.getSource(), new TranslatableComponent("cli.msg.region.info.group.invalid", group).withStyle(ChatFormatting.RED));
            return -1;
        }
        return addPlayer(src, player.getUUID(), player.getScoreboardName(), region, group);
    }

    private static int addPlayer(CommandContext<CommandSourceStack> src, UUID uuid, String name, IProtectedRegion region, String group) {
        MutableComponent regionInfoLink = buildRegionInfoLink(region);
        MutableComponent undoLink = buildRegionActionUndoLink(src.getInput(), ADD, REMOVE);
        if (!region.hasPlayer(uuid, group)) {
            region.addPlayer(uuid, name, group);
            RegionDataManager.save();
            TranslatableComponent msg = new TranslatableComponent("cli.msg.info.region.group.player.added", name, group, regionInfoLink);
            sendCmdFeedback(src.getSource(), msg.append(" ").append(undoLink));
            return 0;
        }
        TranslatableComponent msg = new TranslatableComponent("cli.msg.info.region.group.player.present", name, group, regionInfoLink);
        sendCmdFeedback(src.getSource(), msg);
        return 1;
    }

    public static int addTeam(CommandContext<CommandSourceStack> ctx, Team team, IProtectedRegion region, String group) {
        if (!GROUP_LIST.contains(group)) {
            sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.region.info.group.invalid", group).withStyle(ChatFormatting.RED));
            return -1;
        }
        MutableComponent regionInfoLink = buildRegionInfoLink(region);
        MutableComponent teamHoverInfo = buildTeamHoverComponent(team);
        if (!region.hasTeam(team.getName(), group)) {
            region.addTeam(team.getName(), group);
            RegionDataManager.save();
            MutableComponent undoLink = buildRegionActionUndoLink(ctx.getInput(), ADD, REMOVE);
            TranslatableComponent msg = new TranslatableComponent("cli.msg.info.region.group.team.added",
                    teamHoverInfo, group, regionInfoLink);
            sendCmdFeedback(ctx.getSource(), msg.append(" ").append(undoLink));
            return 0;
        }
        TranslatableComponent msg = new TranslatableComponent("cli.msg.info.region.group.team.present",
                teamHoverInfo, group, regionInfoLink);
        sendCmdFeedback(ctx.getSource(), msg);
        return 1;
    }

    public static int removeFlag(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, Set<RegionFlag> flags) {
        flags.forEach(flag -> CommandUtil.removeRegionFlag(ctx, region, flag));
        return 0;
    }

    public static int removeRegionFlag(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, RegionFlag flag) {
        if (region.containsFlag(flag)) {
            region.removeFlag(flag.name);
            RegionDataManager.save();
            MutableComponent msg = new TranslatableComponent("cli.msg.flag.removed", flag.name,
                    buildRegionInfoLink(region));
            MutableComponent undoLink = buildRegionActionUndoLink(ctx.getInput(), REMOVE, ADD);
            sendCmdFeedback(ctx.getSource(), msg.append(" ").append(undoLink));
            return 0;
        } else {
            MutableComponent msg = new TranslatableComponent("cli.msg.flag.not-present", flag.name,
                    buildRegionInfoLink(region));
            sendCmdFeedback(ctx.getSource(), msg);
            return 1;
        }
    }

    public static int copyRegionFlags(CommandContext<CommandSourceStack> ctx, IProtectedRegion srcRegion, IProtectedRegion targetRegion) {
        if (srcRegion.getFlags().isEmpty()) {
            sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.info.region.flag.empty", buildRegionInfoLink(srcRegion)));
            return 1;
        }
        // get the flags which are in the srcRegion but not in the targetRegion
        Set<IFlag> flagsToCopy = srcRegion.getFlags().stream()
                .filter(flag -> !targetRegion.containsFlag(flag.getName()))
                .collect(Collectors.toSet());
        // flagsToCopy.forEach(region::addFlag);
        srcRegion.getFlags().forEach(targetRegion::addFlag);
        RegionDataManager.save();
        sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.copy.region.flags", flagsToCopy.size(), buildRegionInfoLink(srcRegion), buildRegionInfoLink(targetRegion)));
        return 0;
    }

    public static int copyRegionState(CommandContext<CommandSourceStack> ctx, IProtectedRegion srcRegion, IProtectedRegion targetRegion) {
        targetRegion.setIsActive(srcRegion.isActive());
        targetRegion.setIsMuted(srcRegion.isMuted());
        if (srcRegion instanceof IMarkableRegion && targetRegion instanceof IMarkableRegion) {
            IMarkableRegion regionSource = (IMarkableRegion) srcRegion;
            IMarkableRegion regionTarget = (IMarkableRegion) targetRegion;
            regionTarget.setPriority(regionSource.getPriority());
        }
        RegionDataManager.save();
        sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.copy.region.state", buildRegionInfoLink(srcRegion), buildRegionInfoLink(targetRegion)));
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
            sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.copy.region.players", playerEntriesToCopy.size(), group, buildRegionInfoLink(srcRegion), buildRegionInfoLink(targetRegion)));
            return 0;
        }
        sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.info.region.group.player.empty", group, buildRegionInfoLink(srcRegion)));
        return 1;
    }

    public static int copyRegionPlayers(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, IProtectedRegion srcRegion) {
        return copyRegionPlayers(ctx, region, srcRegion, CommandConstants.MEMBER.toString())
                + copyRegionPlayers(ctx, region, srcRegion, CommandConstants.OWNER.toString());
    }

    public static int clearFlags(CommandContext<CommandSourceStack> ctx, IProtectedRegion region) {
        int amount = region.getFlags().size();
        if (amount == 0) {
            MutableComponent feedbackMsg = new TranslatableComponent("cli.msg.info.region.flag.empty", buildRegionInfoLink(region));
            sendCmdFeedback(ctx.getSource(), feedbackMsg);
            return 1;
        }
        Collection<IFlag> flags = region.getFlags(); // TODO: Undo action link
        region.getFlags().clear();
        MutableComponent feedbackMsg = new TranslatableComponent("cli.msg.info.region.flag.cleared", amount, buildRegionInfoLink(region));
        sendCmdFeedback(ctx.getSource(), feedbackMsg);
        RegionDataManager.save();
        return 0;
    }

    public static int clearPlayers(CommandContext<CommandSourceStack> ctx, IProtectedRegion region) {
        return clearPlayers(ctx, region, MEMBER) + clearPlayers(ctx, region, OWNER);
    }

    public static int clearPlayers(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, String groupName) {
        int amount = region.getGroup(groupName).getPlayers().size();
        if (amount == 0) {
            MutableComponent feedbackMsg = new TranslatableComponent("cli.msg.info.region.players.empty", buildRegionInfoLink(region), groupName);
            sendCmdFeedback(ctx.getSource(), feedbackMsg);
            return 1;
        }
        region.getGroup(groupName).clearPlayers();
        MutableComponent feedbackMsg = new TranslatableComponent("cli.msg.info.region.players.cleared", buildRegionInfoLink(region), amount, groupName);
        sendCmdFeedback(ctx.getSource(), feedbackMsg);
        RegionDataManager.save();
        return 0;
    }

    public static int clearTeams(CommandContext<CommandSourceStack> ctx, IProtectedRegion region) {
        return clearPlayers(ctx, region, MEMBER) + clearPlayers(ctx, region, OWNER);
    }

    public static int clearTeams(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, String groupName) {
        int amount = region.getGroup(groupName).getTeams().size();
        if (amount == 0) {
            MutableComponent feedbackMsg = new TranslatableComponent("cli.msg.region.info.teams.empty", buildRegionInfoLink(region), groupName);
            sendCmdFeedback(ctx.getSource(), feedbackMsg);
            return 1;
        }
        region.getGroup(groupName).clearTeams();
        MutableComponent feedbackMsg = new TranslatableComponent("cli.msg.region.info.teams.cleared", buildRegionInfoLink(region), amount, groupName);
        sendCmdFeedback(ctx.getSource(), feedbackMsg);
        RegionDataManager.save();
        return 0;
    }


    public static int clearGroups(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, String groupName) {
        return CommandUtil.clearTeams(ctx, region, groupName) + CommandUtil.clearPlayers(ctx, region, groupName);
    }

    public static int addFlag(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, Set<RegionFlag> flags) {
        flags.forEach(flag -> CommandUtil.addRegionFlag(ctx, region, flag));
        return 0;
    }

    public static int addRegionFlag(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, RegionFlag flag) {
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
            if (flag.name.contains("spawning") && FlagConfig.removeEntitiesEnabled()) {
                removeInvolvedEntities(ctx, region, flag);
            }
            region.addFlag(iFlag);
            RegionDataManager.save();
            MutableComponent flagLink = buildFlagInfoLink(region, iFlag);
            MutableComponent msg = new TranslatableComponent("cli.msg.flag.added",
                    flagLink, buildRegionInfoLink(region));
            MutableComponent undoLink = buildRegionActionUndoLink(ctx.getInput(), ADD, REMOVE);
            sendCmdFeedback(ctx.getSource(), msg.append(" ").append(undoLink));
            return 0;
        } else {
            MutableComponent msg = new TranslatableComponent("cli.msg.flag.present", flag.name,
                    buildRegionInfoLink(region));
            sendCmdFeedback(ctx.getSource(), msg);
            return 1;
        }
    }

    public static void removeInvolvedEntities(CommandContext<CommandSourceStack> ctx, IProtectedRegion region, RegionFlag flag) {
        ResourceKey<Level> dimKey = ResourceKey.create(Registry.DIMENSION_REGISTRY, region.getDim().location());
        MinecraftServer server = ctx.getSource().getServer();
        ServerLevel regionWorld = server.getLevel(dimKey);
        if (regionWorld != null) {
            Predicate<? super Entity> entityFilter = getEntityFilterForFlag(flag);
            switch (region.getRegionType()) {
                case GLOBAL: {
                    server.getAllLevels().forEach(world -> {
                        List<Entity> entitiesToRemove = getEntitiesToRemove(world, entityFilter, flag);
                        entitiesToRemove.forEach(world::despawn);
                    });
                }
                break;
                case DIMENSION: {
                    List<Entity> entitiesToRemove = getEntitiesToRemove(regionWorld, entityFilter, flag);
                    entitiesToRemove.forEach(regionWorld::despawn);
                }
                break;
                case LOCAL: {
                    List<Entity> entitiesToRemove = getEntitiesToRemove(regionWorld, (IMarkableRegion) region, entityFilter);
                    entitiesToRemove.forEach(regionWorld::despawn);
                }
                break;
            }
        }
    }

    private static boolean hasEnabledPersistenceFlag(Entity e) {
        if (e instanceof Mob) {
            Mob mob = (Mob) e;
            return mob.isPersistenceRequired();
        }
        return false;
    }

    private static boolean isNotPersistent(Entity e) {
        return !hasEnabledPersistenceFlag(e) && !e.hasCustomName();
    }

    private static Predicate<? super Entity> getEntityFilterForFlag(RegionFlag flag) {
        switch (flag) {
            case SPAWNING_ALL:
                return e -> !(e instanceof Player);
            case SPAWNING_MONSTER:
                return HandlerUtil::isMonster;
            case SPAWNING_ANIMAL:
                return HandlerUtil::isAnimal;
            case SPAWNING_GOLEM:
                return e -> e instanceof SnowGolem || e instanceof IronGolem;
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

    /**
     * TODO: make this work with areas of different shapes by manually implementing it. <br>
     * Use the predicate to determine if the entity is within the region
     * ...area.intersects(entity.getBoundingBox()) ...
     */
    private static List<Entity> getEntitiesToRemove(ServerLevel level, IMarkableRegion region, Predicate<? super Entity> entityFilter) {
        return level.getEntities((Entity) null, ((CuboidArea) region.getArea()).getArea(), entityFilter)
                .stream()
                .filter(CommandUtil::isNotPersistent)
                .collect(Collectors.toList());
    }

    /**
     * TODO: after flags can be negated (either flag is not existent or deactivated...)
     */
    private static List<Entity> getEntitiesToRemove(ServerLevel level, Predicate<? super Entity> entityFilter, RegionFlag flag) {
        List<Entity> entities = level.getEntities(null, entityFilter);
        return entities.stream()
                // don't consider entities, which are currently in a Local Region which doesn't have the flag
                .filter(e -> isInRegionWithoutFlag(level, flag, e))
                .filter(CommandUtil::isNotPersistent)
                .collect(Collectors.toList());
    }

    private static boolean isInRegionWithoutFlag(ServerLevel level, RegionFlag flag, Entity e) {
        return LocalRegions.getRegionWithoutFlag(flag, e.blockPosition(), level.dimension()) == null;
    }

    public static int promptRegionInfo(CommandContext<CommandSourceStack> ctx, IProtectedRegion region) {
        // == Region [<name>] overview ==
        sendCmdFeedback(ctx.getSource(), buildRegionOverviewHeader(region));
        // Flags: [n flag(s)][+]
        sendCmdFeedback(ctx.getSource(), buildInfoComponent("cli.msg.info.region.flag", buildFlagListLink(region)));
        if (region.getRegionType() == RegionType.LOCAL) {
            IMarkableRegion markableRegion = (IMarkableRegion) region;
            // Area: [Area]
            sendCmdFeedback(ctx.getSource(), buildInfoComponent("cli.msg.info.region.area", buildRegionAreaLink(markableRegion)));
        }
        // Groups: [owners], [members], [<listAffiliations>]
        sendCmdFeedback(ctx.getSource(), buildInfoComponent("cli.msg.info.region.group", buildGroupLinks(region)));

        promptRegionChildrenInfo(ctx, region);
        // State: [State]
        sendCmdFeedback(ctx.getSource(), buildInfoComponent("cli.msg.info.region.state", buildRegionStateLink(region)));
        return 0;
    }

    public static void promptRegionChildrenInfo(CommandContext<CommandSourceStack> ctx, IProtectedRegion region) {
        switch (region.getRegionType()) {
            case GLOBAL: {
                // [n dimensions(s)]
                sendCmdFeedback(ctx.getSource(), buildInfoComponent("cli.msg.info.dim.region", buildRegionChildrenLink(region)));
            }
            break;
            case DIMENSION: {
                // Regions: [global], [n children], [n regions][+],
                MessageUtil.buildRegionChildrenLink(region);

                MutableComponent globalRegionLink = buildRegionInfoLink(region, new TranslatableComponent("cli.msg.info.region.global.link.hover"));

                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(region.getDim());

                MutableComponent regionsAndChildren = new TranslatableComponent("cli.msg.dim.info.region")
                        .append(": ")
                        .append(globalRegionLink)
                        .append(new TextComponent(", ").withStyle(ChatFormatting.RESET))
                        .append(buildRegionChildrenLink(region))
                        .append(new TextComponent(", ").withStyle(ChatFormatting.RESET))
                        .append(buildDimRegionsLink(dimCache));
                sendCmdFeedback(ctx.getSource(), regionsAndChildren);
            }
            break;
            case LOCAL: {
                // Hierarchy: [parent][x], [n children][+]
                MutableComponent regionHierarchy = new TranslatableComponent("cli.msg.info.region.hierarchy")
                        .append(": ")
                        .append(buildRegionInfoLink(region))
                        .append(buildParentClearLink((IMarkableRegion) region))
                        .append(new TextComponent(", ").withStyle(ChatFormatting.RESET))
                        .append(buildRegionChildrenLink(region));
                sendCmdFeedback(ctx.getSource(), regionHierarchy);
            }
            break;
            case TEMPLATE:
                break;
            default:
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        }
    }

}
