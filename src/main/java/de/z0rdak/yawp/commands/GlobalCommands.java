package de.z0rdak.yawp.commands;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import de.z0rdak.yawp.core.group.GroupType;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.*;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.command.CommandSource;
import net.minecraft.command.Commands;
import net.minecraft.command.ISuggestionProvider;
import net.minecraft.command.arguments.EntityArgument;
import net.minecraft.command.arguments.TeamArgument;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.scoreboard.Team;
import net.minecraft.util.text.IFormattableTextComponent;
import net.minecraft.util.text.StringTextComponent;

import java.util.*;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.commands.arguments.ArgumentUtil.*;
import static de.z0rdak.yawp.util.MessageUtil.*;

public class GlobalCommands {

    private GlobalCommands() {
    }

    // TODO: RESET
    public static LiteralArgumentBuilder<CommandSource> build() {
        return literal(GLOBAL)
                .executes(ctx -> promptGlobalInfo(ctx.getSource(), getGlobalRegion()))
                .then(literal(INFO).executes(ctx -> promptGlobalInfo(ctx.getSource(), getGlobalRegion())))
                .then(literal(STATE)
                        .executes(ctx -> promptRegionState(ctx, getGlobalRegion()))
                        .then(literal(ALERT)
                                .executes(ctx -> setAlertState(ctx, getGlobalRegion()))
                                .then(Commands.argument(ALERT.toString(), BoolArgumentType.bool())
                                        .executes(ctx -> setAlertState(ctx, getGlobalRegion(), getAlertArgument(ctx))))
                        )
                        .then(literal(ENABLE)
                                .executes(ctx -> setActiveState(ctx, getGlobalRegion()))
                                .then(Commands.argument(ENABLE.toString(), BoolArgumentType.bool())
                                        .executes(ctx -> setActiveState(ctx, getGlobalRegion(), getEnableArgument(ctx))))
                        )
                )
                .then(literal(LIST)
                        .then(literal(REGION)
                                .executes(ctx -> promptDimensionalRegions(ctx, getGlobalRegion(), 0))
                                .then(Commands.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                        .executes(ctx -> promptDimensionalRegions(ctx, getGlobalRegion(), getPageNoArgument(ctx))))
                        )
                        /* /wp dimension <dim> list group <group> */
                        .then(literal(GROUP)
                                .then(Commands.argument(GROUP.toString(), StringArgumentType.string())
                                        .suggests((ctx, builder) -> ISuggestionProvider.suggest(RegionCommands.GROUP_LIST, builder))
                                        .executes(ctx -> promptGroupLinks(ctx, getGlobalRegion(), getGroupArgument(ctx)))
                                        .then(literal(TEAM)
                                                .executes(ctx -> promptGroupList(ctx, getGlobalRegion(), getGroupArgument(ctx), GroupType.TEAM, 0))
                                                .then(Commands.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                                        .executes(ctx -> promptGroupList(ctx, getGlobalRegion(), getGroupArgument(ctx), GroupType.TEAM, getPageNoArgument(ctx))))
                                        )
                                        .then(literal(PLAYER)
                                                .executes(ctx -> promptGroupList(ctx, getGlobalRegion(), getGroupArgument(ctx), GroupType.PLAYER, 0))
                                                .then(Commands.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                                        .executes(ctx -> promptGroupList(ctx, getGlobalRegion(), getGroupArgument(ctx), GroupType.PLAYER, getPageNoArgument(ctx))))
                                        )
                                )
                        )
                        /* /wp dimension <dim> list flag */
                        .then(literal(FLAG)
                                .executes(ctx -> promptFlagList(ctx, getGlobalRegion(), 0))
                                .then(Commands.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                        .executes(ctx -> promptFlagList(ctx, getGlobalRegion(), getPageNoArgument(ctx))))
                        )
                )
                .then(literal(REMOVE)
                        .then(literal(PLAYER)
                                .then(Commands.argument(GROUP.toString(), StringArgumentType.string())
                                        .suggests((ctx, builder) -> ISuggestionProvider.suggest(RegionCommands.GROUP_LIST, builder))
                                        .then(Commands.argument(PLAYER.toString(), EntityArgument.player())
                                                .executes(ctx -> removePlayer(ctx, getPlayerArgument(ctx), getGlobalRegion(), getGroupArgument(ctx)))))

                                .then(Commands.argument(GROUP.toString(), StringArgumentType.string())
                                        .suggests((ctx, builder) -> ISuggestionProvider.suggest(RegionCommands.GROUP_LIST, builder))
                                        .then(Commands.argument(PLAYER.toString(), EntityArgument.player())
                                                .executes(ctx -> removePlayer(ctx, getPlayerArgument(ctx), getGlobalRegion(), getGroupArgument(ctx))))))
                        .then(literal(TEAM)
                                .then(Commands.argument(GROUP.toString(), StringArgumentType.string())
                                        .suggests((ctx, builder) -> ISuggestionProvider.suggest(RegionCommands.GROUP_LIST, builder))
                                        .then(Commands.argument(TEAM.toString(), TeamArgument.team())
                                                .executes(ctx -> removeTeam(ctx, getTeamArgument(ctx), getGlobalRegion(), getGroupArgument(ctx)))))

                                .then(Commands.argument(GROUP.toString(), StringArgumentType.string())
                                        .suggests((ctx, builder) -> ISuggestionProvider.suggest(RegionCommands.GROUP_LIST, builder))
                                        .then(Commands.argument(TEAM.toString(), TeamArgument.team())
                                                .executes(ctx -> removeTeam(ctx, getTeamArgument(ctx), getGlobalRegion(), getGroupArgument(ctx))))))
                        .then(literal(FLAG)
                                .then(Commands.argument(FLAG.toString(), StringArgumentType.string())
                                        .suggests((ctx, builder) -> ISuggestionProvider.suggest(RegionDataManager.get().getFlagsIdsForDim(getDimCacheArgument(ctx)), builder))
                                        .executes(ctx -> removeFlag(ctx, getGlobalRegion(), getFlagArgument(ctx)))))
                )
                .then(literal(ADD)
                        .then(literal(PLAYER)
                                .then(Commands.argument(GROUP.toString(), StringArgumentType.string())
                                        .suggests((ctx, builder) -> ISuggestionProvider.suggest(RegionCommands.GROUP_LIST, builder))
                                        .then(Commands.argument(PLAYER.toString(), EntityArgument.player())
                                                .executes(ctx -> addPlayer(ctx, getPlayerArgument(ctx), getGlobalRegion(), getGroupArgument(ctx)))))
                                .then(Commands.argument(GROUP.toString(), StringArgumentType.string())
                                        .suggests((ctx, builder) -> ISuggestionProvider.suggest(RegionCommands.GROUP_LIST, builder))
                                        .then(Commands.argument(PLAYER.toString(), EntityArgument.player())
                                                .executes(ctx -> addPlayer(ctx, getPlayerArgument(ctx), getGlobalRegion(), getGroupArgument(ctx))))))
                        .then(literal(TEAM)
                                .then(Commands.argument(GROUP.toString(), StringArgumentType.string())
                                        .suggests((ctx, builder) -> ISuggestionProvider.suggest(RegionCommands.GROUP_LIST, builder))
                                        .then(Commands.argument(TEAM.toString(), TeamArgument.team())
                                                .executes(ctx -> addTeam(ctx, getTeamArgument(ctx), getGlobalRegion(), getGroupArgument(ctx)))))

                                .then(Commands.argument(GROUP.toString(), StringArgumentType.string())
                                        .suggests((ctx, builder) -> ISuggestionProvider.suggest(RegionCommands.GROUP_LIST, builder))
                                        .then(Commands.argument(TEAM.toString(), TeamArgument.team())
                                                .executes(ctx -> addTeam(ctx, getTeamArgument(ctx), getGlobalRegion(), getGroupArgument(ctx))))))
                        .then(literal(FLAG)
                                .then(Commands.argument(FLAG.toString(), StringArgumentType.string())
                                        .suggests((ctx, builder) -> ISuggestionProvider.suggest(RegionFlag.getFlagNames(), builder))
                                        .executes(ctx -> addFlag(ctx, getGlobalRegion(), getFlagArgument(ctx)))))

                );
    }

    private static int setActiveState(CommandContext<CommandSource> ctx, IProtectedRegion region, boolean activate) {
        return CommandUtil.setActiveState(ctx, region, RegionType.GLOBAL, activate);
    }

    private static int setActiveState(CommandContext<CommandSource> ctx, IProtectedRegion region) {
        return setActiveState(ctx, region, !region.isActive());
    }

    private static int setAlertState(CommandContext<CommandSource> ctx, IProtectedRegion region, boolean showAlert) {
        return CommandUtil.setAlertState(ctx, region, RegionType.GLOBAL, showAlert);
    }

    private static int setAlertState(CommandContext<CommandSource> ctx, IProtectedRegion region) {
        return setAlertState(ctx, region, !region.isMuted());
    }

    private static int promptRegionState(CommandContext<CommandSource> ctx, IProtectedRegion region) {
        CommandUtil.promptRegionState(ctx, region, RegionType.GLOBAL);
        return 0;
    }

    private static int removeFlag(CommandContext<CommandSource> ctx, IProtectedRegion region, RegionFlag flag) {
        return CommandUtil.removeRegionFlag(ctx, region, RegionType.GLOBAL, flag);
    }

    private static int addFlag(CommandContext<CommandSource> ctx, IProtectedRegion region, RegionFlag flag) {
        return CommandUtil.addRegionFlag(ctx, region, RegionType.GLOBAL, flag);
    }

    private static int removePlayer(CommandContext<CommandSource> ctx, ServerPlayerEntity player, IProtectedRegion region, String group) {
        return CommandUtil.removePlayer(ctx, player, region, RegionType.GLOBAL, group);
    }

    private static int removePlayer(CommandContext<CommandSource> ctx, String playerName, IProtectedRegion region, String group) {
        return CommandUtil.removePlayer(ctx, playerName, region, RegionType.GLOBAL, group);
    }


    private static int removeTeam(CommandContext<CommandSource> ctx, Team team, IProtectedRegion region, String group) {
        return CommandUtil.removeTeam(ctx, team, region, RegionType.GLOBAL, group);
    }

    private static int addPlayer(CommandContext<CommandSource> ctx, ServerPlayerEntity player, IProtectedRegion region, String group) {
        return CommandUtil.addPlayer(ctx, player, region, RegionType.GLOBAL, group);
    }

    private static int addTeam(CommandContext<CommandSource> ctx, Team team, IProtectedRegion region, String group) {
        return CommandUtil.addTeam(ctx, team, region, RegionType.GLOBAL, group);
    }

    private static int promptFlagList(CommandContext<CommandSource> ctx, GlobalRegion region, int pageNo) {
        String cmd = buildCommandStr(GLOBAL.toString(), LIST.toString(), FLAG.toString());
        return CommandUtil.promptFlagList(ctx, cmd, region, RegionType.GLOBAL, pageNo);
    }

    private static int promptGroupLinks(CommandContext<CommandSource> ctx, GlobalRegion region, String group) {
        return CommandUtil.promptGroupLinks(ctx, region, group, RegionType.GLOBAL);
    }

    private static int promptGroupList(CommandContext<CommandSource> ctx, IProtectedRegion region, String group, GroupType groupType, int pageNo) {
        String cmd =  buildCommandStr(GLOBAL.toString(), LIST.toString(), GROUP.toString(), group, groupType.name);
        return CommandUtil.promptGroupList(ctx, cmd, region, group, groupType, RegionType.GLOBAL, pageNo);
    }

    private static int promptDimensionalRegions(CommandContext<CommandSource> ctx, GlobalRegion region, int pageNo) {
        List<DimensionRegionCache> dimCaches = RegionDataManager.getDimensionCaches();
        List<IFormattableTextComponent> regionPagination = buildPaginationComponents(
                buildRegionListHeader(region, RegionType.GLOBAL),
                buildCommandStr(GLOBAL.toString(), LIST.toString(), DIM.toString()),
                buildResetDimensionalRegionEntries(region, dimCaches, RegionType.GLOBAL),
                pageNo,
                // empty string, since there is now manual creation of dimensional regions
                new StringTextComponent(""));
        regionPagination.forEach(line -> sendCmdFeedback(ctx.getSource(), line));
        return 0;
    }

    // TODO:
    private static int promptGlobalInfo(CommandSource src, GlobalRegion globalRegion) {
        // [] header []
        sendCmdFeedback(src, buildRegionOverviewHeader(globalRegion, RegionType.GLOBAL));
        // [n dimensions(s)]
        sendCmdFeedback(src, buildInfoComponent("cli.msg.info.dim.region", buildRegionChildrenLink(globalRegion, RegionType.GLOBAL)));
        // Groups: [owners], [members], [<listAffiliations>]
        sendCmdFeedback(src, buildInfoComponent("cli.msg.info.region.group", buildGroupLinks(globalRegion, RegionType.GLOBAL)));
        // Flags: [n flag(s)] [+]
        sendCmdFeedback(src, buildInfoComponent("cli.msg.info.region.flag", buildFlagListLink(globalRegion, RegionType.GLOBAL)));
        // State: [activated]
        sendCmdFeedback(src, buildDimEnableLink(globalRegion));
        return 0;
    }
}
