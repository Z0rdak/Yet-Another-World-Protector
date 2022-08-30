package de.z0rdak.yawp.commands;

import com.mojang.brigadier.RedirectModifier;
import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.suggestion.SuggestionProvider;
import com.mojang.brigadier.suggestion.SuggestionsBuilder;
import de.z0rdak.yawp.config.server.CommandPermissionConfig;
import de.z0rdak.yawp.core.affiliation.PlayerContainer;
import de.z0rdak.yawp.core.flag.ConditionFlag;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.AbstractRegion;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.CommandUtil;
import net.minecraft.ChatFormatting;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.SharedSuggestionProvider;
import net.minecraft.commands.arguments.DimensionArgument;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.commands.arguments.TeamArgument;
import net.minecraft.network.chat.*;
import net.minecraft.resources.ResourceKey;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.level.Level;
import net.minecraft.world.scores.PlayerTeam;

import java.util.*;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.util.CommandUtil.*;
import static de.z0rdak.yawp.util.CommandUtil.getDimensionArgument;
import static de.z0rdak.yawp.util.MessageUtil.*;

public class DimensionCommands {

    private DimensionCommands() {
    }

    public static final LiteralArgumentBuilder<CommandSourceStack> DIMENSION_COMMAND = register();

    public static LiteralArgumentBuilder<CommandSourceStack> register() {
        // TODO: Command for managing affiliations and data handling
        List<String> affiliationList = Arrays.asList("member", "owner");
        return literal(CommandConstants.DIMENSION)
                /* /wp dimension help */
                // .then(helpLiteral.executes(ctx -> promptHelp(ctx.getSource())))
                /* /wp dimension <dim> list region */
                .then(Commands.argument(CommandConstants.DIMENSION.toString(), DimensionArgument.dimension())
                        /* /wp dimension <dim> [info] */
                        .executes(ctx -> promptDimensionInfo(ctx.getSource(), CommandUtil.getDimensionArgument(ctx)))
                        .then(literal(CommandConstants.INFO).executes(ctx -> promptDimensionInfo(ctx.getSource(), CommandUtil.getDimensionArgument(ctx))))
                        /* /wp dimension <dim> activate */
                        .then(literal(CommandConstants.ACTIVATE).executes(ctx -> setActiveState(ctx.getSource(), getDimensionArgument(ctx), getActivateArgument(ctx)))
                                .then(Commands.argument(CommandConstants.ACTIVATE.toString(), BoolArgumentType.bool()).executes(ctx -> setActiveState(ctx.getSource(), getDimensionArgument(ctx), getActivateArgument(ctx)))))
                        .then(literal(CommandConstants.LIST)
                                .then(literal(CommandConstants.REGION).executes(ctx -> promptDimensionRegionList(ctx.getSource(), getDimensionArgument(ctx))))
                                /* /wp dimension <dim> list owner */
                                .then(literal(CommandConstants.OWNER).executes(ctx -> promptDimensionPlayerList(ctx.getSource(), getDimensionArgument(ctx), CommandConstants.OWNER)))
                                /* /wp dimension <dim> list member */
                                .then(literal(CommandConstants.MEMBER).executes(ctx -> promptDimensionPlayerList(ctx.getSource(), getDimensionArgument(ctx), CommandConstants.MEMBER)))
                                /* /wp dimension <dim> list flag */
                                .then(literal(CommandConstants.FLAG).executes(ctx -> promptDimensionFlagList(ctx.getSource(), getDimensionArgument(ctx)))))

                        .then(literal(CommandConstants.REMOVE)
                                .then(literal(CommandConstants.PLAYER)
                                        .then(Commands.argument(CommandConstants.AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(affiliationList, builder))
                                                .then(Commands.argument(CommandConstants.PLAYER.toString(), EntityArgument.player())
                                                        .executes(ctx -> removePlayer(ctx.getSource(), getPlayerArgument(ctx), getDimensionArgument(ctx), getAssociateArgument(ctx)))))

                                        .then(Commands.argument(CommandConstants.AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(affiliationList, builder))
                                                .then(Commands.argument(CommandConstants.PLAYER.toString(), EntityArgument.player())
                                                        .executes(ctx -> removePlayer(ctx.getSource(), getPlayerArgument(ctx), getDimensionArgument(ctx), getAssociateArgument(ctx))))))
                                .then(literal(CommandConstants.TEAM)
                                        .then(Commands.argument(CommandConstants.AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(affiliationList, builder))
                                                .then(Commands.argument(CommandConstants.TEAM.toString(), TeamArgument.team())
                                                        .executes(ctx -> removeTeam(ctx.getSource(), getTeamArgument(ctx), getDimensionArgument(ctx), getAssociateArgument(ctx)))))

                                        .then(Commands.argument(CommandConstants.AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(affiliationList, builder))
                                                .then(Commands.argument(CommandConstants.TEAM.toString(), TeamArgument.team())
                                                        .executes(ctx -> removeTeam(ctx.getSource(), getTeamArgument(ctx), getDimensionArgument(ctx), getAssociateArgument(ctx))))))
                                .then(literal(CommandConstants.FLAG)
                                        .then(Commands.argument(CommandConstants.FLAG.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(RegionDataManager.get().getFlagsIdsForDim(getDimensionArgument(ctx)), builder))
                                                .executes(ctx -> removeFlag(ctx.getSource(), CommandUtil.getDimensionArgument(ctx), StringArgumentType.getString(ctx, CommandConstants.FLAG.toString()))))))

                        .then(literal(CommandConstants.ADD)
                                .then(literal(CommandConstants.PLAYER)
                                        .then(Commands.argument(CommandConstants.AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(affiliationList, builder))
                                                .then(Commands.argument(CommandConstants.PLAYER.toString(), EntityArgument.player())
                                                        .executes(ctx -> addPlayer(ctx.getSource(), getPlayerArgument(ctx), getDimensionArgument(ctx), getAssociateArgument(ctx)))))

                                        .then(Commands.argument(CommandConstants.AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(affiliationList, builder))
                                                .then(Commands.argument(CommandConstants.PLAYER.toString(), EntityArgument.player())
                                                        .executes(ctx -> addPlayer(ctx.getSource(), getPlayerArgument(ctx), getDimensionArgument(ctx), getAssociateArgument(ctx))))))
                                .then(literal(CommandConstants.TEAM)
                                        .then(Commands.argument(CommandConstants.AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(affiliationList, builder))
                                                .then(Commands.argument(CommandConstants.TEAM.toString(), TeamArgument.team())
                                                        .executes(ctx -> addTeam(ctx.getSource(), getTeamArgument(ctx), getDimensionArgument(ctx), getAssociateArgument(ctx)))))

                                        .then(Commands.argument(CommandConstants.AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(affiliationList, builder))
                                                .then(Commands.argument(CommandConstants.TEAM.toString(), TeamArgument.team())
                                                        .executes(ctx -> addTeam(ctx.getSource(), getTeamArgument(ctx), getDimensionArgument(ctx), getAssociateArgument(ctx))))))
                                .then(literal(CommandConstants.FLAG)
                                        .then(Commands.argument(CommandConstants.FLAG.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(RegionFlag.getFlags(), builder))
                                                .executes(ctx -> addFlag(ctx.getSource(), CommandUtil.getDimensionArgument(ctx), StringArgumentType.getString(ctx, CommandConstants.FLAG.toString())))))));
    }

    private static int removeFlag(CommandSourceStack src, ResourceKey<Level> dim, String flag) {
        DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(dim);
        if (dimCache != null) {
            dimCache.removeFlag(flag);
        }

        sendCmdFeedback(src, new TranslatableComponent("cli.msg.flags.removed", flag, dim.location().toString()));
        return 0;
    }

    private static int addFlag(CommandSourceStack src, ResourceKey<Level> dim, String flag) {
        // TODO: For now this works because we only have condition flags and no black/whitelist feature
        IFlag iflag = new ConditionFlag(flag, false);
        DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(dim);
        if (dimCache != null) {
            dimCache.addFlag(iflag);
        }
        sendCmdFeedback(src, new TranslatableComponent("cli.msg.flags.added", flag, dim.location().toString()));
        return 0;
    }

    private static int removePlayer(CommandSourceStack src, ServerPlayer player, ResourceKey<Level> dim, String affiliationType) {
        DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(dim);
        if (dimCache != null) {
            if (affiliationType.equals(CommandConstants.MEMBER.toString())) {
                dimCache.removeMember(player);
            }
            if (affiliationType.equals(CommandConstants.OWNER.toString())) {
                dimCache.removeOwner(player);
            }
            sendCmdFeedback(src, new TranslatableComponent("cli.msg.dim.info.player.removed", affiliationType, player.getScoreboardName(), dim.location().toString()));
        }
        return 0;
    }

    private static int removeTeam(CommandSourceStack src, PlayerTeam team, ResourceKey<Level> dim, String affiliationType) {
        DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(dim);
        if (dimCache != null) {
            if (affiliationType.equals(CommandConstants.MEMBER.toString())) {
                dimCache.removeMember(team);
            }
            if (affiliationType.equals(CommandConstants.OWNER.toString())) {
                dimCache.removeOwner(team);
            }
            sendCmdFeedback(src, new TranslatableComponent("cli.msg.dim.info.player.removed", affiliationType, team.getName(), dim.location().toString()));
            return 0;
        }
        return 1;
    }

    // TODO: If works replace with switch and catch error
    private static int addPlayer(CommandSourceStack src, ServerPlayer player, ResourceKey<Level> dim, String affiliationType) {
        DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(dim);
        if (dimCache != null) {
            if (affiliationType.equals(CommandConstants.MEMBER.toString())) {
                dimCache.addMember(player);
            }
            if (affiliationType.equals(CommandConstants.OWNER.toString())) {
                dimCache.addOwner(player);
            }
            sendCmdFeedback(src, new TranslatableComponent("cli.msg.dim.info.player.added", player.getScoreboardName(), dim.location().toString(), affiliationType));
            return 0;
        }
        return 1;
    }

    private static int addTeam(CommandSourceStack src, PlayerTeam team, ResourceKey<Level> dim, String affiliationType) {
        DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(dim);
        if (dimCache != null) {
            if (affiliationType.equals(CommandConstants.MEMBER.toString())) {
                dimCache.addMember(team);
            }
            if (affiliationType.equals(CommandConstants.OWNER.toString())) {
                dimCache.addOwner(team);
            }
            sendCmdFeedback(src, new TranslatableComponent("cli.msg.dim.info.team.added", team.getName(), dim.location().toString(), affiliationType));
            return 0;
        }
        return 1;
    }


    // TODO: Check
    private static int promptDimensionFlagList(CommandSourceStack src, ResourceKey<Level> dim) {
        List<IFlag> flags = RegionDataManager.get().getFlagsForDim(dim)
                .stream()
                // TODO: implement comparable for flags
                // .sorted()
                .collect(Collectors.toList());
        if (flags.isEmpty()) {
            sendCmdFeedback(src, new TranslatableComponent("cli.msg.dim.info.flags.empty", dim));
            return 1;
        }
        // TODO: lang key
        sendCmdFeedback(src, new TranslatableComponent(ChatFormatting.BOLD + "== Flags in dimension '" + dim.location() + "' ==="));
        flags.forEach(flag -> {
           MutableComponent removeFlagLink = new TextComponent(" - ")
                    .append(buildDimensionRemoveFlagLink(flag, dim))
                    .append(new TextComponent(" '" + flag.getFlagName() + "' "));

            sendCmdFeedback(src, removeFlagLink);
        });
        return 0;
    }

    private static int promptDimensionPlayerList(CommandSourceStack src, ResourceKey<Level> dim, CommandConstants memberOrOwner) {
        DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(dim);
        if (dimCache != null) {
            DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
            String playerLangKeyPart = memberOrOwner == CommandConstants.OWNER ? "owner" : "member";
            String affiliationText = playerLangKeyPart.substring(0, 1).toUpperCase() + playerLangKeyPart.substring(1) + "s";
            sendCmdFeedback(src, new TranslatableComponent(ChatFormatting.BOLD + "== " + affiliationText + " in dimension '" + dim.location() + "' ==="));
            sendCmdFeedback(src, buildTeamList(dimRegion, memberOrOwner));
            sendCmdFeedback(src, buildPlayerList(dimRegion, memberOrOwner));
            return 0;
        }
        return 1;
    }

    private static int setActiveState(CommandSourceStack src, ResourceKey<Level> dim, boolean activate) {
        DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(dim);
        if (dimCache != null) {
            dimCache.setDimState(activate);

            String langKey = "cli.msg.info.state." + (activate ? "activated" : "deactivated");
            sendCmdFeedback(src, new TranslatableComponent(langKey, dim.location().toString()));
            return 0;
        }
        return 1;
    }

    // TODO: Rework help to be more interactive (each command clickable
    // TODO: If needed hardcoded at first
    private static int promptHelp(CommandSourceStack source) {
        ResourceKey<Level> dim = source.getLevel().dimension();
        sendCmdFeedback(source, buildHelpHeader("cli.msg.dim.help.header"));
        sendCmdFeedback(source, buildDimHelpLink("cli.msg.dim.help.1", CommandConstants.DIMENSION, new ArrayList<>(Arrays.asList(dim.location().toString(), CommandConstants.HELP.toString()))));
        sendCmdFeedback(source, buildDimHelpLink("cli.msg.dim.help.2", CommandConstants.DIMENSION, new ArrayList<>(Arrays.asList(dim.location().toString(), CommandConstants.LIST.toString()))));
        sendCmdFeedback(source, buildDimHelpLink("cli.msg.dim.help.3", CommandConstants.DIMENSION, new ArrayList<>(Arrays.asList(dim.location().toString(), CommandConstants.ADD.toString(), CommandConstants.PLAYER.toString()))));
        sendCmdFeedback(source, buildDimHelpLink("cli.msg.dim.help.4", CommandConstants.DIMENSION, new ArrayList<>(Arrays.asList(dim.location().toString(), CommandConstants.ADD.toString(), CommandConstants.FLAG.toString()))));
        sendCmdFeedback(source, buildDimHelpLink("cli.msg.dim.help.5", CommandConstants.DIMENSION, new ArrayList<>(Arrays.asList(dim.location().toString(), CommandConstants.INFO.toString()))));
        sendCmdFeedback(source, buildDimHelpLink("cli.msg.dim.help.6", CommandConstants.DIMENSION, new ArrayList<>(Arrays.asList(dim.location().toString(), CommandConstants.ACTIVATE.toString()))));
        return 0;
    }

    // TODO: Check and extract to own method -> dimInfo uses this, too
    private static int promptDimensionRegionList(CommandSourceStack source, ResourceKey<Level> dim) {
        DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(dim);
        if (dimCache != null) {
            List<IMarkableRegion> regionsForDim = dimCache.regionsInDimension
                    .values()
                    .stream()
                    .sorted(Comparator.comparing(IMarkableRegion::getName))
                    .collect(Collectors.toList());
            if (regionsForDim.isEmpty()) {
                sendCmdFeedback(source, new TranslatableComponent("cli.msg.dim.info.regions.empty", dim.location().toString()));
                return -1;
            }
            sendCmdFeedback(source, new TranslatableComponent(ChatFormatting.BOLD + "== Regions in dimension '" + dim.location() + "' ==="));
            regionsForDim.forEach(region -> {
                sendCmdFeedback(source, new TextComponent(" - ")
                        .append(buildDimSuggestRegionRemovalLink(dim, region.getName())
                                .append(buildDimensionRegionInfoLink(dim, region))));
            });
            return 0;
        }
        return 1;
    }

    /* Used for dimension info */
    private static void promptDimensionOwners(CommandSourceStack src, DimensionalRegion dimRegion) {
        // [n player(s)] [+]
        PlayerContainer owners = dimRegion.getOwners();
       MutableComponent playersAddLink = buildDimAddPlayerLink(dimRegion, "cli.msg.dim.info.players.add",
                CommandConstants.OWNER);
       MutableComponent players = owners.hasPlayers()
                ? buildPlayerListLink(dimRegion, owners, CommandConstants.OWNER)
                : new TranslatableComponent(owners.getPlayers().size() + " player(s)");
        players.append(playersAddLink);

        // [n team(s)] [+]
       MutableComponent teamAddLink = buildDimAddTeamLink(dimRegion, "cli.msg.dim.info.teams.add",
                CommandConstants.OWNER);
       MutableComponent teams = owners.hasTeams()
                ? buildTeamListLink(dimRegion, owners, CommandConstants.OWNER)
                : new TranslatableComponent(owners.getTeams().size() + " teams(s)");
        teams.append(teamAddLink);

        // Owners: [n player(s)] [+], [n team(s)] [+]
       MutableComponent dimOwners = new TranslatableComponent("cli.msg.dim.info.owners")
                .append(new TextComponent(": "))
                .append(players).append(new TextComponent(", "))
                .append(teams);
        sendCmdFeedback(src, dimOwners);
    }

    private static void promptDimensionMembers(CommandSourceStack src, DimensionalRegion dimRegion) {
        // [n player(s)] [+]
        PlayerContainer members = dimRegion.getMembers();
       MutableComponent playersAddLink = buildDimAddPlayerLink(dimRegion, "cli.msg.dim.info.players.add",
                CommandConstants.MEMBER);
       MutableComponent players = members.hasPlayers() ?
                buildPlayerListLink(dimRegion, members, CommandConstants.MEMBER)
                // TODO lang-key
                : new TranslatableComponent(members.getPlayers().size() + " player(s)");
        players.append(playersAddLink);

        // [n team(s)] [+]
       MutableComponent teamAddLink = buildDimAddTeamLink(dimRegion, "cli.msg.dim.info.teams.add",
                CommandConstants.MEMBER);
       MutableComponent teams = members.hasTeams()
                ? buildTeamListLink(dimRegion, members, CommandConstants.MEMBER)
                // TODO lang-key
                : new TranslatableComponent(members.getTeams().size() + " teams(s)");
        teams.append(teamAddLink);

        // Members: [n player(s)] [+], [n team(s)] [+]
       MutableComponent dimMembers = new TranslatableComponent("cli.msg.dim.info.members")
                .append(new TextComponent(": "))
                .append(players).append(new TextComponent(", "))
                .append(teams);
        sendCmdFeedback(src, dimMembers);


    }

    private static void promptDimensionFlags(CommandSourceStack src, DimensionalRegion dimRegion) {
       MutableComponent dimFlagMessage = new TranslatableComponent("cli.msg.dim.info.flags");
       MutableComponent flags = dimRegion.getFlags().isEmpty()
                // TODO lang-key
                ? new TextComponent(dimRegion.getFlags().size() + " flags(s)")
                : buildDimFlagListLink(dimRegion);
        dimFlagMessage.append(new TextComponent(": "))
                .append(flags)
                .append(buildAddDimFlagLink(dimRegion));
        sendCmdFeedback(src, dimFlagMessage);
    }

    private static void promptDimensionState(CommandSourceStack src, AbstractRegion region, String command) {
        String onClickAction = region.isActive() ? "deactivate" : "activate";
        String hoverText = "cli.msg.info.state." + onClickAction;
        String linkText = "cli.msg.info.state.link." + (region.isActive() ? "activate" : "deactivate");
        ChatFormatting color = region.isActive() ? ChatFormatting.GREEN : ChatFormatting.RED;
       MutableComponent stateLink = buildExecuteCmdComponent(linkText, command, color, hoverText, ClickEvent.Action.RUN_COMMAND);
        sendCmdFeedback(src, new TranslatableComponent("cli.msg.info.state")
                .append(new TextComponent(": "))
                .append(stateLink));
    }

    private static int promptDimensionInfo(CommandSourceStack src, ResourceKey<Level> dim) {
        DimensionRegionCache cache = RegionDataManager.get().cacheFor(dim);
        DimensionalRegion dimRegion = cache.getDimensionalRegion();

        // Dimension info header
       MutableComponent dimInfoHeader = new TextComponent(ChatFormatting.BOLD + "== Dimension ")
                .append(buildDimensionalInfoLink(dim))
                .append(new TextComponent(ChatFormatting.BOLD + " information =="));
        sendCmdFeedback(src, dimInfoHeader);

        // Dimension owners & members
        promptDimensionOwners(src, dimRegion);
        promptDimensionMembers(src, dimRegion);

        // Flags: [n flag(s)] [+]
        promptDimensionFlags(src, dimRegion);

        // State: [activated]
        String command = "/" + CommandPermissionConfig.BASE_CMD + " " + CommandConstants.DIMENSION + " " + dimRegion.getName() + " " + CommandConstants.ACTIVATE + " " + !dimRegion.isActive();
        promptDimensionState(src, dimRegion, command);
        return 0;
    }
}
