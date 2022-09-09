package de.z0rdak.yawp.commands;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import de.z0rdak.yawp.config.server.CommandPermissionConfig;
import de.z0rdak.yawp.core.affiliation.PlayerContainer;
import de.z0rdak.yawp.core.flag.BooleanFlag;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.AbstractRegion;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.command.CommandSource;
import net.minecraft.command.Commands;
import net.minecraft.command.ISuggestionProvider;
import net.minecraft.command.arguments.DimensionArgument;
import net.minecraft.command.arguments.EntityArgument;
import net.minecraft.command.arguments.TeamArgument;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.scoreboard.ScorePlayerTeam;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.text.*;
import net.minecraft.util.text.event.ClickEvent;
import net.minecraft.world.World;

import java.util.*;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.util.CommandUtil.*;
import static de.z0rdak.yawp.util.CommandUtil.getDimRegionArgument;
import static de.z0rdak.yawp.util.MessageUtil.*;

public class DimensionCommands {

    private DimensionCommands() {
    }

    public static final LiteralArgumentBuilder<CommandSource> DIMENSION_COMMAND = register();

    public static LiteralArgumentBuilder<CommandSource> register() {
        // TODO: Command for managing affiliations and data handling
        List<String> affiliationList = Arrays.asList("member", "owner");
        return literal(CommandConstants.DIMENSION)

                /* /wp dimension help */
                // .then(helpLiteral.executes(ctx -> promptHelp(ctx.getSource())))
                /* /wp dimension <dim> list region */
                .then(Commands.argument(CommandConstants.DIMENSION.toString(), DimensionArgument.dimension())
                        /* /wp dimension <dim> [info] */
                        .executes(ctx -> promptDimensionInfo(ctx.getSource(), getDimCacheArgument(ctx)))
                        .then(literal(CommandConstants.INFO).executes(ctx -> promptDimensionInfo(ctx.getSource(), getDimCacheArgument(ctx))))
                        /* /wp dimension <dim> activate */
                        .then(literal(CommandConstants.ACTIVATE).executes(ctx -> setActiveState(ctx.getSource(), getDimCacheArgument(ctx), getActivateArgument(ctx)))
                                .then(Commands.argument(CommandConstants.ACTIVATE.toString(), BoolArgumentType.bool()).executes(ctx -> setActiveState(ctx.getSource(), getDimCacheArgument(ctx), getActivateArgument(ctx)))))
                        .then(literal(CommandConstants.LIST)
                                .then(literal(CommandConstants.REGION).executes(ctx -> promptDimensionRegionList(ctx.getSource(), getDimCacheArgument(ctx))))
                                /* /wp dimension <dim> list owner */
                                .then(literal(CommandConstants.OWNER).executes(ctx -> promptDimensionPlayerList(ctx.getSource(), getDimCacheArgument(ctx), CommandConstants.OWNER)))
                                /* /wp dimension <dim> list member */
                                .then(literal(CommandConstants.MEMBER).executes(ctx -> promptDimensionPlayerList(ctx.getSource(), getDimCacheArgument(ctx), CommandConstants.MEMBER)))
                                /* /wp dimension <dim> list flag */
                                .then(literal(CommandConstants.FLAG).executes(ctx -> promptDimensionFlagList(ctx.getSource(), getDimCacheArgument(ctx)))))

                        .then(literal(CommandConstants.REMOVE)
                                .then(literal(CommandConstants.PLAYER)
                                        .then(Commands.argument(CommandConstants.AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> ISuggestionProvider.suggest(affiliationList, builder))
                                                .then(Commands.argument(CommandConstants.PLAYER.toString(), EntityArgument.player())
                                                        .executes(ctx -> removePlayer(ctx.getSource(), getPlayerArgument(ctx), getDimCacheArgument(ctx), getAssociateArgument(ctx)))))

                                        .then(Commands.argument(CommandConstants.AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> ISuggestionProvider.suggest(affiliationList, builder))
                                                .then(Commands.argument(CommandConstants.PLAYER.toString(), EntityArgument.player())
                                                        .executes(ctx -> removePlayer(ctx.getSource(), getPlayerArgument(ctx), getDimCacheArgument(ctx), getAssociateArgument(ctx))))))
                                .then(literal(CommandConstants.TEAM)
                                        .then(Commands.argument(CommandConstants.AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> ISuggestionProvider.suggest(affiliationList, builder))
                                                .then(Commands.argument(CommandConstants.TEAM.toString(), TeamArgument.team())
                                                        .executes(ctx -> removeTeam(ctx.getSource(), getTeamArgument(ctx), getDimCacheArgument(ctx), getAssociateArgument(ctx)))))

                                        .then(Commands.argument(CommandConstants.AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> ISuggestionProvider.suggest(affiliationList, builder))
                                                .then(Commands.argument(CommandConstants.TEAM.toString(), TeamArgument.team())
                                                        .executes(ctx -> removeTeam(ctx.getSource(), getTeamArgument(ctx), getDimCacheArgument(ctx), getAssociateArgument(ctx))))))
                                .then(literal(CommandConstants.FLAG)
                                        .then(Commands.argument(CommandConstants.FLAG.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> ISuggestionProvider.suggest(RegionDataManager.get().getFlagsIdsForDim(getDimCacheArgument(ctx)), builder))
                                                .executes(ctx -> removeFlag(ctx.getSource(), getDimCacheArgument(ctx), StringArgumentType.getString(ctx, CommandConstants.FLAG.toString()))))))

                        .then(literal(CommandConstants.ADD)
                                .then(literal(CommandConstants.PLAYER)
                                        .then(Commands.argument(CommandConstants.AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> ISuggestionProvider.suggest(affiliationList, builder))
                                                .then(Commands.argument(CommandConstants.PLAYER.toString(), EntityArgument.player())
                                                        .executes(ctx -> addPlayer(ctx.getSource(), getPlayerArgument(ctx), getDimCacheArgument(ctx), getAssociateArgument(ctx)))))

                                        .then(Commands.argument(CommandConstants.AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> ISuggestionProvider.suggest(affiliationList, builder))
                                                .then(Commands.argument(CommandConstants.PLAYER.toString(), EntityArgument.player())
                                                        .executes(ctx -> addPlayer(ctx.getSource(), getPlayerArgument(ctx), getDimCacheArgument(ctx), getAssociateArgument(ctx))))))
                                .then(literal(CommandConstants.TEAM)
                                        .then(Commands.argument(CommandConstants.AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> ISuggestionProvider.suggest(affiliationList, builder))
                                                .then(Commands.argument(CommandConstants.TEAM.toString(), TeamArgument.team())
                                                        .executes(ctx -> addTeam(ctx.getSource(), getTeamArgument(ctx), getDimCacheArgument(ctx), getAssociateArgument(ctx)))))

                                        .then(Commands.argument(CommandConstants.AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> ISuggestionProvider.suggest(affiliationList, builder))
                                                .then(Commands.argument(CommandConstants.TEAM.toString(), TeamArgument.team())
                                                        .executes(ctx -> addTeam(ctx.getSource(), getTeamArgument(ctx), getDimCacheArgument(ctx), getAssociateArgument(ctx))))))
                                .then(literal(CommandConstants.FLAG)
                                        .then(Commands.argument(CommandConstants.FLAG.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> ISuggestionProvider.suggest(RegionFlag.getFlags(), builder))
                                                .executes(ctx -> addFlag(ctx.getSource(), getDimCacheArgument(ctx), StringArgumentType.getString(ctx, CommandConstants.FLAG.toString())))))));
    }

    public static int selectReferenceDim(CommandSource src, DimensionalRegion dim) {
        RegionCommands.CommandSourceReferenceDims.put(src, dim.getDimensionKey());
        // TODO: Init dim-cache
        MessageUtil.sendCmdFeedback(src, new StringTextComponent("Selected dim '" + dim.getDimensionKey().location().toString() + "' as reference for region commands for '" + src.getTextName() + "'."));
        return 0;
    }

    private static int removeFlag(CommandSource src,  DimensionRegionCache dimCache, String flag) {
        if (dimCache != null) {
            RegistryKey<World> dim = dimCache.dimensionKey();
            dimCache.removeFlag(flag);
            sendCmdFeedback(src, new TranslationTextComponent("cli.msg.flags.removed", flag, dim.location().toString()));
            return 0;
        }
        return 1;
    }

    private static int addFlag(CommandSource src, DimensionRegionCache dimCache, String flag) {
        // TODO: For now this works because we only have condition flags and no black/whitelist feature
        IFlag iflag = new BooleanFlag(flag, false);
        if (dimCache != null) {
            RegistryKey<World> dim = dimCache.dimensionKey();
            dimCache.addFlag(iflag);
            sendCmdFeedback(src, new TranslationTextComponent("cli.msg.flags.added", flag, dim.location().toString()));
            return 0;
        }
        return 1;
    }

    private static int removePlayer(CommandSource src, ServerPlayerEntity player, DimensionRegionCache dimCache, String affiliationType) {
        if (dimCache != null) {
            RegistryKey<World> dim = dimCache.dimensionKey();
            if (affiliationType.equals(CommandConstants.MEMBER.toString())) {
                dimCache.removeMember(player);
            }
            if (affiliationType.equals(CommandConstants.OWNER.toString())) {
                dimCache.removeOwner(player);
            }
            sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info.player.removed", affiliationType, player.getScoreboardName(), dim.location().toString()));
        }
        return 0;
    }

    private static int removeTeam(CommandSource src, ScorePlayerTeam team, DimensionRegionCache dimCache, String affiliationType) {
        if (dimCache != null) {
            RegistryKey<World> dim = dimCache.dimensionKey();
            if (affiliationType.equals(CommandConstants.MEMBER.toString())) {
                dimCache.removeMember(team);
            }
            if (affiliationType.equals(CommandConstants.OWNER.toString())) {
                dimCache.removeOwner(team);
            }
            sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info.player.removed", affiliationType, team.getName(), dim.location().toString()));
            return 0;
        }
        return 1;
    }

    // TODO: If works replace with switch and catch error
    private static int addPlayer(CommandSource src, ServerPlayerEntity player, DimensionRegionCache dimCache, String affiliationType) {
        if (dimCache != null) {
            RegistryKey<World> dim = dimCache.dimensionKey();
            if (affiliationType.equals(CommandConstants.MEMBER.toString())) {
                dimCache.addMember(player);
            }
            if (affiliationType.equals(CommandConstants.OWNER.toString())) {
                dimCache.addOwner(player);
            }
            sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info.player.added", player.getScoreboardName(), dim.location().toString(), affiliationType));
            return 0;
        }
        return 1;
    }

    private static int addTeam(CommandSource src, ScorePlayerTeam team, DimensionRegionCache dimCache, String affiliationType) {
        if (dimCache != null) {
            RegistryKey<World> dim = dimCache.dimensionKey();
            if (affiliationType.equals(CommandConstants.MEMBER.toString())) {
                dimCache.addMember(team);
            }
            if (affiliationType.equals(CommandConstants.OWNER.toString())) {
                dimCache.addOwner(team);
            }
            sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info.team.added", team.getName(), dim.location().toString(), affiliationType));
            return 0;
        }
        return 1;
    }


    // TODO: Check
    private static int promptDimensionFlagList(CommandSource src,  DimensionRegionCache dimCache) {
        List<IFlag> flags = dimCache.getDimensionalRegion().getFlags()
                .stream()
                // TODO: implement comparable for flags
                // .sorted()
                .collect(Collectors.toList());
        RegistryKey<World> dim = dimCache.dimensionKey();
        if (flags.isEmpty()) {
            sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info.flags.empty", dim));
            return 1;
        }
        // TODO: lang key
        sendCmdFeedback(src, new TranslationTextComponent(TextFormatting.BOLD + "== Flags in dimension '" + dim.location() + "' ==="));
        flags.forEach(flag -> {
            IFormattableTextComponent removeFlagLink = new StringTextComponent(" - ")
                    .append(buildDimensionRemoveFlagLink(flag, dim))
                    .append(new StringTextComponent(" '" + flag.getFlagIdentifier() + "' "));

            sendCmdFeedback(src, removeFlagLink);
        });
        return 0;
    }

    private static int promptDimensionPlayerList(CommandSource src,  DimensionRegionCache dimCache, CommandConstants memberOrOwner) {
        if (dimCache != null) {
            DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
            String playerLangKeyPart = memberOrOwner == CommandConstants.OWNER ? "owner" : "member";
            String affiliationText = playerLangKeyPart.substring(0, 1).toUpperCase() + playerLangKeyPart.substring(1) + "s";
            sendCmdFeedback(src, new TranslationTextComponent(TextFormatting.BOLD + "== " + affiliationText + " in dimension '" + dimRegion.getDimensionKey().location() + "' ==="));
            sendCmdFeedback(src, buildTeamList(dimRegion, memberOrOwner));
            sendCmdFeedback(src, buildPlayerList(dimRegion, memberOrOwner));
            return 0;
        }
        return 1;
    }

    private static int setActiveState(CommandSource src, DimensionRegionCache dimCache, boolean activate) {
        if (dimCache != null) {
            dimCache.setDimState(activate);

            String langKey = "cli.msg.info.state." + (activate ? "activated" : "deactivated");
            sendCmdFeedback(src, new TranslationTextComponent(langKey, dimCache.getDimensionalRegion().getDimensionKey().location().toString()));
            return 0;
        }
        return 1;
    }

    // TODO: Rework help to be more interactive (each command clickable
    // TODO: If needed hardcoded at first
    private static int promptHelp(CommandSource source) {
        RegistryKey<World> dim = source.getLevel().dimension();
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
    private static int promptDimensionRegionList(CommandSource source, DimensionRegionCache dimCache) {
        if (dimCache != null) {
            RegistryKey<World> dim =  dimCache.getDimensionalRegion().getDimensionKey();
            List<IMarkableRegion> regionsForDim = dimCache.regionsInDimension
                    .values()
                    .stream()
                    .sorted(Comparator.comparing(IMarkableRegion::getName))
                    .collect(Collectors.toList());
            if (regionsForDim.isEmpty()) {
                sendCmdFeedback(source, new TranslationTextComponent("cli.msg.dim.info.regions.empty", dim.location().toString()));
                return -1;
            }
            sendCmdFeedback(source, new TranslationTextComponent(TextFormatting.BOLD + "== Regions in dimension '" + dim.location() + "' ==="));
            regionsForDim.forEach(region -> {
                sendCmdFeedback(source, new StringTextComponent(" - ")
                        .append(buildDimSuggestRegionRemovalLink(dim, region.getName())
                                .append(buildDimensionRegionInfoLink(dim, region))));
            });
            return 0;
        }
        return 1;
    }

    /* Used for dimension info */
    private static void promptDimensionOwners(CommandSource src, DimensionalRegion dimRegion) {
        // [n player(s)] [+]
        PlayerContainer owners = dimRegion.getOwners();
        IFormattableTextComponent playersAddLink = buildDimAddPlayerLink(dimRegion, "cli.msg.dim.info.players.add",
                CommandConstants.OWNER);
        IFormattableTextComponent players = owners.hasPlayers()
                ? buildPlayerListLink(dimRegion, owners, CommandConstants.OWNER)
                : new TranslationTextComponent(owners.getPlayers().size() + " player(s)");
        players.append(playersAddLink);

        // [n team(s)] [+]
        IFormattableTextComponent teamAddLink = buildDimAddTeamLink(dimRegion, "cli.msg.dim.info.teams.add",
                CommandConstants.OWNER);
        IFormattableTextComponent teams = owners.hasTeams()
                ? buildTeamListLink(dimRegion, owners, CommandConstants.OWNER)
                : new TranslationTextComponent(owners.getTeams().size() + " teams(s)");
        teams.append(teamAddLink);

        // Owners: [n player(s)] [+], [n team(s)] [+]
        IFormattableTextComponent dimOwners = new TranslationTextComponent("cli.msg.dim.info.owners")
                .append(new StringTextComponent(": "))
                .append(players).append(new StringTextComponent(", "))
                .append(teams);
        sendCmdFeedback(src, dimOwners);
    }

    private static void promptDimensionMembers(CommandSource src, DimensionalRegion dimRegion) {
        // [n player(s)] [+]
        PlayerContainer members = dimRegion.getMembers();
        IFormattableTextComponent playersAddLink = buildDimAddPlayerLink(dimRegion, "cli.msg.dim.info.players.add",
                CommandConstants.MEMBER);
        IFormattableTextComponent players = members.hasPlayers() ?
                buildPlayerListLink(dimRegion, members, CommandConstants.MEMBER)
                // TODO lang-key
                : new TranslationTextComponent(members.getPlayers().size() + " player(s)");
        players.append(playersAddLink);

        // [n team(s)] [+]
        IFormattableTextComponent teamAddLink = buildDimAddTeamLink(dimRegion, "cli.msg.dim.info.teams.add",
                CommandConstants.MEMBER);
        IFormattableTextComponent teams = members.hasTeams()
                ? buildTeamListLink(dimRegion, members, CommandConstants.MEMBER)
                // TODO lang-key
                : new TranslationTextComponent(members.getTeams().size() + " teams(s)");
        teams.append(teamAddLink);

        // Members: [n player(s)] [+], [n team(s)] [+]
        IFormattableTextComponent dimMembers = new TranslationTextComponent("cli.msg.dim.info.members")
                .append(new StringTextComponent(": "))
                .append(players).append(new StringTextComponent(", "))
                .append(teams);
        sendCmdFeedback(src, dimMembers);


    }

    private static void promptDimensionFlags(CommandSource src, DimensionalRegion dimRegion) {
        IFormattableTextComponent dimFlagMessage = new TranslationTextComponent("cli.msg.dim.info.flags", buildDimFlagListLink(dimRegion));
        IFormattableTextComponent flags = dimRegion.getFlags().isEmpty()
                // TODO lang-key
                ? new StringTextComponent(dimRegion.getFlags().size() + " flags(s)")
                : buildDimFlagListLink(dimRegion);
        dimFlagMessage.append(new StringTextComponent(": "))
                .append(flags)
                .append(buildAddDimFlagLink(dimRegion));
        sendCmdFeedback(src, dimFlagMessage);
    }

    private static void promptDimensionState(CommandSource src, AbstractRegion region, String command) {
        String onClickAction = region.isActive() ? "deactivate" : "activate";
        String hoverText = "cli.msg.info.state." + onClickAction;
        String linkText = "cli.msg.info.state.link." + (region.isActive() ? "activate" : "deactivate");
        TextFormatting color = region.isActive() ? TextFormatting.GREEN : TextFormatting.RED;
        IFormattableTextComponent stateLink = buildExecuteCmdComponent(linkText, command, color, hoverText, ClickEvent.Action.RUN_COMMAND);
        sendCmdFeedback(src, new TranslationTextComponent("cli.msg.info.state")
                .append(new StringTextComponent(": "))
                .append(stateLink));
    }

    private static int promptDimensionInfo(CommandSource src, DimensionRegionCache dimCache) {
        // Dimension info header
        DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
        IFormattableTextComponent dimInfoHeader = new StringTextComponent(TextFormatting.BOLD + "== Dimension ")
                .append(buildDimensionalInfoLink(dimRegion.getDimensionKey()))
                .append(new StringTextComponent(TextFormatting.BOLD + " information =="));
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
