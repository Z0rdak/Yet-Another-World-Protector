package de.z0rdak.regionshield.commands;

import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.builder.RequiredArgumentBuilder;
import de.z0rdak.regionshield.core.affiliation.PlayerContainer;
import de.z0rdak.regionshield.core.flag.ConditionFlag;
import de.z0rdak.regionshield.core.flag.IFlag;
import de.z0rdak.regionshield.core.flag.RegionFlag;
import de.z0rdak.regionshield.core.region.AbstractRegion;
import de.z0rdak.regionshield.core.region.DimensionalRegion;
import de.z0rdak.regionshield.core.region.IMarkableRegion;
import de.z0rdak.regionshield.managers.data.region.DimensionRegionCache;
import de.z0rdak.regionshield.managers.data.region.RegionDataManager;
import de.z0rdak.regionshield.util.CommandUtil;
import de.z0rdak.regionshield.util.MessageUtil;
import net.minecraft.command.CommandSource;
import net.minecraft.command.Commands;
import net.minecraft.command.ISuggestionProvider;
import net.minecraft.command.arguments.TeamArgument;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.scoreboard.ScorePlayerTeam;
import net.minecraft.scoreboard.Team;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.text.*;
import net.minecraft.util.text.event.ClickEvent;
import net.minecraft.world.World;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

import static de.z0rdak.regionshield.util.CommandUtil.*;
import static de.z0rdak.regionshield.util.CommandUtil.getDimensionArgument;
import static de.z0rdak.regionshield.util.MessageUtil.*;

public class DimensionCommands {

    private DimensionCommands() {
    }

    public static final LiteralArgumentBuilder<CommandSource> DIMENSION_COMMAND = register();

    public static LiteralArgumentBuilder<CommandSource> register() {
        return dimensionLiteral
                .executes(ctx -> promptHelp(ctx.getSource()))
                /* /wp dimension [help] */
                .then(helpLiteral.executes(ctx -> promptHelp(ctx.getSource())))
                /* /wp dimension <dim> list region */
                .then(dimensionArgument.then(listLiteral.then(regionLiteral
                        .executes(ctx -> promptDimensionRegionList(ctx.getSource(), getDimensionArgument(ctx))))))
                /* /wp dimension <dim> list owner */
                .then(dimensionArgument.then(listLiteral.then(ownerLiteral
                        .executes(ctx -> promptDimensionPlayerList(ctx.getSource(), getDimensionArgument(ctx), CommandConstants.OWNER)))))
                /* /wp dimension <dim> list member */
                .then(dimensionArgument.then(listLiteral.then(memberLiteral
                        .executes(ctx -> promptDimensionPlayerList(ctx.getSource(), getDimensionArgument(ctx), CommandConstants.MEMBER)))))
                /* /wp dimension <dim> list flag */
                .then(dimensionArgument.then(listLiteral.then(flagLiteral
                        .executes(ctx -> promptDimensionFlagList(ctx.getSource(), getDimensionArgument(ctx))))))

                /* /wp dim <dim> remove player owner <player> */
                .then(dimensionArgument.then(removeLiteral.then(playerLiteral.then(ownerLiteral.then(playerArgument
                        .executes(ctx -> removePlayer(ctx.getSource(), getPlayerArgument(ctx), getDimensionArgument(ctx), CommandConstants.OWNER)))))))
                /* /wp dim <dim> remove team owner <team> */
                .then(dimensionArgument.then(removeLiteral.then(teamLiteral.then(ownerLiteral.then(teamArgument
                        .executes(ctx -> removeTeam(ctx.getSource(), getTeamArgument(ctx), getDimensionArgument(ctx), CommandConstants.OWNER)))))))
                /* /wp dim <dim> remove player member <player> */
                .then(dimensionArgument.then(removeLiteral.then(playerLiteral.then(memberLiteral.then(playerArgument
                        .executes(ctx -> removePlayer(ctx.getSource(), getPlayerArgument(ctx), getDimensionArgument(ctx), CommandConstants.MEMBER)))))))
                /* /wp dim <dim> remove team member <team> */
                .then(dimensionArgument.then(removeLiteral.then(teamLiteral.then(memberLiteral.then(teamArgument
                        .executes(ctx -> removeTeam(ctx.getSource(), getTeamArgument(ctx), getDimensionArgument(ctx), CommandConstants.MEMBER)))))))

                /* /wp dim <dim> add player owner <player> */
                .then(dimensionArgument.then(addLiteral.then(playerLiteral.then(ownerLiteral.then(playerArgument
                        .executes(ctx -> addPlayer(ctx.getSource(), getPlayerArgument(ctx), getDimensionArgument(ctx), CommandConstants.OWNER)))))))
                /* /wp dim <dim> add team owner <team> */
                .then(dimensionArgument.then(addLiteral.then(teamLiteral.then(ownerLiteral.then(teamArgument
                        .executes(ctx -> addTeam(ctx.getSource(), getTeamArgument(ctx), getDimensionArgument(ctx), CommandConstants.OWNER)))))))
                /* /wp dim <dim> add player member <player> */
                .then(dimensionArgument.then(addLiteral.then(playerLiteral.then(memberLiteral.then(playerArgument
                        .executes(ctx -> addPlayer(ctx.getSource(), getPlayerArgument(ctx), getDimensionArgument(ctx), CommandConstants.MEMBER)))))))
                /* /wp dim <dim> add team member <team> */
                .then(dimensionArgument.then(addLiteral.then(teamLiteral.then(memberLiteral.then(teamArgument
                        .executes(ctx -> addTeam(ctx.getSource(), getTeamArgument(ctx), getDimensionArgument(ctx), CommandConstants.MEMBER)))))))

                /* /wp dimension <dim> add flag <flag> */
                .then(dimensionArgument.then(addLiteral.then(flagLiteral.then(flagArgument
                        .suggests((ctx, builder) -> ISuggestionProvider.suggest(RegionFlag.getFlags(), builder))
                        .executes(ctx -> addFlag(ctx.getSource(), CommandUtil.getDimensionArgument(ctx), StringArgumentType.getString(ctx, CommandConstants.FLAG.toString())))))))
                /* /wp dimension <dim> remove flag <flag> */
                .then(dimensionArgument.then(removeLiteral.then(flagLiteral.then(flagArgument
                        .suggests((ctx, builder) -> ISuggestionProvider.suggest(RegionDataManager.get().cacheFor(getDimensionArgument(ctx)).getDimFlags(), builder))
                        .executes(ctx -> removeFlag(ctx.getSource(), CommandUtil.getDimensionArgument(ctx), StringArgumentType.getString(ctx, CommandConstants.FLAG.toString())))))))

                /* /wp dimension <dim> [info] */
                .then(dimensionArgument.executes(ctx -> promptDimensionInfo(ctx.getSource(), CommandUtil.getDimensionArgument(ctx)))
                        .then(infoLiteral.executes(ctx -> promptDimensionInfo(ctx.getSource(), CommandUtil.getDimensionArgument(ctx)))))
                /* /wp dimension <dim> activate */
                .then(dimensionArgument.then(activateLiteral.executes(ctx -> setActiveState(ctx.getSource(), getDimensionArgument(ctx), getActivateArgument(ctx)))
                        .then(activateArgument.executes(ctx -> setActiveState(ctx.getSource(), getDimensionArgument(ctx), getActivateArgument(ctx))))));
    }

    private static int removeFlag(CommandSource src, RegistryKey<World> dim, String flag) {
        RegionDataManager.get().cacheFor(dim).removeFlag(flag);
        sendCmdFeedback(src, new TranslationTextComponent("cli.msg.flags.removed", dim.location().toString()));
        return 0;
    }

    private static int addFlag(CommandSource src, RegistryKey<World> dim, String flag) {
        // TODO: For now this works because we only have condition flags and no black/whitelist feature
        IFlag iflag = new ConditionFlag(flag, false);
        RegionDataManager.get().cacheFor(dim).addFlag(iflag);
        sendCmdFeedback(src, new TranslationTextComponent("cli.msg.flags.added", dim.location().toString()));
        return 0;
    }
    private static int removePlayer(CommandSource src, ServerPlayerEntity player, RegistryKey<World> dim, CommandConstants memberOrOwner) {
        if (memberOrOwner == CommandConstants.MEMBER) {
            RegionDataManager.get().cacheFor(dim).removeMember(player);
        }
        if (memberOrOwner == CommandConstants.OWNER) {
            RegionDataManager.get().cacheFor(dim).removeOwner(player);
        }
        sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info." + memberOrOwner.toString() + ".player.removed", dim.location().toString()));
        return 0;
    }

    private static int removeTeam(CommandSource src, ScorePlayerTeam team, RegistryKey<World> dim, CommandConstants memberOrOwner) {
        if (memberOrOwner == CommandConstants.MEMBER) {
            RegionDataManager.get().cacheFor(dim).removeMember(team);
        }
        if (memberOrOwner == CommandConstants.OWNER) {
            RegionDataManager.get().cacheFor(dim).removeOwner(team);
        }
        sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info." + memberOrOwner.toString() + ".team.removed", dim.location().toString()));
        return 0;
    }


    private static int addPlayer(CommandSource src, ServerPlayerEntity player, RegistryKey<World> dim, CommandConstants memberOrOwner) {
        if (memberOrOwner == CommandConstants.MEMBER) {
            RegionDataManager.get().cacheFor(dim).addMember(player);
        }
        if (memberOrOwner == CommandConstants.OWNER) {
            RegionDataManager.get().cacheFor(dim).addOwner(player);
        }
        sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info." + memberOrOwner.toString() + ".player.added", dim.location().toString()));
        return 0;
    }

    private static int addTeam(CommandSource src, ScorePlayerTeam team, RegistryKey<World> dim, CommandConstants memberOrOwner) {
        if (memberOrOwner == CommandConstants.MEMBER) {
            RegionDataManager.get().cacheFor(dim).addMember(team);
        }
        if (memberOrOwner == CommandConstants.OWNER) {
            RegionDataManager.get().cacheFor(dim).addOwner(team);
        }
        sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info." + memberOrOwner.toString() + ".team.added", dim.location().toString()));
        return 0;
    }


    // TODO: Check
    private static int promptDimensionFlagList(CommandSource src, RegistryKey<World> dim) {
        List<IFlag> flags = RegionDataManager.get().dimFor(dim).getFlags()
                .stream()
                // TODO: implement comarable for flags
                // .sorted()
                .collect(Collectors.toList());
        if (flags.isEmpty()) {
            sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info.flags.empty", dim));
            return 1;
        }
        // TODO: lang key
        sendCmdFeedback(src, new TranslationTextComponent(TextFormatting.BOLD + "== Flags in dimension '" + dim.location() + "' ==="));
        flags.forEach(flag -> {
            IFormattableTextComponent removeFlagLink = new StringTextComponent(" - ")
                    .append(buildDimensionRemoveFlagLink(flag, dim))
                    .append(new StringTextComponent(" '" + flag.getFlagName() + "' "));

            sendCmdFeedback(src, removeFlagLink);
        });
        return 0;
    }

    // TODO: Check
    private static int promptDimensionPlayerList(CommandSource src, RegistryKey<World> dim, CommandConstants memberOrOwner) {
        List<String> playerNames = new ArrayList<>();
        switch (memberOrOwner) {
            case OWNER:
                playerNames = RegionDataManager.get().dimFor(dim).getOwners().getPlayers()
                        .values()
                        .stream()
                        .sorted()
                        .collect(Collectors.toList());
                break;
            case MEMBER:
                playerNames = RegionDataManager.get().dimFor(dim).getMembers().getPlayers()
                        .values()
                        .stream()
                        .sorted()
                        .collect(Collectors.toList());
                break;
            default:
                break;
        }
        String playerLangKeyPart = memberOrOwner == CommandConstants.OWNER ? "owner" : "member";
        if (playerNames.isEmpty()) {
            sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info." + playerLangKeyPart + ".players.empty", dim));
            return -1;
        }
        // TODO: lang key
        sendCmdFeedback(src, new TranslationTextComponent(TextFormatting.BOLD + "== Players(" + playerLangKeyPart + ") in dimension '" + dim.location() + "' ==="));
        playerNames.forEach(playerName -> {
            IFormattableTextComponent removePlayerLink = new StringTextComponent(" - ")
                    .append(buildDimensionRemovePlayerLink(playerName, dim, memberOrOwner))
                    .append(new StringTextComponent(" '" + playerName + "'"));;
            sendCmdFeedback(src, removePlayerLink);
        });
        return 0;
    }

    // FIXME: only activates
    private static int setActiveState(CommandSource src, RegistryKey<World> dim, boolean activate) {
        RegionDataManager.get().dimFor(dim).setIsActive(activate);
        String langKey = "cli.msg.info.state." + (activate ? "activated" : "deactivated");
        sendCmdFeedback(src, new TranslationTextComponent(langKey, dim.location().toString()));
        return 0;
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
    private static int promptDimensionRegionList(CommandSource source, RegistryKey<World> dim) {
        List<IMarkableRegion> regionsForDim = RegionDataManager.get().cacheFor(dim).regionsInDimension
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

    /**
     * Region owners: [n player(s)] [+], [n team(s)] [+]
     * @param src
     * @param dimRegion
     */
    private static void promptDimensionOwners(CommandSource src, DimensionalRegion dimRegion) {
        // [n player(s)] [+]
        PlayerContainer owners = dimRegion.getOwners();
        IFormattableTextComponent playersAddLink = buildDimAddPlayerLink(dimRegion, "cli.msg.dim.info.owners.players.add",
                CommandConstants.OWNER);
        IFormattableTextComponent players = owners.hasPlayers()
                ? buildPlayerListLink(dimRegion, owners, CommandConstants.OWNER)
                : new TranslationTextComponent( owners.getPlayers().size() + " player(s)");
        players.append(playersAddLink);

        // [n team(s)] [+]
        IFormattableTextComponent teamAddLink = buildDimAddTeamLink(dimRegion, "cli.msg.dim.info.owners.teams.add",
                CommandConstants.OWNER);
        IFormattableTextComponent teams = owners.hasTeams()
                ? buildTeamListLink(dimRegion, owners, CommandConstants.OWNER)
                : new TranslationTextComponent( owners.getTeams().size() + " teams(s)");
        teams.append(teamAddLink);

        // Owners: [n player(s)] [+], [n team(s)] [+]
        IFormattableTextComponent dimOwners = new TranslationTextComponent("cli.msg.dim.info.owners")
                .append(new StringTextComponent(": "))
                        .append(players).append(new StringTextComponent(", "))
                        .append(teams);
        sendCmdFeedback(src, dimOwners);
    }

    /**
     *  Region members: [n player(s)] [+], [n team(s)] [+]
     * @param src
     * @param dimRegion
     */
    private static void promptDimensionMembers(CommandSource src, DimensionalRegion dimRegion) {
        // [n player(s)] [+]
        PlayerContainer members = dimRegion.getMembers();
        IFormattableTextComponent playersAddLink = buildDimAddPlayerLink(dimRegion, "cli.msg.dim.info.members.players.add",
                CommandConstants.MEMBER);
        IFormattableTextComponent players = members.hasPlayers() ?
                buildPlayerListLink(dimRegion, members, CommandConstants.MEMBER)
                : new TranslationTextComponent( members.getPlayers().size() + " player(s)");
        players.append(playersAddLink);

        // [n team(s)] [+]
        IFormattableTextComponent teamAddLink = buildDimAddTeamLink(dimRegion, "cli.msg.dim.info.members.teams.add",
                CommandConstants.MEMBER);
        IFormattableTextComponent teams = members.hasTeams()
                ? buildTeamListLink(dimRegion, members, CommandConstants.MEMBER)
                : new TranslationTextComponent( members.getTeams().size() + " teams(s)");
        teams.append(teamAddLink);

        // Members: [n player(s)] [+], [n team(s)] [+]
        IFormattableTextComponent dimMembers = new TranslationTextComponent("cli.msg.dim.info.members")
                .append(new StringTextComponent(": "))
                .append(players).append(new StringTextComponent(", "))
                .append(teams);
        sendCmdFeedback(src, dimMembers);


    }

    /**
     * Dimension flags: [n flag(s)] [+]
     * @param src
     * @param dimRegion
     */
    private static void promptDimensionFlags(CommandSource src, DimensionalRegion dimRegion){
        IFormattableTextComponent dimFlagMessage = new TranslationTextComponent("cli.msg.dim.info.flags", buildDimFlagListLink(dimRegion));
        IFormattableTextComponent flags = dimRegion.getFlags().isEmpty()
                ? new StringTextComponent(dimRegion.getFlags().size() + " flags(s)")
                : buildDimFlagListLink(dimRegion);
        dimFlagMessage.append(new StringTextComponent(": "))
                .append(flags)
                .append(buildAddDimFlagLink(dimRegion));
        sendCmdFeedback(src, dimFlagMessage);
    }

    private static void promptDimensionState(CommandSource src, AbstractRegion region, String command){
        String onClickAction = region.isActive() ? "deactivate" : "activate";
        String hoverText = "cli.msg.info.state." + onClickAction;
        String linkText = "cli.msg.info.state.link." + (region.isActive() ? "activate" : "deactivate");
        TextFormatting color = region.isActive() ? TextFormatting.GREEN : TextFormatting.RED;
        IFormattableTextComponent stateLink = buildExecuteCmdComponent(linkText, command, color, hoverText, ClickEvent.Action.RUN_COMMAND);
        sendCmdFeedback(src, new TranslationTextComponent("cli.msg.info.state")
                .append(new StringTextComponent(": "))
                .append(stateLink));
    }

    // TODO: Check
    private static int promptDimensionInfo(CommandSource src, RegistryKey<World> dim) {
        DimensionRegionCache cache = RegionDataManager.get().cacheFor(dim);
        DimensionalRegion dimRegion = cache.getDimensionalRegion();

        // Dimension info header
        IFormattableTextComponent dimInfoHeader = new StringTextComponent(TextFormatting.BOLD + "== Dimension ")
                .append(buildDimensionalInfoLink(dim))
                .append(new StringTextComponent(TextFormatting.BOLD + " information =="));
        sendCmdFeedback(src, dimInfoHeader);

        // Dimension owners & members
        promptDimensionOwners(src, dimRegion);
        promptDimensionMembers(src, dimRegion);

        // Flags: [n flag(s)] [+]
        promptDimensionFlags(src, dimRegion);

        // State: [activated]
        String command = "/" + CommandConstants.BASE_CMD + " " + CommandConstants.DIMENSION + " " + dimRegion.getName() + " " + CommandConstants.ACTIVATE + " " + !dimRegion.isActive();
        promptDimensionState(src, dimRegion, command);
        return 0;
    }
}
