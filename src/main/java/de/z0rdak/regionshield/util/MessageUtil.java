package de.z0rdak.regionshield.util;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.regionshield.RegionShield;
import de.z0rdak.regionshield.commands.CommandConstants;
import de.z0rdak.regionshield.core.affiliation.PlayerContainer;
import de.z0rdak.regionshield.core.flag.IFlag;
import de.z0rdak.regionshield.core.region.AbstractMarkableRegion;
import de.z0rdak.regionshield.core.region.AbstractRegion;
import de.z0rdak.regionshield.core.region.DimensionalRegion;
import de.z0rdak.regionshield.core.region.IMarkableRegion;
import net.minecraft.command.CommandSource;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.text.*;
import net.minecraft.util.text.event.ClickEvent;
import net.minecraft.util.text.event.HoverEvent;
import net.minecraft.world.World;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static de.z0rdak.regionshield.commands.CommandConstants.*;
import static de.z0rdak.regionshield.util.CommandUtil.*;
import static net.minecraft.util.text.TextFormatting.*;
import static net.minecraft.util.text.event.ClickEvent.Action.*;

public class MessageUtil {

    private MessageUtil() {
    }

    public static IFormattableTextComponent buildHelpLink(String translationKey, CommandConstants cmd) {
        String command =  "/" + BASE_CMD + " " + cmd.toString() + " " + HELP;
        return new StringTextComponent(" ")
                .append(buildExecuteCmdComponent("=>", command, GREEN, "Show detailed help for the " + cmd.toString() + " commands",  RUN_COMMAND))
                .append(new StringTextComponent(" "))
                .append(new TranslationTextComponent(translationKey));
    }

    public static IFormattableTextComponent buildHelpHeader(String translationKey){
        return new StringTextComponent(BOLD + " == ")
                .append(new TranslationTextComponent(translationKey).setStyle(Style.EMPTY.withBold(true)))
                .append(new StringTextComponent(BOLD + " == "));
    }

    public static IFormattableTextComponent buildHelpHeader(TranslationTextComponent translationTextComponent){
        return new StringTextComponent(BOLD + " == ")
                .append(translationTextComponent)
                .append(new StringTextComponent(BOLD + " == "));
    }

    public static void sendCmdFeedback(CommandSource src, IFormattableTextComponent text) {
        try {
            if (src.getEntity() == null) {
                src.sendSuccess(text, true);
            } else {
                sendMessage(src.getPlayerOrException(), text);
            }
        } catch (CommandSyntaxException e) {
            RegionShield.LOGGER.error(e);
        }
    }

    public static void sendCmdFeedback(CommandSource src, String langKey) {
        try {
            TranslationTextComponent text = new TranslationTextComponent(langKey);
            if (src.getEntity() == null) {
                src.sendSuccess(text, true);
            } else {
                MessageUtil.sendMessage(src.getPlayerOrException(), text);
            }
        } catch (CommandSyntaxException e) {
            RegionShield.LOGGER.error(e);
        }
    }

    public static void sendMessage(PlayerEntity player, ITextComponent textComponent) {
        player.sendMessage(textComponent, player.getUUID());
    }

    public static void sendMessage(PlayerEntity player, String translationKey) {
        player.sendMessage(new TranslationTextComponent(translationKey), player.getUUID());
    }

    private static String format(double value) {
        return String.format("%.2f", value);
    }

    // TODO: overload for TranslationTextComponents
    public static IFormattableTextComponent buildExecuteCmdComponent(String linkText, String command, TextFormatting color, String hoverText, ClickEvent.Action eventAction){
        return TextComponentUtils.wrapInSquareBrackets(new TranslationTextComponent(linkText))
                .setStyle(Style.EMPTY.withColor(Color.fromLegacyFormat(color))
                        .withClickEvent(new ClickEvent(eventAction, command))
                        .withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, new TranslationTextComponent(hoverText))));
    }

    public static IFormattableTextComponent buildDimensionTeleportLink(AbstractMarkableRegion region) {
        String cmdLinkText = buildTeleportLinkText(region.getDim(), region.getTpTarget());
        String executeCmdStr = buildDimTeleportCmd(region.getDim(), "@s", region.getTpTarget());
        return buildExecuteCmdComponent(cmdLinkText, executeCmdStr, GREEN, "chat.link.hover.region.tp", RUN_COMMAND);
    }

    public static String buildTeleportCmd(String tpSource, BlockPos target){
        return "tp " + tpSource + " " + target.getX() + " " + target.getY() + " " + target.getZ();
    }

    public static String buildTeleportLinkText(RegistryKey<World> dim, BlockPos target){
        return dim + "@ [" + target.getX() + ", " + target.getY() + ", " + target.getZ() + "]";
    }

    public static String buildExecuteCommandString(RegistryKey<World> dim, String command){
        return "/execute in " + dim.location() + " run " + command;
    }

    public static String buildDimTeleportCmd(RegistryKey<World> dim, String tpSource, BlockPos target){
        return buildExecuteCommandString(dim, buildTeleportCmd(tpSource, target));
    }

    public static IFormattableTextComponent buildHelpSuggestionLink(String translationKey, CommandConstants baseCmd, CommandConstants cmd) {
        String command = "/" + BASE_CMD + " " + baseCmd + " " + cmd + " ";
        return new StringTextComponent(" ")
                .append(buildExecuteCmdComponent("=>", command, GREEN, "chat.link.hover.command.copy", SUGGEST_COMMAND))
                .append(new StringTextComponent(" "))
                .append(new TranslationTextComponent(translationKey));
    }

    public static IFormattableTextComponent buildDimHelpLink(String translationKey, CommandConstants baseCmd, List<String> cmds) {
        String cmdStr = String.join(" ", cmds);
        String command = "/" + BASE_CMD + " " + baseCmd + " " + cmdStr;
        return new StringTextComponent(" ")
                .append(buildExecuteCmdComponent("=>", command, GREEN, "chat.link.hover.command.copy", SUGGEST_COMMAND))
                .append(new StringTextComponent(" "))
                .append(new TranslationTextComponent(translationKey));
    }

    public static IFormattableTextComponent buildInteractiveDimHelpLink(String translationKey, CommandConstants baseCmd, List<String> cmds) {
        String cmdStr = String.join(" ", cmds);
        // TODO
        String command = "/" + BASE_CMD + " " + baseCmd + " " + cmdStr;
        return new StringTextComponent(" ")
                .append(buildExecuteCmdComponent("=>", command, GREEN, "chat.link.hover.command.copy", SUGGEST_COMMAND))
                .append(new StringTextComponent(" "))
                .append(new TranslationTextComponent(translationKey));
    }

    public static String buildDimAddPlayerCmdStr(String region, CommandConstants memberOrOwner){
        return buildDimAddCmdStr(region) + " " + PLAYER + " " + memberOrOwner;
    }

    public static String buildDimAddTeamCmdStr(String region, CommandConstants memberOrOwner){
        return buildDimAddCmdStr(region) + " " + TEAM + " " + memberOrOwner;
    }

    public static String buildDimAddCmdStr(String region){
        return "/" + BASE_CMD + " " + DIMENSION + " " + region + " " + ADD;
    }

    public static IFormattableTextComponent buildDimAddPlayerLink(DimensionalRegion dimRegion, String hoverTextLangKey, CommandConstants memberOrOwner) {
        String command = buildDimAddPlayerCmdStr(dimRegion.getName(), memberOrOwner);
        String hoverText = hoverTextLangKey + " '" + dimRegion.getName() + "'";
        String linkText = "+";
        return buildExecuteCmdComponent(linkText, command, GREEN, hoverText, SUGGEST_COMMAND);
    }


    // TODO: generalize with Supplier<String> (regionName) -> build....(regionName)
    public static IFormattableTextComponent buildDimAddTeamLink(DimensionalRegion dimRegion, String hoverTextLangKey, CommandConstants memberOrOwner) {
        return buildExecuteCmdComponent("+", buildDimAddTeamCmdStr(dimRegion.getName(), memberOrOwner),
                GREEN, hoverTextLangKey, SUGGEST_COMMAND);
    }

    public static IFormattableTextComponent buildDimStateActiveLink(AbstractRegion region){
        boolean isActive = region.isActive();
        IFormattableTextComponent activeText = isActive
                ? new TranslationTextComponent("message.region.info.active.true")
                : new TranslationTextComponent("message.region.info.active.false");
        TextFormatting color = isActive
                ? GREEN
                : RED;
        String onClickAction = isActive ? "deactivate" : "activate";
        String command = "/" + BASE_CMD + " " + REGION + " " + onClickAction + " " + region.getName();
        // TODO: translatable overload (linkText) ?
        return buildExecuteCmdComponent(activeText.getString(), command, color, onClickAction + " " + REGION + " '" + region.getName() + "'", RUN_COMMAND);
    }

    public static IFormattableTextComponent buildDimensionalInfoLink(RegistryKey<World> dim) {
        String command = "/" + BASE_CMD + " " + DIMENSION + " " + dim.location() + " " + INFO;
        String hoverText = "cli.msg.dim.info";
        return buildExecuteCmdComponent(dim.location().toString(), command, GREEN, hoverText, RUN_COMMAND);
    }

    public static IFormattableTextComponent buildPlayerListLink(DimensionalRegion dimRegion, PlayerContainer players, CommandConstants memberOrOwner) {
        String command = "/" + BASE_CMD + " " + DIMENSION + " " + dimRegion.getName() + " " + LIST + " " + memberOrOwner;
        String hoverText = "List players(" + memberOrOwner.toString() + ") in dimension '" + dimRegion.getName() + "'";
        String linkText = players.getPlayers().size() + " player(s)";
        return buildExecuteCmdComponent(linkText, command, AQUA, hoverText, RUN_COMMAND);
    }

    public static IFormattableTextComponent buildTeamListLink(DimensionalRegion dimRegion, PlayerContainer teams, CommandConstants memberOrOwner) {
        String command = "/" + BASE_CMD + " " + DIMENSION + " " + dimRegion.getName() + " " + LIST + " " + memberOrOwner;
        String hoverText = "List teams(" + memberOrOwner.toString() + ") in dimension '" + dimRegion.getName() + "'";
        String linkText = teams.getTeams().size() + " teams(s)";
        return buildExecuteCmdComponent(linkText, command, AQUA, hoverText, RUN_COMMAND);
    }

    public static IFormattableTextComponent buildDimensionRegionInfoLink(RegistryKey<World> dim, IMarkableRegion region){
        BlockPos target = region.getTpTarget();
        String regionInfoCommand = buildCommandStr(REGION.toString(), INFO.toString(), region.getName(),dim.location().toString());
        IFormattableTextComponent regionInfoLink = buildExecuteCmdComponent(region.getName(), regionInfoCommand,
                GREEN, "cli.msg.region.info.link.hover", RUN_COMMAND);
        IFormattableTextComponent teleportLink = buildExecuteCmdComponent(buildTeleportLinkText(dim, target),
                buildDimTeleportCmd(dim, "@s", target),
                GREEN, "cli.msg.region.info.tp.link.hover" , RUN_COMMAND);
        return  regionInfoLink
                .append(new StringTextComponent(": ").setStyle(Style.EMPTY.withColor(Color.fromLegacyFormat(TextFormatting.RESET))))
                .append(teleportLink);
    }

    public static IFormattableTextComponent buildDimSuggestRegionRemovalLink(RegistryKey<World> dim, String regionName) {
        String command = "/" + BASE_CMD + " " + REGION + " " + dim.location() + " " + REMOVE + " " + regionName;
        String hoverText = "cli.msg.dim.region.remove.link.hover";
        String linkText = "x";
        return buildExecuteCmdComponent(linkText, command, RED,hoverText, SUGGEST_COMMAND);

    }

    public static IFormattableTextComponent buildAddDimFlagLink(DimensionalRegion dimRegion) {
        String command = buildCommandStr(DIMENSION.toString(), dimRegion.getName(), ADD.toString(), FLAG.toString());
        String hoverText = "cli.msg.dim.flag.add.link.hover";
        String linkText = "+";
        return buildExecuteCmdComponent(linkText, command, GREEN, hoverText, SUGGEST_COMMAND);
    }

    public static IFormattableTextComponent buildDimFlagListLink(DimensionalRegion dimRegion) {
        String command = buildCommandStr(DIMENSION.toString(), dimRegion.getName(), LIST.toString(), FLAG.toString());
        String hoverText = "List flags in dimension '" + dimRegion.getName() + "'";
        String linkText = dimRegion.getFlags().size() + " flags(s)";
        return buildExecuteCmdComponent(linkText, command, AQUA, hoverText, RUN_COMMAND);
    }

    public static IFormattableTextComponent buildDimRemovePlayerLink(String playerName, RegistryKey<World> dim, CommandConstants memberOrOwner) {
        String command = buildCommandStr(DIMENSION.toString(), dim.location().toString(), REMOVE.toString(), PLAYER.toString(), memberOrOwner.toString(), playerName);
        String hoverText = "Remove player '" + playerName + "' from dimension " + "'" + dim.location() + "'";
        String linkText = "x";
        return buildExecuteCmdComponent(linkText, command, RED, hoverText, SUGGEST_COMMAND);
    }

    public static IFormattableTextComponent buildDimRemoveTeamLink(String teamName, RegistryKey<World> dim, CommandConstants memberOrOwner) {
        String command = buildCommandStr(DIMENSION.toString(), dim.location().toString(), REMOVE.toString(), TEAM.toString(), memberOrOwner.toString(), teamName);
        String hoverText = "Remove team '" + teamName + "' from dimension " + "'" + dim.location() + "'";
        String linkText = "x";
        return buildExecuteCmdComponent(linkText, command, RED, hoverText, SUGGEST_COMMAND);
    }

    public static IFormattableTextComponent buildDimensionRemoveFlagLink(IFlag flag, RegistryKey<World> dim) {
        String command =  "/" + BASE_CMD + " " + DIMENSION + " " + dim.location() + " " + REMOVE + " " + FLAG + " " + flag.getFlagName();
        String hoverText =" Remove flag '" + flag.getFlagName() + "' from dimension " + "'" + dim.location() + "'";
        String linkText = "x";
        return buildExecuteCmdComponent(linkText, command, RED, hoverText, SUGGEST_COMMAND);
    }

    public static IFormattableTextComponent buildTeamList(DimensionalRegion dimCache, CommandConstants memberOrOwner) {
        Set<String> teamNames = getAssociateList(dimCache, memberOrOwner, TEAM);
        String playerLangKeyPart = memberOrOwner == CommandConstants.OWNER ? "owner" : "member";
        IFormattableTextComponent teamList = new StringTextComponent("Teams: ");
        if (teamNames.isEmpty()) {
            teamList.append(new TranslationTextComponent("cli.msg.dim.info." + playerLangKeyPart + ".teams.empty", dimCache.getDimensionKey().location()));
        }
        teamList.append(new StringTextComponent("\n"));
        teamNames.forEach(teamName -> {
            IFormattableTextComponent removeTeamLink = new StringTextComponent(" - ")
                    .append(buildDimRemoveTeamLink(teamName, dimCache.getDimensionKey(), memberOrOwner))
                    .append(new StringTextComponent(" '" + teamName + "'\n"));
            teamList.append(removeTeamLink);
        });
        return teamList;
    }

    private static Set<String> getAssociateList(DimensionalRegion dimRegion, CommandConstants memberOrOwner, CommandConstants playerOrTeam){
        Set<String> associateNames = new HashSet<>();
        switch (memberOrOwner) {
            case OWNER:
                switch (playerOrTeam){
                    case PLAYER:
                        associateNames = new HashSet<>(dimRegion.getOwners().getPlayers().values());
                        break;
                    case TEAM:
                        associateNames = new HashSet<>(dimRegion.getOwners().getTeams());
                        break;
                }
                break;
            case MEMBER:
                switch (playerOrTeam){
                    case PLAYER:
                        associateNames = new HashSet<>(dimRegion.getMembers().getPlayers().values());
                        break;
                    case TEAM:
                        associateNames = new HashSet<>(dimRegion.getMembers().getTeams());
                        break;
                }
                break;
            default:
                break;
        }
        return associateNames;
    }

    public static IFormattableTextComponent buildPlayerList(DimensionalRegion dimCache, CommandConstants memberOrOwner) {
        Set<String> playerNames = getAssociateList(dimCache, memberOrOwner, PLAYER);
        IFormattableTextComponent playerList = new StringTextComponent("Players: ");
        String playerLangKeyPart = memberOrOwner == CommandConstants.OWNER ? "owner" : "member";
        if (playerNames.isEmpty()) {
            return playerList.append(new TranslationTextComponent("cli.msg.dim.info." + playerLangKeyPart + ".players.empty", dimCache.getDimensionKey().location()));
        }
        playerList.append(new StringTextComponent("\n"));
        playerNames.forEach(playerName -> {
            IFormattableTextComponent removePlayerLink = new StringTextComponent(" - ")
                    .append(buildDimRemovePlayerLink(playerName, dimCache.getDimensionKey(), memberOrOwner))
                    .append(new StringTextComponent(" '" + playerName + "'\n"));
            playerList.append(removePlayerLink);
        });
        return playerList;
    }
}
