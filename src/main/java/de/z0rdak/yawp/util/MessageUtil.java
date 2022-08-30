package de.z0rdak.yawp.util;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.commands.CommandConstants;
import de.z0rdak.yawp.config.server.CommandPermissionConfig;
import de.z0rdak.yawp.core.affiliation.PlayerContainer;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.region.AbstractMarkableRegion;
import de.z0rdak.yawp.core.region.AbstractRegion;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import net.minecraft.ChatFormatting;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.*;

import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.minecraftforge.client.event.RenderTooltipEvent;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.util.CommandUtil.*;
import static net.minecraft.ChatFormatting.*;
import static net.minecraft.network.chat.ClickEvent.Action.RUN_COMMAND;
import static net.minecraft.network.chat.ClickEvent.Action.SUGGEST_COMMAND;

public class MessageUtil {

    private MessageUtil() {
    }

    public static MutableComponent buildHelpLink(String translationKey, CommandConstants cmd) {
        String command =  "/" + CommandPermissionConfig.BASE_CMD + " " + cmd.toString() + " " + HELP;
        return new TextComponent(" ")
                .append(buildExecuteCmdComponent("=>", command, GREEN, "Show detailed help for the " + cmd.toString() + " commands",  RUN_COMMAND))
                .append(new TextComponent(" "))
                .append(new TranslatableComponent(translationKey));
    }

    public static MutableComponent buildHelpHeader(String translationKey){
        return new TextComponent(BOLD + " == ")
                .append(new TranslatableComponent(translationKey).setStyle(Style.EMPTY.withBold(true)))
                .append(new TextComponent(BOLD + " == "));
    }

    public static MutableComponent buildHelpHeader(TranslatableComponent TranslatableComponent){
        return new TextComponent(BOLD + " == ")
                .append(TranslatableComponent)
                .append(new TextComponent(BOLD + " == "));
    }

    public static void sendCmdFeedback(CommandSourceStack src, MutableComponent text) {
        try {
            if (src.getEntity() == null) {
                src.sendSuccess(text, true);
            } else {
                sendMessage(src.getPlayerOrException(), text);
            }
        } catch (CommandSyntaxException e) {
            YetAnotherWorldProtector.LOGGER.error(e);
        }
    }

    public static void sendCmdFeedback(CommandSourceStack src, String langKey) {
        try {
            TranslatableComponent text = new TranslatableComponent(langKey);
            if (src.getEntity() == null) {
                src.sendSuccess(text, true);
            } else {
                MessageUtil.sendMessage(src.getPlayerOrException(), text);
            }
        } catch (CommandSyntaxException e) {
            YetAnotherWorldProtector.LOGGER.error(e);
        }
    }

    public static void sendMessage(Player player, MutableComponent textComponent) {
        player.sendMessage(textComponent, player.getUUID());
    }

    public static void sendMessage(Player player, String translationKey) {
        player.sendMessage(new TranslatableComponent(translationKey), player.getUUID());
    }

    public static void sendStatusMessage(Player player, TranslatableComponent text) {
        player.displayClientMessage(text, true);
    }

    public static void sendStatusMessage(Player player, String langKey) {
        player.displayClientMessage(new TranslatableComponent(langKey), true);
    }

    private static String format(double value) {
        return String.format("%.2f", value);
    }

    // TODO: overload for TranslatableComponents
    public static MutableComponent buildExecuteCmdComponent(String linkText, String command, ChatFormatting color, String hoverText, ClickEvent.Action eventAction){
        return ComponentUtils.wrapInSquareBrackets(new TranslatableComponent(linkText))
                .setStyle(Style.EMPTY.withColor(color)
                        .withClickEvent(new ClickEvent(eventAction, command))
                        .withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, new TranslatableComponent(hoverText))));
    }

    public static MutableComponent buildDimensionTeleportLink(AbstractMarkableRegion region) {
        String cmdLinkText = buildTeleportLinkText(region.getDim(), region.getTpTarget());
        String executeCmdStr = buildDimTeleportCmd(region.getDim(), "@s", region.getTpTarget());
        return buildExecuteCmdComponent(cmdLinkText, executeCmdStr, GREEN, "chat.link.hover.region.tp", RUN_COMMAND);
    }

    public static String buildTeleportCmd(String tpSource, BlockPos target){
        return "tp " + tpSource + " " + target.getX() + " " + target.getY() + " " + target.getZ();
    }

    public static String buildTeleportLinkText(ResourceKey<Level> dim, BlockPos target){
        return dim + "@ [" + target.getX() + ", " + target.getY() + ", " + target.getZ() + "]";
    }

    public static String buildExecuteCommandString(ResourceKey<Level> dim, String command){
        return "/execute in " + dim.location() + " run " + command;
    }

    public static String buildDimTeleportCmd(ResourceKey<Level> dim, String tpSource, BlockPos target){
        return buildExecuteCommandString(dim, buildTeleportCmd(tpSource, target));
    }

    public static MutableComponent buildHelpSuggestionLink(String translationKey, CommandConstants baseCmd, CommandConstants cmd) {
        String command = "/" + CommandPermissionConfig.BASE_CMD + " " + baseCmd + " " + cmd + " ";
        return new TextComponent(" ")
                .append(buildExecuteCmdComponent("=>", command, GREEN, "chat.link.hover.command.copy", SUGGEST_COMMAND))
                .append(new TextComponent(" "))
                .append(new TranslatableComponent(translationKey));
    }

    public static MutableComponent buildDimHelpLink(String translationKey, CommandConstants baseCmd, List<String> cmds) {
        String cmdStr = String.join(" ", cmds);
        String command = "/" + CommandPermissionConfig.BASE_CMD + " " + baseCmd + " " + cmdStr;
        return new TextComponent(" ")
                .append(buildExecuteCmdComponent("=>", command, GREEN, "chat.link.hover.command.copy", SUGGEST_COMMAND))
                .append(new TextComponent(" "))
                .append(new TranslatableComponent(translationKey));
    }

    public static MutableComponent buildInteractiveDimHelpLink(String translationKey, CommandConstants baseCmd, List<String> cmds) {
        String cmdStr = String.join(" ", cmds);
        // TODO
        String command = "/" + CommandPermissionConfig.BASE_CMD + " " + baseCmd + " " + cmdStr;
        return new TextComponent(" ")
                .append(buildExecuteCmdComponent("=>", command, GREEN, "chat.link.hover.command.copy", SUGGEST_COMMAND))
                .append(new TextComponent(" "))
                .append(new TranslatableComponent(translationKey));
    }

    public static String buildDimAddPlayerCmdStr(String region, CommandConstants memberOrOwner){
        return buildDimAddCmdStr(region) + " " + PLAYER + " " + memberOrOwner + " ";
    }

    public static String buildDimAddTeamCmdStr(String region, CommandConstants memberOrOwner){
        return buildDimAddCmdStr(region) + " " + TEAM + " " + memberOrOwner + " ";
    }

    public static String buildDimAddCmdStr(String region){
        return "/" + CommandPermissionConfig.BASE_CMD + " " + DIMENSION + " " + region + " " + ADD;
    }

    public static MutableComponent buildDimAddPlayerLink(DimensionalRegion dimRegion, String hoverTextLangKey, CommandConstants memberOrOwner) {
        String command = buildDimAddPlayerCmdStr(dimRegion.getName(), memberOrOwner);
        //String hoverText = hoverTextLangKey + " '" + dimRegion.getName() + "'";
        String hoverText = hoverTextLangKey;
        String linkText = "+";
        return buildExecuteCmdComponent(linkText, command, GREEN, hoverText, SUGGEST_COMMAND);
    }


    // TODO: generalize with Supplier<String> (regionName) -> build....(regionName)
    public static MutableComponent buildDimAddTeamLink(DimensionalRegion dimRegion, String hoverTextLangKey, CommandConstants memberOrOwner) {
        return buildExecuteCmdComponent("+", buildDimAddTeamCmdStr(dimRegion.getName(), memberOrOwner),
                GREEN, hoverTextLangKey, SUGGEST_COMMAND);
    }

    public static MutableComponent buildDimStateActiveLink(AbstractRegion region){
        boolean isActive = region.isActive();
        MutableComponent activeText = isActive
                ? new TranslatableComponent("message.region.info.active.true")
                : new TranslatableComponent("message.region.info.active.false");
        ChatFormatting color = isActive
                ? GREEN
                : RED;
        String onClickAction = isActive ? "deactivate" : "activate";
        String command = "/" + CommandPermissionConfig.BASE_CMD + " " + REGION + " " + onClickAction + " " + region.getName();
        // TODO: translatable overload (linkText) ?
        return buildExecuteCmdComponent(activeText.getString(), command, color, onClickAction + " " + REGION + " '" + region.getName() + "'", RUN_COMMAND);
    }

    public static MutableComponent buildDimensionalInfoLink(ResourceKey<Level> dim) {
        String command = "/" + CommandPermissionConfig.BASE_CMD + " " + DIMENSION + " " + dim.location() + " " + INFO;
        String hoverText = "cli.msg.dim.info";
        return buildExecuteCmdComponent(dim.location().toString(), command, GREEN, hoverText, RUN_COMMAND);
    }

    public static MutableComponent buildPlayerListLink(DimensionalRegion dimRegion, PlayerContainer players, CommandConstants memberOrOwner) {
        String command = "/" + CommandPermissionConfig.BASE_CMD + " " + DIMENSION + " " + dimRegion.getName() + " " + LIST + " " + memberOrOwner;
        String hoverText = "List players(" + memberOrOwner.toString() + ") in dimension '" + dimRegion.getName() + "'";
        String linkText = players.getPlayers().size() + " player(s)";
        return buildExecuteCmdComponent(linkText, command, AQUA, hoverText, RUN_COMMAND);
    }

    public static MutableComponent buildTeamListLink(DimensionalRegion dimRegion, PlayerContainer teams, CommandConstants memberOrOwner) {
        String command = "/" + CommandPermissionConfig.BASE_CMD + " " + DIMENSION + " " + dimRegion.getName() + " " + LIST + " " + memberOrOwner;
        String hoverText = "List teams(" + memberOrOwner.toString() + ") in dimension '" + dimRegion.getName() + "'";
        String linkText = teams.getTeams().size() + " teams(s)";
        return buildExecuteCmdComponent(linkText, command, AQUA, hoverText, RUN_COMMAND);
    }

    public static MutableComponent buildDimensionRegionInfoLink(ResourceKey<Level> dim, IMarkableRegion region){
        BlockPos target = region.getTpTarget();
        String regionInfoCommand = buildCommandStr(REGION.toString(), INFO.toString(), region.getName(),dim.location().toString());
        MutableComponent regionInfoLink = buildExecuteCmdComponent(region.getName(), regionInfoCommand,
                GREEN, "cli.msg.region.info.link.hover", RUN_COMMAND);
        MutableComponent teleportLink = buildExecuteCmdComponent(buildTeleportLinkText(dim, target),
                buildDimTeleportCmd(dim, "@s", target),
                GREEN, "cli.msg.region.info.tp.link.hover" , RUN_COMMAND);
        return  regionInfoLink
                .append(new TextComponent(": ").setStyle(Style.EMPTY.withColor(ChatFormatting.RESET)))
                .append(teleportLink);
    }

    public static MutableComponent buildDimSuggestRegionRemovalLink(ResourceKey<Level> dim, String regionName) {
        String command = "/" + CommandPermissionConfig.BASE_CMD + " " + REGION + " " + dim.location() + " " + REMOVE + " " + regionName;
        String hoverText = "cli.msg.dim.region.remove.link.hover";
        String linkText = "x";
        return buildExecuteCmdComponent(linkText, command, RED,hoverText, SUGGEST_COMMAND);

    }

    public static MutableComponent buildAddDimFlagLink(DimensionalRegion dimRegion) {
        String command = buildCommandStr(DIMENSION.toString(), dimRegion.getName(), ADD.toString(), FLAG.toString());
        String hoverText = "cli.msg.dim.flag.add.link.hover";
        String linkText = "+";
        return buildExecuteCmdComponent(linkText, command, GREEN, hoverText, SUGGEST_COMMAND);
    }

    public static MutableComponent buildDimFlagListLink(DimensionalRegion dimRegion) {
        String command = buildCommandStr(DIMENSION.toString(), dimRegion.getName(), LIST.toString(), FLAG.toString());
        String hoverText = "List flags in dimension '" + dimRegion.getName() + "'";
        String linkText = dimRegion.getFlags().size() + " flags(s)";
        return buildExecuteCmdComponent(linkText, command, AQUA, hoverText, RUN_COMMAND);
    }

    public static MutableComponent buildDimRemovePlayerLink(String playerName, ResourceKey<Level> dim, CommandConstants memberOrOwner) {
        String command = buildCommandStr(DIMENSION.toString(), dim.location().toString(), REMOVE.toString(), PLAYER.toString(), memberOrOwner.toString(), playerName);
        //String hoverText = "Remove player '" + playerName + "' from dimension " + "'" + dim.location() + "'";
        String hoverText = "Remove player";
        String linkText = "x";
        return buildExecuteCmdComponent(linkText, command, RED, hoverText, SUGGEST_COMMAND);
    }

    public static MutableComponent buildDimRemoveTeamLink(String teamName, ResourceKey<Level> dim, CommandConstants memberOrOwner) {
        String command = buildCommandStr(DIMENSION.toString(), dim.location().toString(), REMOVE.toString(), TEAM.toString(), memberOrOwner.toString(), teamName);
        //String hoverText = "Remove team '" + teamName + "' from dimension " + "'" + dim.location() + "'";
        String hoverText = "Remove team";
        String linkText = "x";
        return buildExecuteCmdComponent(linkText, command, RED, hoverText, SUGGEST_COMMAND);
    }

    public static MutableComponent buildDimensionRemoveFlagLink(IFlag flag, ResourceKey<Level> dim) {
        String command =  "/" + CommandPermissionConfig.BASE_CMD + " " + DIMENSION + " " + dim.location() + " " + REMOVE + " " + FLAG + " " + flag.getFlagName();
        //String hoverText =" Remove flag '" + flag.getFlagName() + "' from dimension " + "'" + dim.location() + "'";
        String hoverText ="Remove flag";
        String linkText = "x";
        return buildExecuteCmdComponent(linkText, command, RED, hoverText, SUGGEST_COMMAND);
    }

    public static MutableComponent buildTeamList(DimensionalRegion dimCache, CommandConstants memberOrOwner) {
        Set<String> teamNames = getAssociateList(dimCache, memberOrOwner, TEAM);
        String playerLangKeyPart = memberOrOwner == CommandConstants.OWNER ? "owners" : "members";
        MutableComponent teamList = new TextComponent("Teams: ");
        if (teamNames.isEmpty()) {
            teamList.append(new TranslatableComponent("cli.msg.dim.info." + playerLangKeyPart + ".teams.empty", dimCache.getDimensionKey().location()));
        }
        teamList.append(new TextComponent("\n"));
        teamNames.forEach(teamName -> {
            MutableComponent removeTeamLink = new TextComponent(" - ")
                    .append(buildDimRemoveTeamLink(teamName, dimCache.getDimensionKey(), memberOrOwner))
                    .append(new TextComponent(" '" + teamName + "'\n"));
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

    public static MutableComponent buildPlayerList(DimensionalRegion dimCache, CommandConstants memberOrOwner) {
        Set<String> playerNames = getAssociateList(dimCache, memberOrOwner, PLAYER);
        MutableComponent playerList = new TextComponent("Players: ");
        String playerLangKeyPart = memberOrOwner == CommandConstants.OWNER ? "owners" : "members";
        if (playerNames.isEmpty()) {
            return playerList.append(new TranslatableComponent("cli.msg.dim.info." + playerLangKeyPart + ".players.empty", dimCache.getDimensionKey().location()));
        }
        playerList.append(new TextComponent("\n"));
        playerNames.forEach(playerName -> {
            MutableComponent removePlayerLink = new TextComponent(" - ")
                    .append(buildDimRemovePlayerLink(playerName, dimCache.getDimensionKey(), memberOrOwner))
                    .append(new TextComponent(" '" + playerName + "'\n"));
            playerList.append(removePlayerLink);
        });
        return playerList;
    }

}
