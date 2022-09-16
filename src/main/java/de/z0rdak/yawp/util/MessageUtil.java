package de.z0rdak.yawp.util;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.commands.CommandConstants;
import de.z0rdak.yawp.config.server.CommandPermissionConfig;
import de.z0rdak.yawp.core.affiliation.PlayerContainer;
import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.area.IMarkableArea;
import de.z0rdak.yawp.core.area.SphereArea;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.region.*;
import net.minecraft.command.CommandSource;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.scoreboard.ScorePlayerTeam;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.text.*;
import net.minecraft.util.text.event.ClickEvent;
import net.minecraft.util.text.event.HoverEvent;
import net.minecraft.world.World;
import org.apache.commons.lang3.NotImplementedException;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.config.server.RegionConfig.REGION_DEFAULT_PRIORITY_INC;
import static de.z0rdak.yawp.util.CommandUtil.*;
import static net.minecraft.util.text.TextFormatting.*;
import static net.minecraft.util.text.event.ClickEvent.Action.*;

public class MessageUtil {

    private MessageUtil() {
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
            YetAnotherWorldProtector.LOGGER.error(e);
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
            YetAnotherWorldProtector.LOGGER.error(e);
        }
    }

    public static void sendMessage(PlayerEntity player, ITextComponent textComponent) {
        player.sendMessage(textComponent, player.getUUID());
    }

    public static void sendMessage(PlayerEntity player, String translationKey) {
        player.sendMessage(new TranslationTextComponent(translationKey), player.getUUID());
    }

    public static void sendStatusMessage(PlayerEntity player, TranslationTextComponent text) {
        player.displayClientMessage(text, true);
    }

    public static void sendStatusMessage(PlayerEntity player, String langKey) {
        player.displayClientMessage(new TranslationTextComponent(langKey), true);
    }

    public static IFormattableTextComponent buildExecuteCmdComponent(String linkText, String hoverText, String command, ClickEvent.Action eventAction, TextFormatting color){
        return TextComponentUtils.wrapInSquareBrackets(new TranslationTextComponent(linkText))
                .setStyle(Style.EMPTY.withColor(Color.fromLegacyFormat(color))
                        .withClickEvent(new ClickEvent(eventAction, command))
                        .withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, new TranslationTextComponent(hoverText))));
    }

    public static IFormattableTextComponent buildExecuteCmdComponent(IFormattableTextComponent linkText, IFormattableTextComponent hoverText, String command, ClickEvent.Action eventAction, TextFormatting color){
        return TextComponentUtils.wrapInSquareBrackets(linkText)
                .setStyle(Style.EMPTY.withColor(Color.fromLegacyFormat(color))
                        .withClickEvent(new ClickEvent(eventAction, command))
                        .withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, hoverText)));
    }

    public static IFormattableTextComponent buildDimensionTeleportLink(IMarkableRegion region) {
        String cmdLinkText = buildTeleportLinkText(region.getDim(), region.getTpTarget());
        String executeCmdStr = buildDimTeleportCmd(region.getDim(), "@s", region.getTpTarget());
        TranslationTextComponent teleportCmdHoverText = new TranslationTextComponent("cli.msg.info.region.spatial.location.teleport", region.getName(), region.getDim().location().toString());
        return buildExecuteCmdComponent(new StringTextComponent(cmdLinkText), teleportCmdHoverText, executeCmdStr, RUN_COMMAND, AQUA);
    }


    public static String buildTeleportCmd(String tpSource, BlockPos target){
        return "tp " + tpSource + " " + target.getX() + " " + target.getY() + " " + target.getZ();
    }

    public static String buildTeleportLinkText(RegistryKey<World> dim, BlockPos target){
        return buildTeleportLinkText(dim.location().toString(), target);
    }

    public static String buildTeleportLinkText(String regionName, BlockPos target){
        return regionName + " @ [" + buildBlockPosTeleportLinkText(target) + "]";
    }

    public static String buildBlockPosTeleportLinkText(BlockPos target){
        return target.getX() + ", " + target.getY() + ", " + target.getZ();
    }


    public static String buildExecuteCommandString(RegistryKey<World> dim, String command){
        return "/execute in " + dim.location() + " run " + command;
    }

    public static String buildDimTeleportCmd(RegistryKey<World> dim, String tpSource, BlockPos target){
        return buildExecuteCommandString(dim, buildTeleportCmd(tpSource, target));
    }

    public static String buildRegionTpCmd(IMarkableRegion region, String target){
        return buildDimTeleportCmd(region.getDim(), target, region.getTpTarget());
    }

    public static IFormattableTextComponent buildHelpSuggestionLink(String translationKey, CommandConstants baseCmd, CommandConstants cmd) {
        String command = "/" + CommandPermissionConfig.BASE_CMD + " " + baseCmd + " " + cmd + " ";
        return new StringTextComponent(" ")
                .append(buildExecuteCmdComponent("=>", "chat.link.hover.command.copy", command, SUGGEST_COMMAND, GREEN))
                .append(new StringTextComponent(" "))
                .append(new TranslationTextComponent(translationKey));
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

    public static String buildRegionCmdStr(IMarkableRegion region, CommandConstants constant){
        return CommandUtil.buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), constant.toString());
    }

    public static IFormattableTextComponent buildDimAddPlayerLink(DimensionalRegion dimRegion, String hoverTextLangKey, CommandConstants memberOrOwner) {
        String command = buildDimAddPlayerCmdStr(dimRegion.getName(), memberOrOwner);
        String linkText = "+";
        return buildExecuteCmdComponent(linkText, hoverTextLangKey, command, SUGGEST_COMMAND, GREEN);
    }

    public static IFormattableTextComponent buildDimAddTeamLink(DimensionalRegion dimRegion, String hoverTextLangKey, CommandConstants memberOrOwner) {
        return buildExecuteCmdComponent("+", hoverTextLangKey, buildDimAddTeamCmdStr(dimRegion.getName(), memberOrOwner),
                SUGGEST_COMMAND, GREEN);
    }

    public static IFormattableTextComponent buildRegionEnableComponent(IMarkableRegion region){
        String cmd = CommandUtil.buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), ENABLE.toString(), String.valueOf(!region.isActive()));
        YetAnotherWorldProtector.LOGGER.warn(cmd);
        String linkTextKey = "cli.msg.info.region.state.enable." + region.isActive() + ".link.text";
        String hoverTextKey = "cli.msg.info.region.state.enable." + !region.isActive() + ".link.hover";
        TextFormatting color = region.isActive() ? TextFormatting.GREEN : TextFormatting.RED;
        return buildExecuteCmdComponent(linkTextKey, hoverTextKey, cmd, ClickEvent.Action.RUN_COMMAND, color);
    }

    public static IFormattableTextComponent buildRegionPriorityComponent(IMarkableRegion region) {
        String incPriorityCmd = CommandUtil.buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), PRIORITY.toString(), INC.toString(), String.valueOf( REGION_DEFAULT_PRIORITY_INC.get()));
        IFormattableTextComponent incLinkText = new TranslationTextComponent("cli.msg.info.region.state.priority.increase.link.text", REGION_DEFAULT_PRIORITY_INC.get());
        IFormattableTextComponent incHoverText = new TranslationTextComponent("cli.msg.info.region.state.priority.increase.link.hover", REGION_DEFAULT_PRIORITY_INC.get());
        IFormattableTextComponent increaseLink = buildExecuteCmdComponent(incLinkText, incHoverText, incPriorityCmd, ClickEvent.Action.RUN_COMMAND, GREEN);
        String decPriorityCmd = CommandUtil.buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), PRIORITY.toString(), DEC.toString(), String.valueOf( REGION_DEFAULT_PRIORITY_INC.get()));
        IFormattableTextComponent decLinkText = new TranslationTextComponent("cli.msg.info.region.state.priority.decrease.link.text", REGION_DEFAULT_PRIORITY_INC.get());
        IFormattableTextComponent decHoverText = new TranslationTextComponent("cli.msg.info.region.state.priority.decrease.link.hover", REGION_DEFAULT_PRIORITY_INC.get());
        IFormattableTextComponent decreaseLink = buildExecuteCmdComponent(decLinkText, decHoverText, decPriorityCmd, ClickEvent.Action.RUN_COMMAND, RED);
        StringTextComponent priorityValue = new StringTextComponent(String.valueOf(region.getPriority()));
        String setPriorityCmd = CommandUtil.buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), PRIORITY.toString(), "");
        TranslationTextComponent setPriorityLinkText = new TranslationTextComponent("cli.msg.info.region.state.priority.set.link.text");
        TranslationTextComponent setPriorityHoverText = new TranslationTextComponent("cli.msg.info.region.state.priority.set.link.hover");
        IFormattableTextComponent setPriorityLink = buildExecuteCmdComponent(setPriorityLinkText, setPriorityHoverText, setPriorityCmd, SUGGEST_COMMAND, GREEN);
        return priorityValue.append(" ")
                .append(setPriorityLink).append(" ")
                .append(increaseLink).append(" ")
                .append(decreaseLink);
    }

    public static IFormattableTextComponent buildRegionAlertComponentLink(IMarkableRegion region){
        String cmd = CommandUtil.buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), ALERT.toString(), String.valueOf(!region.isMuted()));
        YetAnotherWorldProtector.LOGGER.warn(cmd);
        String linkTextKey = "cli.msg.info.region.state.alert." + region.isMuted() + ".link.text";
        String hoverTextKey = "cli.msg.info.region.state.alert." + !region.isMuted() + ".link.hover";
        TextFormatting color = region.isMuted() ? TextFormatting.GREEN : TextFormatting.RED;
        return buildExecuteCmdComponent(linkTextKey, hoverTextKey, cmd, ClickEvent.Action.RUN_COMMAND, color);
    }

    public static IFormattableTextComponent buildDimensionalInfoLink(RegistryKey<World> dim) {
        String command = "/" + CommandPermissionConfig.BASE_CMD + " " + DIMENSION + " " + dim.location() + " " + INFO;
        String hoverText = "cli.msg.dim.info";
        return buildExecuteCmdComponent(dim.location().toString(), hoverText, command, RUN_COMMAND, GREEN);
    }

    // TODO: How to make info links more generic for AbstractRegion
    public static IFormattableTextComponent buildRegionInfoLink(RegistryKey<World> dim, String regionName){
        String cmd = buildCommandStr(REGION.toString(), dim.location().toString(), regionName, INFO.toString());
        StringTextComponent regionInfoLinkText = new StringTextComponent(regionName);
        TranslationTextComponent regionInfoLinkHover = new TranslationTextComponent("cli.msg.info.region", regionName);
        return buildExecuteCmdComponent(regionInfoLinkText, regionInfoLinkHover, cmd, RUN_COMMAND, GREEN);
    }

    public static IFormattableTextComponent buildRegionInfoLink(IMarkableRegion region) {
        return buildRegionInfoLink(region.getDim(), region.getName());
    }

    public static IFormattableTextComponent buildRegionSpatialPropLink(IMarkableRegion region) {
        String showSpatialPropLink = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), SPATIAL.toString());
        IFormattableTextComponent spatialPropLinkText = new TranslationTextComponent(  "cli.msg.info.region.spatial.link.text");
        IFormattableTextComponent spatialPropHoverText = new TranslationTextComponent(  "cli.msg.info.region.spatial.link.hover", region.getName());
        return buildExecuteCmdComponent(spatialPropLinkText, spatialPropHoverText, showSpatialPropLink, RUN_COMMAND, AQUA);
    }

    public static IFormattableTextComponent buildRegionSpatialHeader(IMarkableRegion region){
        return new StringTextComponent(TextFormatting.BOLD + "")
                .append(new TranslationTextComponent("cli.msg.info.region.spatial.header", buildRegionInfoLink(region)))
                .append(new StringTextComponent(TextFormatting.BOLD + ""));
    }

    public static IFormattableTextComponent buildRegionLocationComponent(IMarkableRegion region){
        return composeInfoComponent(  "cli.msg.info.region.spatial.location", buildDimensionTeleportLink(region));
    }

    public static IFormattableTextComponent buildRegionAreaComponent(IMarkableRegion region){
        IFormattableTextComponent areaInfo = buildRegionAreaDetailComponent(region);
        return composeInfoComponent(  "cli.msg.info.region.spatial.area", areaInfo);
    }

    public static IFormattableTextComponent buildBlockPosTpLinks(IMarkableRegion region) {
        List<IFormattableTextComponent> tpLinks = region.getArea().getMarkedBlocks()
                .stream()
                .map(pos -> buildDimensionalBlockTpLink(region.getDim(), pos))
                .collect(Collectors.toList());
        IFormattableTextComponent blockPosTpLinkList = new StringTextComponent("");
        tpLinks.forEach(tpLink -> blockPosTpLinkList.append(tpLink).append(" "));
        return blockPosTpLinkList;
    }

    public static IFormattableTextComponent buildRegionAreaDetailComponent(IMarkableRegion region){
        IMarkableArea area = region.getArea();
        IFormattableTextComponent areaInfo = new StringTextComponent(area.getAreaType().areaType)
                .append("\n");
        switch (area.getAreaType()) {
            case CUBOID: {
                CuboidArea cuboidArea = (CuboidArea) area;
                IFormattableTextComponent sizeInfo = new TranslationTextComponent("Size: X=" + cuboidArea.getArea().getXsize() + ", Y=" + cuboidArea.getArea().getYsize() + ", Z=" + cuboidArea.getArea().getZsize());
                IFormattableTextComponent markedBlocksInfo = new TranslationTextComponent("Marked blocks: ").append(buildBlockPosTpLinks(region));
                areaInfo.append(sizeInfo).append("\n")
                        .append(markedBlocksInfo);
                return areaInfo;
            }
            case CYLINDER:
                CylinderRegion cylinderRegion;
                return new StringTextComponent("CylinderRegion info here");
            case SPHERE:
                {
                SphereArea sphereArea = (SphereArea) area;
                IFormattableTextComponent sizeInfo = new TranslationTextComponent("Size: Center")
                        .append(buildDimensionalBlockTpLink(region.getDim(), new BlockPos(sphereArea.getCenter()))).append(", Radius=" + sphereArea.getRadius());
                IFormattableTextComponent markedBlocksInfo = new TranslationTextComponent("Marked blocks: ").append(buildBlockPosTpLinks(region));
                areaInfo.append(sizeInfo).append("\n")
                        .append(markedBlocksInfo);
                return areaInfo;
                }
            case POLYGON_3D:
                PolygonRegion polygonRegion;
                return new StringTextComponent("PolygonRegion info here");
            case PRISM:
                PrismRegion prismRegion;
                return new StringTextComponent("PrismRegion info here");
            default:
                throw new NotImplementedException("");
        }
    }

    public static IFormattableTextComponent buildRegionAffiliationLink(IMarkableRegion region) {
        IFormattableTextComponent affiliationLinks = new StringTextComponent("");
        List<String> affiliations = Arrays.asList("owner","member");
        affiliations.forEach( affiliation -> {
            String cmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), affiliation);
            int affiliationSize;
            switch (affiliation) {
                case "owner":
                    affiliationSize = region.getOwners().getPlayers().size() + region.getOwners().getTeams().size();
                    break;
                case "member":
                    affiliationSize = region.getMembers().getPlayers().size() + region.getMembers().getTeams().size();
                    break;
                default:
                    affiliationSize = -1;
                    break;
            }
            IFormattableTextComponent linkText = new TranslationTextComponent("cli.msg.info.region.affiliation.list.link.text", affiliationSize, affiliation);
            IFormattableTextComponent hoverText = new TranslationTextComponent("cli.msg.info.region.affiliation.list.link.hover", affiliation, region.getName());
            IFormattableTextComponent affiliationLink = buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, AQUA);
            affiliationLinks.append(affiliationLink).append(" ");
        });
        return affiliationLinks;
    }

    private static IFormattableTextComponent buildRegionAddPlayerLink(IMarkableRegion region, IFormattableTextComponent hoverText, String affiliation) {
        String command = buildRegionCmdStr(region, ADD) + " " + PLAYER + " " + affiliation + " ";
        IFormattableTextComponent linkText = new TranslationTextComponent("cli.msg.info.region.affiliation.player.add.link.text");
        return buildExecuteCmdComponent(linkText, hoverText, command, SUGGEST_COMMAND, GREEN);
    }

    private static IFormattableTextComponent buildRegionAddTeamLink(IMarkableRegion region, IFormattableTextComponent hoverText, String affiliation) {
        String command = buildRegionCmdStr(region, ADD) + " " + TEAM + " " + affiliation + " ";
        IFormattableTextComponent linkText = new TranslationTextComponent("cli.msg.info.region.affiliation.team.add.link.text");
        return buildExecuteCmdComponent(linkText, hoverText, command, SUGGEST_COMMAND, GREEN);
    }

    public static IFormattableTextComponent buildRegionAffiliationHeader(IMarkableRegion region, String affiliation) {
        return new StringTextComponent(TextFormatting.BOLD + "")
                .append(new TranslationTextComponent("cli.msg.info.region.affiliation.header", buildRegionInfoLink(region), affiliation))
                .append(new StringTextComponent(TextFormatting.BOLD + ""));
    }

    public static IFormattableTextComponent buildRegionAffiliationTeamListLink(IMarkableRegion region, String affiliation, PlayerContainer playerContainer) {
        // Teams: [n team(s)] [+]
        IFormattableTextComponent addTeamLinkHoverText = new TranslationTextComponent("cli.msg.info.region.affiliation.team.add.link.hover", affiliation, region.getName());
        IFormattableTextComponent teamAddLink = buildRegionAddTeamLink(region, addTeamLinkHoverText, affiliation);
        IFormattableTextComponent teamListLink = playerContainer.hasTeams()
                ? buildRegionTeamListLink(region, playerContainer, affiliation)
                : new TranslationTextComponent("cli.msg.info.region.affiliation.team.list.link.text", playerContainer.getTeams().size());
        IFormattableTextComponent teams = new TranslationTextComponent("cli.msg.info.region.affiliation.team").append(": ");
        teams.append(teamListLink).append(teamAddLink);
        return teams;
    }

    public static IFormattableTextComponent buildRegionAffiliationPlayerListLink(IMarkableRegion region, String affiliation, PlayerContainer playerContainer) {
        // Players: [n player(s)] [+]
        IFormattableTextComponent addPlayerLinkHoverText = new TranslationTextComponent("cli.msg.info.region.affiliation.player.add.link.hover", affiliation, region.getName());
        IFormattableTextComponent playersAddLink = buildRegionAddPlayerLink(region, addPlayerLinkHoverText, affiliation);
        IFormattableTextComponent playerListLink = playerContainer.hasPlayers()
                ? buildRegionPlayerListLink(region, playerContainer, affiliation)
                : new TranslationTextComponent("cli.msg.info.region.affiliation.player.list.link.text", playerContainer.getPlayers().size());
        IFormattableTextComponent players = new TranslationTextComponent("cli.msg.info.region.affiliation.player").append(": ");
        players.append(playerListLink).append(playersAddLink);
        return players;
    }

    public static IFormattableTextComponent buildRegionHierarchyLink(IMarkableRegion region) {
     return buildRegionParentLink(region).append(", ").append(buildRegionChildrenLink(region));
    }


    public static IFormattableTextComponent buildRegionChildrenHeader(IMarkableRegion region) {
        return new StringTextComponent(TextFormatting.BOLD + "")
                .append(new TranslationTextComponent("cli.msg.info.region.children.header", buildRegionInfoLink(region)))
                .append(new StringTextComponent(TextFormatting.BOLD + ""));
    }

    public static IFormattableTextComponent buildRegionChildrenComponent(IMarkableRegion region) {

        return new StringTextComponent("Children go here");
    }

    public static IFormattableTextComponent buildRegionParentLink(IMarkableRegion region){
        IFormattableTextComponent parentLink;
        if (region.getParent() != null) {
            String regionParentInfoCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getParent().getName(), INFO.toString());
            IFormattableTextComponent parentLinkText = new TranslationTextComponent( "cli.msg.info.region.parent.link.text", region.getName());
            IFormattableTextComponent parentHoverText = new TranslationTextComponent( "cli.msg.info.region.parent.link.hover", region.getName());
            String clearRegionParentCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), PARENT.toString(), CLEAR.toString(), region.getParent().getName());
            IFormattableTextComponent parentClearLinkText = new TranslationTextComponent( "cli.msg.info.region.parent.clear.link.text");
            IFormattableTextComponent parentClearHoverText = new TranslationTextComponent( "cli.msg.info.region.parent.clear.link.hover", region.getParent().getName());

            parentLink = buildExecuteCmdComponent(parentLinkText, parentHoverText, regionParentInfoCmd, RUN_COMMAND, AQUA)
                    .append(buildExecuteCmdComponent(parentClearLinkText, parentClearHoverText, clearRegionParentCmd, SUGGEST_COMMAND, RED));
        } else {
            String setRegionParentCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), PARENT.toString(), SET.toString(), "");
            IFormattableTextComponent setParentLinkText = new TranslationTextComponent( "cli.msg.info.region.parent.set.link.text");
            IFormattableTextComponent setParentHoverText = new TranslationTextComponent( "cli.msg.info.region.parent.set.link.hover", region.getName());
            parentLink = new TranslationTextComponent("cli.msg.info.region.parent.null")
                    .append(" ")
                    .append(buildExecuteCmdComponent(setParentLinkText, setParentHoverText, setRegionParentCmd, SUGGEST_COMMAND, GREEN));
        }
        return parentLink;
    }

    public static IFormattableTextComponent buildRegionChildrenLink(IMarkableRegion region) {
        String regionChildrenListLink = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), CHILDREN.toString());
        IFormattableTextComponent childrenLinkText = new TranslationTextComponent("cli.msg.info.region.children.link.text", region.getChildren().size());
        IFormattableTextComponent childrenHoverText = new TranslationTextComponent("cli.msg.info.region.children.link.text", region.getName());
        IFormattableTextComponent regionChildrenLink = region.getChildren().size() == 0 ? childrenLinkText :
        buildExecuteCmdComponent(childrenLinkText, childrenHoverText, regionChildrenListLink, RUN_COMMAND, AQUA);
        String addChildrenCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), ADD.toString(), CHILD.toString(), "");
        IFormattableTextComponent addChildrenLinkText = new TranslationTextComponent("cli.msg.info.region.children.add.link.text");
        IFormattableTextComponent addChildrenHoverText = new TranslationTextComponent("cli.msg.info.region.children.add.link.hover", region.getName());
        IFormattableTextComponent addChildrenLink = buildExecuteCmdComponent(addChildrenLinkText, addChildrenHoverText, addChildrenCmd, SUGGEST_COMMAND, GREEN);
        return regionChildrenLink.append(addChildrenLink);
    }

    public static IFormattableTextComponent buildFlagListLink(IMarkableRegion region) {
        String listCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), FLAG.toString());
        IFormattableTextComponent flagsLinkText = new TranslationTextComponent("cli.msg.info.region.flag.link.text", region.getFlags().size());
        IFormattableTextComponent flagsHoverText = new TranslationTextComponent("cli.msg.info.region.flag.link.hover", region.getName());
        IFormattableTextComponent flags = buildExecuteCmdComponent(flagsLinkText, flagsHoverText, listCmd, RUN_COMMAND, AQUA);
        String addCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), ADD.toString(), FLAG.toString(), "");
        IFormattableTextComponent addFlagLinkText = new TranslationTextComponent( "cli.msg.info.region.flag.add.link.text");
        IFormattableTextComponent addFlagHoverText = new TranslationTextComponent( "cli.msg.info.region.flag.add.link.hover", region.getName());
        IFormattableTextComponent addFlag = buildExecuteCmdComponent(addFlagLinkText, addFlagHoverText, addCmd, SUGGEST_COMMAND, GREEN);
        return flags.append(addFlag);
    }

    public static IFormattableTextComponent buildRegionStateLink(IMarkableRegion region){
        String showStateCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), STATE.toString());
        IFormattableTextComponent stateLinkText = new TranslationTextComponent(  "cli.msg.info.region.state.link.text");
        IFormattableTextComponent stateHoverText = new TranslationTextComponent(  "cli.msg.info.region.state.link.hover", region.getName());
        return buildExecuteCmdComponent(stateLinkText, stateHoverText, showStateCmd, RUN_COMMAND, AQUA);
    }

    public static IFormattableTextComponent buildRegionStateHeader(IMarkableRegion region){
        return new TranslationTextComponent("cli.msg.info.region.state.header", buildRegionInfoLink(region));
    }

    public static IFormattableTextComponent composeRegionEnableComponent(IMarkableRegion region){
        return composeInfoComponent("cli.msg.info.region.state.enable", buildRegionEnableComponent(region));
    }

    public static IFormattableTextComponent composeRegionAlertComponent(IMarkableRegion region){
        return composeInfoComponent("cli.msg.info.region.state.alert",
                buildRegionAlertComponentLink(region));
    }

    public static IFormattableTextComponent composeRegionPriorityComponent(IMarkableRegion region){
        return composeInfoComponent("cli.msg.info.region.state.priority",
                buildRegionPriorityComponent(region));
    }

    private static IFormattableTextComponent composeInfoComponent(String subjectLangKey, IFormattableTextComponent payload){
        return new TranslationTextComponent(subjectLangKey).append(": ").append(payload);
    }

    public static IFormattableTextComponent buildDimPlayerListLink(DimensionalRegion dimRegion, PlayerContainer players, CommandConstants memberOrOwner) {
        String command = "/" + CommandPermissionConfig.BASE_CMD + " " + DIMENSION + " " + dimRegion.getName() + " " + LIST + " " + memberOrOwner;
        IFormattableTextComponent hoverText = new TranslationTextComponent("cli.msg.info.region.affiliation.player.list.link.hover", memberOrOwner.toString(), dimRegion.getName());
        IFormattableTextComponent linkText = new TranslationTextComponent("cli.msg.info.region.affiliation.player.list.link.text", players.getPlayers().size());
        return buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, AQUA);
    }

    public static IFormattableTextComponent buildDimTeamListLink(DimensionalRegion dimRegion, PlayerContainer teams, CommandConstants memberOrOwner) {
        String command = "/" + CommandPermissionConfig.BASE_CMD + " " + DIMENSION + " " + dimRegion.getName() + " " + LIST + " " + memberOrOwner;
        IFormattableTextComponent hoverText = new TranslationTextComponent("cli.msg.info.region.affiliation.team.list.link.hover", memberOrOwner.toString(), dimRegion.getName());
        IFormattableTextComponent linkText = new TranslationTextComponent("cli.msg.info.region.affiliation.team.list.link.text", teams.getTeams().size());
        return buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, AQUA);
    }

    public static IFormattableTextComponent buildRegionPlayerListLink(IMarkableRegion region, PlayerContainer players, String affiliation) {
        String cmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), affiliation, PLAYER.toString());
        IFormattableTextComponent hoverText = new TranslationTextComponent("cli.msg.info.region.affiliation.player.list.link.hover", affiliation, region.getName());
        IFormattableTextComponent linkText = new TranslationTextComponent("cli.msg.info.region.affiliation.player.list.link.text", players.getPlayers().size());
        return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, AQUA);
    }

    public static IFormattableTextComponent buildRegionTeamListLink(IMarkableRegion region, PlayerContainer teams, String affiliation) {
        String cmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), affiliation, TEAM.toString());
        IFormattableTextComponent hoverText = new TranslationTextComponent("cli.msg.info.region.affiliation.team.list.link.hover", affiliation, region.getName());
        IFormattableTextComponent linkText = new TranslationTextComponent("cli.msg.info.region.affiliation.team.list.link.text", teams.getTeams().size());
        return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, AQUA);
    }


    public static IFormattableTextComponent buildRegionInfoAndTpLink(IMarkableRegion region){
        String teleportCmd = buildDimTeleportCmd(region.getDim(), "@s", region.getTpTarget());
        IFormattableTextComponent teleportLink = buildExecuteCmdComponent(buildBlockPosTeleportLinkText(region.getTpTarget()),
            "cli.msg.region.info.tp.link.hover", teleportCmd, RUN_COMMAND, AQUA);
        IFormattableTextComponent separator = new StringTextComponent(Color.fromLegacyFormat(TextFormatting.RESET) + " @ " + Color.fromLegacyFormat(TextFormatting.RESET));
        return  buildRegionInfoLink(region)
                .append(separator)
                .append(teleportLink);
    }

    public static IFormattableTextComponent buildDimensionalBlockTpLink(RegistryKey<World> dim, BlockPos target) {
        String teleportCmd = buildDimTeleportCmd(dim, "@s", target);
        return buildExecuteCmdComponent(buildBlockPosTeleportLinkText(target),
                "cli.msg.info.region.spatial.location.teleport.link.hover", teleportCmd, RUN_COMMAND, AQUA);
    }

    public static IFormattableTextComponent buildDimSuggestRegionRemovalLink(IMarkableRegion region) {
        String cmd = buildCommandStr(DIMENSION.toString(), region.getDim().location().toString(), DELETE.toString(), region.getName());
        String hoverText = "cli.msg.dim.region.remove.link.hover";
        String linkText = "x";
        return buildExecuteCmdComponent(linkText, hoverText, cmd, SUGGEST_COMMAND, RED);
    }

    public static IFormattableTextComponent buildAddDimFlagLink(DimensionalRegion dimRegion) {
        String command = buildCommandStr(DIMENSION.toString(), dimRegion.getName(), ADD.toString(), FLAG.toString(), "");
        String hoverText = "cli.msg.dim.flag.add.link.hover";
        String linkText = "+";
        return buildExecuteCmdComponent(linkText, hoverText, command, SUGGEST_COMMAND, GREEN);
    }

    public static IFormattableTextComponent buildDimFlagListLink(DimensionalRegion dimRegion) {
        String command = buildCommandStr(DIMENSION.toString(), dimRegion.getName(), LIST.toString(), FLAG.toString());
        String hoverText = "List flags in dimension '" + dimRegion.getName() + "'";
        String linkText = dimRegion.getFlags().size() + " flags(s)";
        return buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, AQUA);
    }

    public static IFormattableTextComponent buildDimRemovePlayerLink(String playerName, RegistryKey<World> dim, CommandConstants memberOrOwner) {
        String command = buildCommandStr(DIMENSION.toString(), dim.location().toString(), REMOVE.toString(), PLAYER.toString(), memberOrOwner.toString(), playerName);
        //String hoverText = "Remove player '" + playerName + "' from dimension " + "'" + dim.location() + "'";
        String hoverText = "Remove player";
        String linkText = "x";
        return buildExecuteCmdComponent(linkText, hoverText, command, SUGGEST_COMMAND, RED);
    }

    public static IFormattableTextComponent buildDimRemoveTeamLink(String teamName, RegistryKey<World> dim, CommandConstants memberOrOwner) {
        String command = buildCommandStr(DIMENSION.toString(), dim.location().toString(), REMOVE.toString(), TEAM.toString(), memberOrOwner.toString(), teamName);
        //String hoverText = "Remove team '" + teamName + "' from dimension " + "'" + dim.location() + "'";
        String hoverText = "Remove team";
        String linkText = "x";
        return buildExecuteCmdComponent(linkText, hoverText, command, SUGGEST_COMMAND, RED);
    }

    public static IFormattableTextComponent buildRegionRemoveTeamLink(IMarkableRegion region, String team, String affiliation) {
        String command = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), REMOVE.toString(), TEAM.toString(), affiliation, team);
        TranslationTextComponent linkText = new TranslationTextComponent("cli.msg.info.region.affiliation.team.remove.link.text");
        TranslationTextComponent linkHoverText = new TranslationTextComponent("cli.msg.info.region.affiliation.team.remove.link.hover", team, region.getName());
        return buildExecuteCmdComponent(linkText, linkHoverText, command, SUGGEST_COMMAND, RED);
    }

    public static IFormattableTextComponent buildRegionRemovePlayerLink(IMarkableRegion region, String player, String affiliation) {
        String command = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), REMOVE.toString(), PLAYER.toString(), affiliation, player);
        TranslationTextComponent linkText = new TranslationTextComponent("cli.msg.info.region.affiliation.player.remove.link.text");
        TranslationTextComponent linkHoverText = new TranslationTextComponent("cli.msg.info.region.affiliation.player.remove.link.hover", player, region.getName());
        return buildExecuteCmdComponent(linkText, linkHoverText, command, SUGGEST_COMMAND, RED);
    }

    public static IFormattableTextComponent buildRegionRemoveChildLink(IMarkableRegion region, IMarkableRegion child) {
        String command = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), REMOVE.toString(), CHILD.toString(), child.getName());
        TranslationTextComponent linkText = new TranslationTextComponent("cli.msg.info.region.children.remove.link.text");
        TranslationTextComponent linkHoverText = new TranslationTextComponent("cli.msg.info.region.affiliation.remove.link.hover", child.getName(), region.getName());
        return buildExecuteCmdComponent(linkText, linkHoverText, command, SUGGEST_COMMAND, RED);
    }

    public static IFormattableTextComponent buildDimensionRemoveFlagLink(IFlag flag, RegistryKey<World> dim) {
        String command =  "/" + CommandPermissionConfig.BASE_CMD + " " + DIMENSION + " " + dim.location() + " " + REMOVE + " " + FLAG + " " + flag.getFlagIdentifier();
        //String hoverText =" Remove flag '" + flag.getFlagName() + "' from dimension " + "'" + dim.location() + "'";
        String hoverText ="Remove flag";
        String linkText = "x";
        return buildExecuteCmdComponent(linkText, hoverText, command, SUGGEST_COMMAND, RED);
    }

    public static IFormattableTextComponent buildTeamList(DimensionalRegion dimCache, CommandConstants memberOrOwner) {
        Set<String> teamNames = getAssociateList(dimCache, memberOrOwner.toString(), TEAM.toString());
        String playerLangKeyPart = memberOrOwner == CommandConstants.OWNER ? "owners" : "members";
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

    public static Set<String> getAssociateList(AbstractRegion region, String affiliation, String playerOrTeam){
        Set<String> associateNames = new HashSet<>();
        switch (affiliation) {
            case "owner":
                switch (playerOrTeam){
                    case "player":
                        associateNames = new HashSet<>(region.getOwners().getPlayers().values());
                        break;
                    case "team":
                        associateNames = new HashSet<>(region.getOwners().getTeams());
                        break;
                }
                break;
            case "member":
                switch (playerOrTeam){
                    case "player":
                        associateNames = new HashSet<>(region.getMembers().getPlayers().values());
                        break;
                    case "team":
                        associateNames = new HashSet<>(region.getMembers().getTeams());
                        break;
                }
                break;
            default:
                break;
        }
        return associateNames;
    }

    public static IFormattableTextComponent buildPlayerList(DimensionalRegion dimCache, CommandConstants memberOrOwner) {
        Set<String> playerNames = getAssociateList(dimCache, memberOrOwner.toString(), PLAYER.toString());
        IFormattableTextComponent playerList = new StringTextComponent("Players: ");
        String playerLangKeyPart = memberOrOwner == CommandConstants.OWNER ? "owners" : "members";
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

    public static IFormattableTextComponent buildRemoveFlagLink(IFlag flag, IMarkableRegion region) {
        String cmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), REMOVE.toString(), FLAG.toString(), flag.getFlagIdentifier());
        // TODO: lang-key
        String linkText = "x";
        String hoverText = "Remove flag '" + flag.getFlagIdentifier() + "'";
        return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, RED);
    }

}
