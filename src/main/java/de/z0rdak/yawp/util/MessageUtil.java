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
import net.minecraft.ChatFormatting;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.*;
import net.minecraft.network.chat.contents.TranslatableContents;
import net.minecraft.resources.ResourceKey;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import org.apache.commons.lang3.NotImplementedException;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.config.server.RegionConfig.REGION_DEFAULT_PRIORITY_INC;
import static de.z0rdak.yawp.util.CommandUtil.*;
import static net.minecraft.ChatFormatting.*;
import static net.minecraft.network.chat.ClickEvent.Action.*;


public class MessageUtil {

    private MessageUtil() {
    }

    public static MutableComponent buildHelpHeader(String translationKey){
        return Component.literal(BOLD + " == ")
                .append(Component.translatable(translationKey)).setStyle(Style.EMPTY.withBold(true))
                .append(Component.literal(BOLD + " == "));
    }

    public static MutableComponent buildHelpHeader(TranslatableContents translationTextComponent){
        return Component.literal(BOLD + " == ")
                .append(MutableComponent.create(translationTextComponent))
                .append(Component.literal(BOLD + " == "));
    }

    public static void sendCmdFeedback(CommandSourceStack src, Component text) {
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
            MutableComponent text = Component.translatable(langKey);
            if (src.getEntity() == null) {
                src.sendSuccess(text, true);
            } else {
                MessageUtil.sendMessage(src.getPlayerOrException(), text);
            }
        } catch (CommandSyntaxException e) {
            YetAnotherWorldProtector.LOGGER.error(e);
        }
    }

    public static void sendMessage(Player player, Component textComponent) {
        player.sendSystemMessage(textComponent);
    }

    public static void sendMessage(Player player, String translationKey) {
        player.sendSystemMessage(Component.translatable(translationKey));
    }

    public static void sendStatusMessage(Player player, MutableComponent text) {
        player.displayClientMessage(text, true);
    }

    public static void sendStatusMessage(Player player, String langKey) {
        player.displayClientMessage(Component.translatable(langKey), true);
    }

    public static MutableComponent buildExecuteCmdComponent(String linkText, String hoverText, String command, ClickEvent.Action eventAction, ChatFormatting color){
        return ComponentUtils.wrapInSquareBrackets(Component.translatable(linkText))
                .setStyle(Style.EMPTY.withColor(color)
                        .withClickEvent(new ClickEvent(eventAction, command))
                        .withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, Component.translatable(hoverText))));
    }

    public static MutableComponent buildExecuteCmdComponent(MutableComponent linkText, MutableComponent hoverText, String command, ClickEvent.Action eventAction, ChatFormatting color){
        return ComponentUtils.wrapInSquareBrackets(linkText)
                .setStyle(Style.EMPTY.withColor(color)
                        .withClickEvent(new ClickEvent(eventAction, command))
                        .withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, hoverText)));
    }

    public static MutableComponent buildDimensionTeleportLink(IMarkableRegion region) {
        String cmdLinkText = buildTeleportLinkText(region.getDim(), region.getTpTarget());
        String executeCmdStr = buildDimTeleportCmd(region.getDim(), "@s", region.getTpTarget());
        MutableComponent teleportCmdHoverText = Component.translatable("cli.msg.info.region.spatial.location.teleport", region.getName(), region.getDim().location().toString());
        return buildExecuteCmdComponent(Component.literal(cmdLinkText), teleportCmdHoverText, executeCmdStr, RUN_COMMAND, AQUA);
    }


    public static String buildTeleportCmd(String tpSource, BlockPos target){
        return "tp " + tpSource + " " + target.getX() + " " + target.getY() + " " + target.getZ();
    }

    public static String buildTeleportLinkText(ResourceKey<Level> dim, BlockPos target){
        return buildTeleportLinkText(dim.location().toString(), target);
    }

    public static String buildTeleportLinkText(String regionName, BlockPos target){
        return regionName + " @ [" + buildBlockPosTeleportLinkText(target) + "]";
    }

    public static String buildBlockPosTeleportLinkText(BlockPos target){
        return target.getX() + ", " + target.getY() + ", " + target.getZ();
    }


    public static String buildExecuteCommandString(ResourceKey<Level> dim, String command){
        return "/execute in " + dim.location() + " run " + command;
    }

    public static String buildDimTeleportCmd(ResourceKey<Level> dim, String tpSource, BlockPos target){
        return buildExecuteCommandString(dim, buildTeleportCmd(tpSource, target));
    }

    public static String buildRegionTpCmd(IMarkableRegion region, String target){
        return buildDimTeleportCmd(region.getDim(), target, region.getTpTarget());
    }

    public static MutableComponent buildHelpSuggestionLink(String translationKey, CommandConstants baseCmd, CommandConstants cmd) {
        String command = "/" + CommandPermissionConfig.BASE_CMD + " " + baseCmd + " " + cmd + " ";
        return Component.literal(" ")
                .append(buildExecuteCmdComponent("=>", "chat.link.hover.command.copy", command, SUGGEST_COMMAND, GREEN))
                .append(Component.literal(" "))
                .append(Component.translatable(translationKey));
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

    public static MutableComponent buildDimAddPlayerLink(DimensionalRegion dimRegion, String hoverTextLangKey, CommandConstants memberOrOwner) {
        String command = buildDimAddPlayerCmdStr(dimRegion.getName(), memberOrOwner);
        String linkText = "+";
        return buildExecuteCmdComponent(linkText, hoverTextLangKey, command, SUGGEST_COMMAND, GREEN);
    }

    public static MutableComponent buildDimAddTeamLink(DimensionalRegion dimRegion, String hoverTextLangKey, CommandConstants memberOrOwner) {
        return buildExecuteCmdComponent("+", hoverTextLangKey, buildDimAddTeamCmdStr(dimRegion.getName(), memberOrOwner),
                SUGGEST_COMMAND, GREEN);
    }

    public static MutableComponent buildRegionEnableComponent(IMarkableRegion region){
        String cmd = CommandUtil.buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), ENABLE.toString(), String.valueOf(!region.isActive()));
        YetAnotherWorldProtector.LOGGER.warn(cmd);
        String linkTextKey = "cli.msg.info.region.state.enable." + region.isActive() + ".link.text";
        String hoverTextKey = "cli.msg.info.region.state.enable." + !region.isActive() + ".link.hover";
        ChatFormatting color = region.isActive() ? ChatFormatting.GREEN : ChatFormatting.RED;
        return buildExecuteCmdComponent(linkTextKey, hoverTextKey, cmd, ClickEvent.Action.RUN_COMMAND, color);
    }

    public static MutableComponent buildRegionPriorityComponent(IMarkableRegion region) {
        String incPriorityCmd = CommandUtil.buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), STATE.toString(), PRIORITY.toString(), INC.toString(), String.valueOf( REGION_DEFAULT_PRIORITY_INC.get()));
        MutableComponent incLinkText = Component.translatable("cli.msg.info.region.state.priority.increase.link.text", REGION_DEFAULT_PRIORITY_INC.get());
        MutableComponent incHoverText = Component.translatable("cli.msg.info.region.state.priority.increase.link.hover", REGION_DEFAULT_PRIORITY_INC.get());
        MutableComponent increaseLink = buildExecuteCmdComponent(incLinkText, incHoverText, incPriorityCmd, ClickEvent.Action.RUN_COMMAND, GREEN);
        String decPriorityCmd = CommandUtil.buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), STATE.toString(), PRIORITY.toString(), DEC.toString(), String.valueOf( REGION_DEFAULT_PRIORITY_INC.get()));
        MutableComponent decLinkText = Component.translatable("cli.msg.info.region.state.priority.decrease.link.text", REGION_DEFAULT_PRIORITY_INC.get());
        MutableComponent decHoverText = Component.translatable("cli.msg.info.region.state.priority.decrease.link.hover", REGION_DEFAULT_PRIORITY_INC.get());
        MutableComponent decreaseLink = buildExecuteCmdComponent(decLinkText, decHoverText, decPriorityCmd, ClickEvent.Action.RUN_COMMAND, RED);
        MutableComponent priorityValue = Component.literal(String.valueOf(region.getPriority()));
        String setPriorityCmd = CommandUtil.buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), STATE.toString(), PRIORITY.toString(), "");
        MutableComponent setPriorityLinkText = Component.translatable("cli.msg.info.region.state.priority.set.link.text");
        MutableComponent setPriorityHoverText = Component.translatable("cli.msg.info.region.state.priority.set.link.hover");
        MutableComponent setPriorityLink = buildExecuteCmdComponent(setPriorityLinkText, setPriorityHoverText, setPriorityCmd, SUGGEST_COMMAND, GREEN);
        return priorityValue.append(" ")
                .append(setPriorityLink).append(" ")
                .append(increaseLink).append(" ")
                .append(decreaseLink);
    }

    public static MutableComponent buildRegionAlertComponentLink(IMarkableRegion region){
        String cmd = CommandUtil.buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), ALERT.toString(), String.valueOf(!region.isMuted()));
        YetAnotherWorldProtector.LOGGER.warn(cmd);
        String linkTextKey = "cli.msg.info.region.state.alert." + region.isMuted() + ".link.text";
        String hoverTextKey = "cli.msg.info.region.state.alert." + !region.isMuted() + ".link.hover";
        ChatFormatting color = region.isMuted() ? ChatFormatting.GREEN : ChatFormatting.RED;
        return buildExecuteCmdComponent(linkTextKey, hoverTextKey, cmd, ClickEvent.Action.RUN_COMMAND, color);
    }

    public static MutableComponent buildDimensionalInfoLink(ResourceKey<Level> dim) {
        String command = "/" + CommandPermissionConfig.BASE_CMD + " " + DIMENSION + " " + dim.location() + " " + INFO;
        String hoverText = "cli.msg.dim.info";
        return buildExecuteCmdComponent(dim.location().toString(), hoverText, command, RUN_COMMAND, GREEN);
    }

    // TODO: How to make info links more generic for AbstractRegion
    public static MutableComponent buildRegionInfoLink(ResourceKey<Level> dim, String regionName){
        String cmd = buildCommandStr(REGION.toString(), dim.location().toString(), regionName, INFO.toString());
        MutableComponent regionInfoLinkText = Component.literal(regionName);
        MutableComponent regionInfoLinkHover = Component.translatable("cli.msg.info.region", regionName);
        return buildExecuteCmdComponent(regionInfoLinkText, regionInfoLinkHover, cmd, RUN_COMMAND, GREEN);
    }

    public static MutableComponent buildRegionInfoLink(IMarkableRegion region) {
        return buildRegionInfoLink(region.getDim(), region.getName());
    }

    public static MutableComponent buildRegionSpatialPropLink(IMarkableRegion region) {
        String showSpatialPropLink = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), SPATIAL.toString());
        MutableComponent spatialPropLinkText = Component.translatable(  "cli.msg.info.region.spatial.link.text");
        MutableComponent spatialPropHoverText = Component.translatable(  "cli.msg.info.region.spatial.link.hover", region.getName());
        return buildExecuteCmdComponent(spatialPropLinkText, spatialPropHoverText, showSpatialPropLink, RUN_COMMAND, AQUA);
    }

    public static MutableComponent buildRegionSpatialHeader(IMarkableRegion region){
        return Component.literal(ChatFormatting.BOLD + "")
                .append(Component.translatable("cli.msg.info.region.spatial.header", buildRegionInfoLink(region)))
                .append(Component.literal(ChatFormatting.BOLD + ""));
    }

    public static MutableComponent buildRegionLocationComponent(IMarkableRegion region){
        return composeInfoComponent(  "cli.msg.info.region.spatial.location", buildDimensionTeleportLink(region));
    }

    public static MutableComponent buildRegionAreaComponent(IMarkableRegion region){
        MutableComponent areaInfo = buildRegionAreaDetailComponent(region);
        return composeInfoComponent(  "cli.msg.info.region.spatial.area", areaInfo);
    }

    public static MutableComponent buildBlockPosTpLinks(IMarkableRegion region) {
        List<MutableComponent> tpLinks = region.getArea().getMarkedBlocks()
                .stream()
                .map(pos -> buildDimensionalBlockTpLink(region.getDim(), pos))
                .collect(Collectors.toList());
        MutableComponent blockPosTpLinkList = Component.literal("");
        tpLinks.forEach(tpLink -> blockPosTpLinkList.append(tpLink).append(" "));
        return blockPosTpLinkList;
    }

    public static MutableComponent buildRegionAreaDetailComponent(IMarkableRegion region){
        IMarkableArea area = region.getArea();
        MutableComponent areaInfo = Component.literal(area.getAreaType().areaType)
                .append("\n");
        switch (area.getAreaType()) {
            case CUBOID: {
                CuboidArea cuboidArea = (CuboidArea) area;
                MutableComponent sizeInfo = Component.translatable("Size: X=" + cuboidArea.getArea().getXsize() + ", Y=" + cuboidArea.getArea().getYsize() + ", Z=" + cuboidArea.getArea().getZsize());
                MutableComponent markedBlocksInfo = Component.translatable("Marked blocks: ").append(buildBlockPosTpLinks(region));
                areaInfo.append(sizeInfo).append("\n")
                        .append(markedBlocksInfo);
                return areaInfo;
            }
            case CYLINDER:
                CylinderRegion cylinderRegion;
                return Component.literal("CylinderRegion info here");
            case SPHERE:
            {
                SphereArea sphereArea = (SphereArea) area;
                MutableComponent sizeInfo = Component.translatable("Size: Center")
                        .append(buildDimensionalBlockTpLink(region.getDim(), new BlockPos(sphereArea.getCenter().x, sphereArea.getCenter().y, sphereArea.getCenter().z))).append(", Radius=" + sphereArea.getRadius());
                MutableComponent markedBlocksInfo = Component.translatable("Marked blocks: ").append(buildBlockPosTpLinks(region));
                areaInfo.append(sizeInfo).append("\n")
                        .append(markedBlocksInfo);
                return areaInfo;
            }
            case POLYGON_3D:
                PolygonRegion polygonRegion;
                return Component.literal("PolygonRegion info here");
            case PRISM:
                PrismRegion prismRegion;
                return Component.literal("PrismRegion info here");
            default:
                throw new NotImplementedException("");
        }
    }

    public static MutableComponent buildRegionAffiliationLink(IMarkableRegion region) {
        MutableComponent affiliationLinks = Component.literal("");
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
            MutableComponent linkText = Component.translatable("cli.msg.info.region.affiliation.list.link.text", affiliationSize, affiliation);
            MutableComponent hoverText = Component.translatable("cli.msg.info.region.affiliation.list.link.hover", affiliation, region.getName());
            MutableComponent affiliationLink = buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, AQUA);
            affiliationLinks.append(affiliationLink).append(" ");
        });
        return affiliationLinks;
    }

    private static MutableComponent buildRegionAddPlayerLink(IMarkableRegion region, MutableComponent hoverText, String affiliation) {
        String command = buildRegionCmdStr(region, ADD) + " " + PLAYER + " " + affiliation + " ";
        MutableComponent linkText = Component.translatable("cli.msg.info.region.affiliation.player.add.link.text");
        return buildExecuteCmdComponent(linkText, hoverText, command, SUGGEST_COMMAND, GREEN);
    }

    private static MutableComponent buildRegionAddTeamLink(IMarkableRegion region, MutableComponent hoverText, String affiliation) {
        String command = buildRegionCmdStr(region, ADD) + " " + TEAM + " " + affiliation + " ";
        MutableComponent linkText = Component.translatable("cli.msg.info.region.affiliation.team.add.link.text");
        return buildExecuteCmdComponent(linkText, hoverText, command, SUGGEST_COMMAND, GREEN);
    }

    public static MutableComponent buildRegionAffiliationHeader(IMarkableRegion region, String affiliation) {
        return Component.literal(ChatFormatting.BOLD + "")
                .append(Component.translatable("cli.msg.info.region.affiliation.header", buildRegionInfoLink(region), affiliation))
                .append(Component.literal(ChatFormatting.BOLD + ""));
    }

    public static MutableComponent buildRegionAffiliationTeamListLink(IMarkableRegion region, String affiliation, PlayerContainer playerContainer) {
        // Teams: [n team(s)] [+]
        MutableComponent addTeamLinkHoverText = Component.translatable("cli.msg.info.region.affiliation.team.add.link.hover", affiliation, region.getName());
        MutableComponent teamAddLink = buildRegionAddTeamLink(region, addTeamLinkHoverText, affiliation);
        MutableComponent teamListLink = playerContainer.hasTeams()
                ? buildRegionTeamListLink(region, playerContainer, affiliation)
                : Component.translatable("cli.msg.info.region.affiliation.team.list.link.text", playerContainer.getTeams().size());
        MutableComponent teams = Component.translatable("cli.msg.info.region.affiliation.team").append(": ");
        teams.append(teamListLink).append(teamAddLink);
        return teams;
    }

    public static MutableComponent buildRegionAffiliationPlayerListLink(IMarkableRegion region, String affiliation, PlayerContainer playerContainer) {
        // Players: [n player(s)] [+]
        MutableComponent addPlayerLinkHoverText = Component.translatable("cli.msg.info.region.affiliation.player.add.link.hover", affiliation, region.getName());
        MutableComponent playersAddLink = buildRegionAddPlayerLink(region, addPlayerLinkHoverText, affiliation);
        MutableComponent playerListLink = playerContainer.hasPlayers()
                ? buildRegionPlayerListLink(region, playerContainer, affiliation)
                : Component.translatable("cli.msg.info.region.affiliation.player.list.link.text", playerContainer.getPlayers().size());
        MutableComponent players = Component.translatable("cli.msg.info.region.affiliation.player").append(": ");
        players.append(playerListLink).append(playersAddLink);
        return players;
    }

    public static MutableComponent buildRegionHierarchyLink(IMarkableRegion region) {
        return buildRegionParentLink(region).append(", ").append(buildRegionChildrenLink(region));
    }


    public static MutableComponent buildRegionChildrenHeader(IMarkableRegion region) {
        return Component.literal(ChatFormatting.BOLD + "")
                .append(Component.translatable("cli.msg.info.region.children.header", buildRegionInfoLink(region)))
                .append(Component.literal(ChatFormatting.BOLD + ""));
    }

    public static MutableComponent buildRegionChildrenComponent(IMarkableRegion region) {

        return Component.literal("Children go here");
    }

    public static MutableComponent buildRegionParentLink(IMarkableRegion region){
        MutableComponent parentLink;
        if (region.getParent() != null) {
            String regionParentInfoCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getParent().getName(), INFO.toString());
            MutableComponent parentLinkText = Component.translatable( "cli.msg.info.region.parent.link.text", region.getName());
            MutableComponent parentHoverText = Component.translatable( "cli.msg.info.region.parent.link.hover", region.getName());
            String clearRegionParentCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), PARENT.toString(), CLEAR.toString(), region.getParent().getName());
            MutableComponent parentClearLinkText = Component.translatable( "cli.msg.info.region.parent.clear.link.text");
            MutableComponent parentClearHoverText = Component.translatable( "cli.msg.info.region.parent.clear.link.hover", region.getParent().getName());

            parentLink = buildExecuteCmdComponent(parentLinkText, parentHoverText, regionParentInfoCmd, RUN_COMMAND, AQUA)
                    .append(buildExecuteCmdComponent(parentClearLinkText, parentClearHoverText, clearRegionParentCmd, SUGGEST_COMMAND, RED));
        } else {
            String setRegionParentCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), PARENT.toString(), SET.toString(), "");
            MutableComponent setParentLinkText = Component.translatable( "cli.msg.info.region.parent.set.link.text");
            MutableComponent setParentHoverText = Component.translatable( "cli.msg.info.region.parent.set.link.hover", region.getName());
            parentLink = Component.translatable("cli.msg.info.region.parent.null")
                    .append(" ")
                    .append(buildExecuteCmdComponent(setParentLinkText, setParentHoverText, setRegionParentCmd, SUGGEST_COMMAND, GREEN));
        }
        return parentLink;
    }

    public static MutableComponent buildRegionChildrenLink(IMarkableRegion region) {
        String regionChildrenListLink = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), CHILDREN.toString());
        MutableComponent childrenLinkText = Component.translatable("cli.msg.info.region.children.link.text", region.getChildren().size());
        MutableComponent childrenHoverText = Component.translatable("cli.msg.info.region.children.link.text", region.getName());
        MutableComponent regionChildrenLink = region.getChildren().size() == 0 ? childrenLinkText :
                buildExecuteCmdComponent(childrenLinkText, childrenHoverText, regionChildrenListLink, RUN_COMMAND, AQUA);
        String addChildrenCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), ADD.toString(), CHILD.toString(), "");
        MutableComponent addChildrenLinkText = Component.translatable("cli.msg.info.region.children.add.link.text");
        MutableComponent addChildrenHoverText = Component.translatable("cli.msg.info.region.children.add.link.hover", region.getName());
        MutableComponent addChildrenLink = buildExecuteCmdComponent(addChildrenLinkText, addChildrenHoverText, addChildrenCmd, SUGGEST_COMMAND, GREEN);
        return regionChildrenLink.append(addChildrenLink);
    }

    public static MutableComponent buildFlagListLink(IMarkableRegion region) {
        String listCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), FLAG.toString());
        MutableComponent flagsLinkText = Component.translatable("cli.msg.info.region.flag.link.text", region.getFlags().size());
        MutableComponent flagsHoverText = Component.translatable("cli.msg.info.region.flag.link.hover", region.getName());
        MutableComponent flags = buildExecuteCmdComponent(flagsLinkText, flagsHoverText, listCmd, RUN_COMMAND, AQUA);
        String addCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), ADD.toString(), FLAG.toString(), "");
        MutableComponent addFlagLinkText = Component.translatable( "cli.msg.info.region.flag.add.link.text");
        MutableComponent addFlagHoverText = Component.translatable( "cli.msg.info.region.flag.add.link.hover", region.getName());
        MutableComponent addFlag = buildExecuteCmdComponent(addFlagLinkText, addFlagHoverText, addCmd, SUGGEST_COMMAND, GREEN);
        return flags.append(addFlag);
    }

    public static MutableComponent buildRegionStateLink(IMarkableRegion region){
        String showStateCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), STATE.toString());
        MutableComponent stateLinkText = Component.translatable(  "cli.msg.info.region.state.link.text");
        MutableComponent stateHoverText = Component.translatable(  "cli.msg.info.region.state.link.hover", region.getName());
        return buildExecuteCmdComponent(stateLinkText, stateHoverText, showStateCmd, RUN_COMMAND, AQUA);
    }

    public static MutableComponent buildRegionStateHeader(IMarkableRegion region){
        return Component.translatable("cli.msg.info.region.state.header", buildRegionInfoLink(region));
    }

    public static MutableComponent composeRegionEnableComponent(IMarkableRegion region){
        return composeInfoComponent("cli.msg.info.region.state.enable", buildRegionEnableComponent(region));
    }

    public static MutableComponent composeRegionAlertComponent(IMarkableRegion region){
        return composeInfoComponent("cli.msg.info.region.state.alert",
                buildRegionAlertComponentLink(region));
    }

    public static MutableComponent composeRegionPriorityComponent(IMarkableRegion region){
        return composeInfoComponent("cli.msg.info.region.state.priority",
                buildRegionPriorityComponent(region));
    }

    private static MutableComponent composeInfoComponent(String subjectLangKey, MutableComponent payload){
        return Component.translatable(subjectLangKey).append(": ").append(payload);
    }

    public static MutableComponent buildDimPlayerListLink(DimensionalRegion dimRegion, PlayerContainer players, CommandConstants memberOrOwner) {
        String command = "/" + CommandPermissionConfig.BASE_CMD + " " + DIMENSION + " " + dimRegion.getName() + " " + LIST + " " + memberOrOwner;
        MutableComponent hoverText = Component.translatable("cli.msg.info.region.affiliation.player.list.link.hover", memberOrOwner.toString(), dimRegion.getName());
        MutableComponent linkText = Component.translatable("cli.msg.info.region.affiliation.player.list.link.text", players.getPlayers().size());
        return buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, AQUA);
    }

    public static MutableComponent buildDimTeamListLink(DimensionalRegion dimRegion, PlayerContainer teams, CommandConstants memberOrOwner) {
        String command = "/" + CommandPermissionConfig.BASE_CMD + " " + DIMENSION + " " + dimRegion.getName() + " " + LIST + " " + memberOrOwner;
        MutableComponent hoverText = Component.translatable("cli.msg.info.region.affiliation.team.list.link.hover", memberOrOwner.toString(), dimRegion.getName());
        MutableComponent linkText = Component.translatable("cli.msg.info.region.affiliation.team.list.link.text", teams.getTeams().size());
        return buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, AQUA);
    }

    public static MutableComponent buildRegionPlayerListLink(IMarkableRegion region, PlayerContainer players, String affiliation) {
        String cmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), affiliation, PLAYER.toString());
        MutableComponent hoverText = Component.translatable("cli.msg.info.region.affiliation.player.list.link.hover", affiliation, region.getName());
        MutableComponent linkText = Component.translatable("cli.msg.info.region.affiliation.player.list.link.text", players.getPlayers().size());
        return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, AQUA);
    }

    public static MutableComponent buildRegionTeamListLink(IMarkableRegion region, PlayerContainer teams, String affiliation) {
        String cmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), affiliation, TEAM.toString());
        MutableComponent hoverText = Component.translatable("cli.msg.info.region.affiliation.team.list.link.hover", affiliation, region.getName());
        MutableComponent linkText = Component.translatable("cli.msg.info.region.affiliation.team.list.link.text", teams.getTeams().size());
        return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, AQUA);
    }


    public static MutableComponent buildRegionInfoAndTpLink(IMarkableRegion region){
        String teleportCmd = buildDimTeleportCmd(region.getDim(), "@s", region.getTpTarget());
        MutableComponent teleportLink = buildExecuteCmdComponent(buildBlockPosTeleportLinkText(region.getTpTarget()),
                "cli.msg.region.info.tp.link.hover", teleportCmd, RUN_COMMAND, AQUA);
        MutableComponent separator = Component.literal(ChatFormatting.RESET + " @ " + ChatFormatting.RESET);
        return  buildRegionInfoLink(region)
                .append(separator)
                .append(teleportLink);
    }

    public static MutableComponent buildDimensionalBlockTpLink(ResourceKey<Level> dim, BlockPos target) {
        String teleportCmd = buildDimTeleportCmd(dim, "@s", target);
        return buildExecuteCmdComponent(buildBlockPosTeleportLinkText(target),
                "cli.msg.info.region.spatial.location.teleport.link.hover", teleportCmd, RUN_COMMAND, AQUA);
    }

    public static MutableComponent buildDimSuggestRegionRemovalLink(IMarkableRegion region) {
        String cmd = buildCommandStr(DIMENSION.toString(), region.getDim().location().toString(), DELETE.toString(), region.getName());
        String hoverText = "cli.msg.dim.region.remove.link.hover";
        String linkText = "x";
        return buildExecuteCmdComponent(linkText, hoverText, cmd, SUGGEST_COMMAND, RED);
    }

    public static MutableComponent buildAddDimFlagLink(DimensionalRegion dimRegion) {
        String command = buildCommandStr(DIMENSION.toString(), dimRegion.getName(), ADD.toString(), FLAG.toString(), "");
        String hoverText = "cli.msg.dim.flag.add.link.hover";
        String linkText = "+";
        return buildExecuteCmdComponent(linkText, hoverText, command, SUGGEST_COMMAND, GREEN);
    }

    public static MutableComponent buildDimFlagListLink(DimensionalRegion dimRegion) {
        String command = buildCommandStr(DIMENSION.toString(), dimRegion.getName(), LIST.toString(), FLAG.toString());
        String hoverText = "List flags in dimension '" + dimRegion.getName() + "'";
        String linkText = dimRegion.getFlags().size() + " flags(s)";
        return buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, AQUA);
    }

    public static MutableComponent buildDimRemovePlayerLink(String playerName, ResourceKey<Level> dim, CommandConstants memberOrOwner) {
        String command = buildCommandStr(DIMENSION.toString(), dim.location().toString(), REMOVE.toString(), PLAYER.toString(), memberOrOwner.toString(), playerName);
        //String hoverText = "Remove player '" + playerName + "' from dimension " + "'" + dim.location() + "'";
        String hoverText = "Remove player";
        String linkText = "x";
        return buildExecuteCmdComponent(linkText, hoverText, command, SUGGEST_COMMAND, RED);
    }

    public static MutableComponent buildDimRemoveTeamLink(String teamName, ResourceKey<Level> dim, CommandConstants memberOrOwner) {
        String command = buildCommandStr(DIMENSION.toString(), dim.location().toString(), REMOVE.toString(), TEAM.toString(), memberOrOwner.toString(), teamName);
        //String hoverText = "Remove team '" + teamName + "' from dimension " + "'" + dim.location() + "'";
        String hoverText = "Remove team";
        String linkText = "x";
        return buildExecuteCmdComponent(linkText, hoverText, command, SUGGEST_COMMAND, RED);
    }

    public static MutableComponent buildRegionRemoveTeamLink(IMarkableRegion region, String team, String affiliation) {
        String command = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), REMOVE.toString(), TEAM.toString(), affiliation, team);
        MutableComponent linkText = Component.translatable("cli.msg.info.region.affiliation.team.remove.link.text");
        MutableComponent linkHoverText = Component.translatable("cli.msg.info.region.affiliation.team.remove.link.hover", team, region.getName());
        return buildExecuteCmdComponent(linkText, linkHoverText, command, SUGGEST_COMMAND, RED);
    }

    public static MutableComponent buildRegionRemovePlayerLink(IMarkableRegion region, String player, String affiliation) {
        String command = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), REMOVE.toString(), PLAYER.toString(), affiliation, player);
        MutableComponent linkText = Component.translatable("cli.msg.info.region.affiliation.player.remove.link.text");
        MutableComponent linkHoverText = Component.translatable("cli.msg.info.region.affiliation.player.remove.link.hover", player, region.getName());
        return buildExecuteCmdComponent(linkText, linkHoverText, command, SUGGEST_COMMAND, RED);
    }

    public static MutableComponent buildRegionRemoveChildLink(IMarkableRegion region, IMarkableRegion child) {
        String command = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), REMOVE.toString(), CHILD.toString(), child.getName());
        MutableComponent linkText = Component.translatable("cli.msg.info.region.children.remove.link.text");
        MutableComponent linkHoverText = Component.translatable("cli.msg.info.region.children.remove.link.hover", child.getName(), region.getName());
        return buildExecuteCmdComponent(linkText, linkHoverText, command, SUGGEST_COMMAND, RED);
    }

    public static MutableComponent buildDimensionRemoveFlagLink(IFlag flag, ResourceKey<Level> dim) {
        String command =  "/" + CommandPermissionConfig.BASE_CMD + " " + DIMENSION + " " + dim.location() + " " + REMOVE + " " + FLAG + " " + flag.getFlagIdentifier();
        //String hoverText =" Remove flag '" + flag.getFlagName() + "' from dimension " + "'" + dim.location() + "'";
        String hoverText ="Remove flag";
        String linkText = "x";
        return buildExecuteCmdComponent(linkText, hoverText, command, SUGGEST_COMMAND, RED);
    }

    public static MutableComponent buildTeamList(DimensionalRegion dimCache, CommandConstants memberOrOwner) {
        Set<String> teamNames = getAssociateList(dimCache, memberOrOwner.toString(), TEAM.toString());
        String playerLangKeyPart = memberOrOwner == CommandConstants.OWNER ? "owners" : "members";
        MutableComponent teamList = Component.literal("Teams: ");
        if (teamNames.isEmpty()) {
            teamList.append(Component.translatable("cli.msg.dim.info." + playerLangKeyPart + ".teams.empty", dimCache.getDimensionKey().location()));
        }
        teamList.append(Component.literal("\n"));
        teamNames.forEach(teamName -> {
            MutableComponent removeTeamLink = Component.literal(" - ")
                    .append(buildDimRemoveTeamLink(teamName, dimCache.getDimensionKey(), memberOrOwner))
                    .append(Component.literal(" '" + teamName + "'\n"));
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

    public static MutableComponent buildPlayerList(DimensionalRegion dimCache, CommandConstants memberOrOwner) {
        Set<String> playerNames = getAssociateList(dimCache, memberOrOwner.toString(), PLAYER.toString());
        MutableComponent playerList = Component.literal("Players: ");
        String playerLangKeyPart = memberOrOwner == CommandConstants.OWNER ? "owners" : "members";
        if (playerNames.isEmpty()) {
            return playerList.append(Component.translatable("cli.msg.dim.info." + playerLangKeyPart + ".players.empty", dimCache.getDimensionKey().location()));
        }
        playerList.append(Component.literal("\n"));
        playerNames.forEach(playerName -> {
            MutableComponent removePlayerLink = Component.literal(" - ")
                    .append(buildDimRemovePlayerLink(playerName, dimCache.getDimensionKey(), memberOrOwner))
                    .append(Component.literal(" '" + playerName + "'\n"));
            playerList.append(removePlayerLink);
        });
        return playerList;
    }

    public static MutableComponent buildRemoveFlagLink(IFlag flag, IMarkableRegion region) {
        String cmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), REMOVE.toString(), FLAG.toString(), flag.getFlagIdentifier());
        // TODO: lang-key
        String linkText = "x";
        String hoverText = "Remove flag '" + flag.getFlagIdentifier() + "'";
        return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, RED);
    }

}
