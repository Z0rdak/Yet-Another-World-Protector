package de.z0rdak.yawp.util;

import de.z0rdak.yawp.api.commands.CommandConstants;
import de.z0rdak.yawp.api.commands.Commands;
import de.z0rdak.yawp.commands.arguments.ArgumentUtil;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.flag.FlagCorrelation;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.group.GroupType;
import de.z0rdak.yawp.core.group.PlayerContainer;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.data.region.DimensionRegionCache;
import de.z0rdak.yawp.data.region.RegionDataManager;
import de.z0rdak.yawp.util.text.Messages;
import net.minecraft.ChatFormatting;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.ClickEvent;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import org.apache.commons.lang3.NotImplementedException;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.api.commands.CommandConstants.*;
import static de.z0rdak.yawp.api.commands.Commands.*;
import static de.z0rdak.yawp.handler.HandlerUtil.getFlagMapRecursive;
import static de.z0rdak.yawp.util.ChatComponentBuilder.*;
import static de.z0rdak.yawp.util.text.Messages.*;
import static net.minecraft.ChatFormatting.*;
import static net.minecraft.network.chat.ClickEvent.Action.*;

public class ChatLinkBuilder {

    private ChatLinkBuilder() {
    }

    /***
     * [X,Y,Z], ..., [X,Y,Z]
     */
    public static MutableComponent buildAreaMarkedBlocksTpLinks(IMarkableRegion region) {
        List<MutableComponent> tpLinks = region.getArea().markedBlocks().stream().map(pos -> buildDimensionalBlockTpLink(region.getDim(), pos)).collect(Collectors.toList());
        MutableComponent blockPosTpLinkList = Component.literal("");
        tpLinks.forEach(tpLink -> blockPosTpLinkList.append(tpLink).append(" "));
        return blockPosTpLinkList;
    }
    
    public static MutableComponent buildRegionAreaExpandLink(IMarkableRegion region) {
        // [<=expand=>] [<=max=>]
        MutableComponent linkText = Component.translatableWithFallback("cli.msg.info.region.area.area.expand.link.text", "<=expand=>");
        MutableComponent linkHover = Component.translatableWithFallback("cli.msg.info.region.area.area.expand.link.hover", "Expand the area for '%s'", region.getName());
        String expandCmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), AREA.toString(), EXPAND.toString(), region.getArea().getAreaType().areaType);
        switch (region.getArea().getAreaType()) {
            case CUBOID: {
                CuboidArea cuboidArea = (CuboidArea) region.getArea();
                int areaLowerLimit = cuboidArea.getArea().minY();
                int areaUpperLimit = cuboidArea.getArea().maxY();
                // [<=expand=>]
                String expandCmdSuggestion = appendSubCommand(expandCmd, String.valueOf(areaLowerLimit), String.valueOf(areaUpperLimit));
                MutableComponent expandLink = buildExecuteCmdComponent(linkText, linkHover, expandCmdSuggestion, SUGGEST_COMMAND, LINK_COLOR);
                // [<=max=>]
                MutableComponent maxExpandLinkText = Component.translatableWithFallback("cli.msg.info.region.area.area.expand-max.link.text", "<=max=>");
                MutableComponent maxExpandLinkHover = Component.translatableWithFallback("cli.msg.info.region.area.area.expand-max.link.hover", "Expand area to build limit");
                String maxExpandCmd = appendSubCommand(expandCmd, String.valueOf(Constants.MIN_BUILD_LIMIT), String.valueOf(Constants.MAX_BUILD_LIMIT));
                MutableComponent maxExpandLink = buildExecuteCmdComponent(maxExpandLinkText, maxExpandLinkHover, maxExpandCmd, RUN_COMMAND, LINK_COLOR);
                return Messages.substitutable("%s %s", expandLink, maxExpandLink);
            }
            case CYLINDER:
                throw new NotImplementedException("cylinder");
            case SPHERE:
                // [<=expand=>]
                String expandCmdSuggestion = appendSubCommand(expandCmd, String.valueOf(1));
                return buildExecuteCmdComponent(linkText, linkHover, expandCmdSuggestion, SUGGEST_COMMAND, LINK_COLOR);
            case POLYGON_3D:
                throw new NotImplementedException("polygon");
            case PRISM:
                throw new NotImplementedException("prism");
            default:
                throw new IllegalArgumentException("Invalid area type");
        }
    }

    private static MutableComponent buildShowAreaToggleLink(IMarkableRegion region) {
        MutableComponent showAreaLinkText = Component.translatableWithFallback("cli.msg.info.region.area.area.show.link", "Show");
        MutableComponent showAreaLinkHover = Component.translatableWithFallback("cli.msg.info.region.area.area.show.hover", "Toggle visible bounding box of '%s'", region.getName());
        String showAreaCmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), AREA.toString(), "show");
        return buildExecuteCmdComponent(showAreaLinkText, showAreaLinkHover, showAreaCmd, RUN_COMMAND, LINK_COLOR);
    }

    /**
     * [set area]
     */
    public static MutableComponent buildAreaUpdateLink(IMarkableRegion region) {
        MutableComponent setAreaLinkText = Component.translatableWithFallback("cli.msg.info.region.area.area.set.link", "set area");
        MutableComponent setAreaLinkHover = Component.translatableWithFallback("cli.msg.info.region.area.area.set.hover", "Update area of region '%s'", region.getName());
        String blocks = String.join(" ", region.getArea().markedBlocks().stream().map(ChatComponentBuilder::buildBlockCoordinateStr).collect(Collectors.toSet()));
        String setAreaCmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), AREA.toString(), SET.toString(), region.getArea().getAreaType().areaType, blocks);
        return buildExecuteCmdComponent(setAreaLinkText, setAreaLinkHover, setAreaCmd, SUGGEST_COMMAND, LINK_COLOR);
    }
    
    public static MutableComponent buildRegionAreaActionLinks(IMarkableRegion region) {
        // [set area] [set TP] [show area] [<=expand=>] [<=max=>]
        return Messages.substitutable("%s %s %s", buildAreaUpdateLink(region), buildRegionSetTpLink(region), buildRegionAreaExpandLink(region));
        // buildShowAreaToggleLink(region)
    }

    public static MutableComponent buildWikiLink() {
        MutableComponent wikiLinkHover = Component.translatableWithFallback("help.tooltip.wiki.link.hover", "https://github.com/Z0rdak/Yet-Another-Level-Protector/wiki");
        MutableComponent wikiLink = Component.translatableWithFallback("help.tooltip.wiki.link.text", "Open Wiki in default browser");
        return buildExecuteCmdComponent(wikiLink, wikiLinkHover, "https://github.com/Z0rdak/Yet-Another-World-Protector/wiki", OPEN_URL, AQUA);
    }

    public static MutableComponent buildRegionInfoLink(IProtectedRegion region) {
        return buildRegionInfoLink(region, Component.translatableWithFallback("cli.msg.info.region.link.hover", "Show region info for %s", region.getName()));
    }

    public static MutableComponent buildRegionInfoLink(IProtectedRegion region, MutableComponent linkText, MutableComponent hoverText) {
        String cmd = Commands.buildRegionInfoCmd(region);
        return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
    }

    public static MutableComponent buildRegionInfoLink(IProtectedRegion region, MutableComponent hoverText) {
        MutableComponent linkText = Component.literal(region.getName());
        return buildRegionInfoLink(region, linkText, hoverText);
    }

    public static MutableComponent buildRegionAreaLink(IMarkableRegion region) {
        String showSpatialPropLink = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), AREA.toString());
        MutableComponent spatialPropLinkText = Component.translatableWithFallback("cli.msg.info.region.area.link.text", "Area Properties");
        MutableComponent spatialPropHoverText = Component.translatableWithFallback("cli.msg.info.region.area.link.hover", "Show region area properties for %s", region.getName());
        return buildExecuteCmdComponent(spatialPropLinkText, spatialPropHoverText, showSpatialPropLink, RUN_COMMAND, LINK_COLOR);
    }

    public static MutableComponent buildGroupLink(IProtectedRegion region, String group, int groupSize) {
        MutableComponent linkText = Component.translatableWithFallback("cli.msg.info.region.group.list.link.text", "%s %s(s)", groupSize, group);
        MutableComponent hoverText = Component.translatableWithFallback("cli.msg.info.region.group.list.link.hover", "List '%s' for region %s", group, region.getName());
        String cmd = Commands.buildListGroupCommand(region, group);
        return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
    }

    /**
     * Players: [n player(s)] [+]
     */
    public static MutableComponent buildPlayerListLink(IProtectedRegion region, PlayerContainer players, String group) {
        MutableComponent hoverText = Component.translatableWithFallback("cli.msg.info.region.group.player.list.link.hover", "List players of group '%s' in region %s", group, region.getName());
        MutableComponent linkText = Component.translatableWithFallback("cli.msg.info.region.group.player.list.link.text", "%s player(s)", players.getPlayers().size());
        String cmd = Commands.buildListGroupMemberCommand(region, group, GroupType.PLAYER);
        return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
    }

    /**
     * Teams: [n team(s)] [+]
     */
    public static MutableComponent buildTeamListLink(IProtectedRegion region, PlayerContainer teams, String group) {
        MutableComponent hoverText = Component.translatableWithFallback("cli.msg.info.region.group.team.list.link.hover", "List teams of group '%s' in region %s", group, region.getName());
        MutableComponent linkText = Component.translatableWithFallback("cli.msg.info.region.group.team.list.link.text", "%s team(s)", teams.getTeams().size());
        String cmd = Commands.buildListGroupMemberCommand(region, group, GroupType.TEAM);
        return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
    }

    public static MutableComponent buildAddToGroupLink(IProtectedRegion region, String group, GroupType groupType) {
        MutableComponent linkText = Component.translatableWithFallback("cli.link.add", "+");
        String fallback = "Add " + groupType.name + " as '%s' to region %s";
        MutableComponent hoverText = Component.translatableWithFallback("cli.msg.info.region.group." + groupType.name + ".add.link.hover", fallback, group, region.getName());
        String cmd = buildAddGroupMemberCommand(region, groupType, group, ""); // "" for blank (suggestion)
        return buildExecuteCmdComponent(linkText, hoverText, cmd, SUGGEST_COMMAND, LINK_COLOR);
    }

    public static MutableComponent buildGroupTeamListLink(IProtectedRegion region, String group) {
        // Teams: [n team(s)] [+]
        PlayerContainer playerContainer = region.getGroup(group);
        MutableComponent teamAddLink = buildAddToGroupLink(region, group, GroupType.TEAM);
        MutableComponent teamListLink = playerContainer.hasTeams() ? buildTeamListLink(region, playerContainer, group) : Component.translatableWithFallback("cli.msg.info.region.group.team.list.link.text", "%s team(s)", playerContainer.getTeams().size());
        return Messages.substitutable("%s: %s %s", Component.translatableWithFallback("cli.msg.info.region.group.team", "Teams"), teamListLink, teamAddLink);
    }

    public static MutableComponent buildGroupPlayerListLink(IProtectedRegion region, String group) {
        // Players: [n player(s)] [+]
        PlayerContainer playerContainer = region.getGroup(group);
        MutableComponent playersAddLink = buildAddToGroupLink(region, group, GroupType.PLAYER);
        MutableComponent playerListLink = playerContainer.hasPlayers() ? buildPlayerListLink(region, playerContainer, group) : Component.translatableWithFallback("cli.msg.info.region.group.player.list.link.text", "%s player(s)", playerContainer.getPlayers().size());
        return Messages.substitutable("%s: %s %s", Component.translatableWithFallback("cli.msg.info.region.group.player", "Players"), playerListLink, playersAddLink);
    }

    /**
     * Creates a TextComponent for displaying the flag info  <br>
     * Text: [$flag-name] <br>
     * Link: /wp flag dim $dim $flag-name info  <br>
     *
     * @param region involved in creating command link for
     * @param flag   involved in creating command link for
     * @return TextComponent [$flag-name] with a link for the flag info
     */
    public static MutableComponent buildFlagInfoLink(IProtectedRegion region, IFlag flag, ChatFormatting linkColor) {
        MutableComponent text = Component.literal(flag.getName());
        MutableComponent hoverText = Component.translatableWithFallback("cli.flag.info.hover", "Show %s flag info of region '%s'", flag.getName(), region.getName());
        return buildFlagInfoLink(region, flag, text, hoverText, linkColor);
    }

    public static MutableComponent buildFlagInfoLink(IProtectedRegion region, IFlag flag, MutableComponent text, MutableComponent hoverText, ChatFormatting linkColor) {
        String cmd = Commands.buildFlagInfoCmd(region, flag.getName());
        return buildExecuteCmdComponent(text, hoverText, cmd, RUN_COMMAND, linkColor);
    }

    public static MutableComponent buildFlagInfoLink(IProtectedRegion region, IFlag flag) {
        return buildFlagInfoLink(region, flag, LINK_COLOR);
    }

    public static MutableComponent buildFlagStateSuggestionLink(IProtectedRegion region, IFlag flag) {
        MutableComponent hover = Component.translatableWithFallback("cli.flag.state.set.link.hover", "Set flag state for '%s' in '%s'", flag.getName(), region.getName());
        MutableComponent text = Component.translatableWithFallback("cli.flag.state.set.link.text", "s");
        String cmd = buildFlagSuggestStateCmd(region, flag.getName());
        return buildExecuteCmdComponent(text, hover, cmd, SUGGEST_COMMAND, LINK_COLOR);
    }

    public static MutableComponent buildFlagOverrideToggleLink(IProtectedRegion region, IFlag flag, boolean shortLink) {
        String fallback = flag.doesOverride() ? "active" : "inactive";
        String fallbackHover = (flag.doesOverride() ? "Disable" : "Enable") + " flag override for '%s' of '%s'";
        MutableComponent linkText = Component.translatableWithFallback("cli.flag.override.link.text." + flag.doesOverride(), fallback);
        MutableComponent hoverText = Component.translatableWithFallback("cli.flag.override.link.hover." + flag.doesOverride(), fallbackHover, flag.getName(), region.getName());
        if (shortLink) {
            linkText = Component.translatableWithFallback("cli.flag.override.link.Component.toggle", "o");
        }
        ChatFormatting color = flag.doesOverride() ? GREEN : GRAY;
        String cmd = buildFlagOverrideToggleCmd(region, flag.getName());
        return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, color);
    }

    public static MutableComponent buildFlagMessageEditLink(IProtectedRegion region, IFlag flag) {
        MutableComponent hover = Component.translatableWithFallback("cli.flag.msg.Component.set.link.hover", "Change the message shown when the flag '%s' of '%s' is triggered", flag.getName(), region.getName());
        MutableComponent text = Component.translatableWithFallback("cli.flag.msg.Component.set.link.text", "Edit");
        String msg = "\"" + flag.getFlagMsg().msg() + "\"";
        String cmd = buildFlagMsgSetCmd(region, flag.getName(), msg);
        return buildExecuteCmdComponent(text, hover, cmd, SUGGEST_COMMAND, LINK_COLOR);
    }

    public static MutableComponent buildFlagMessageClearLink(IProtectedRegion region, IFlag flag) {
        MutableComponent hover = Component.translatableWithFallback("cli.flag.msg.Component.set.default", "Reset flag message for flag '%s' of '%s' to config default", flag.getName(), region.getName());
        MutableComponent text = Component.translatableWithFallback("cli.link.remove", "x");
        String cmd = buildFlagMsgClearCmd(region, flag.getName());
        return buildExecuteCmdComponent(text, hover, cmd, RUN_COMMAND, REMOVE_CMD_COLOR);
    }

    public static MutableComponent buildFlagMuteToggleLink(IProtectedRegion region, IFlag flag, boolean shortLink) {
        String fallback = !flag.getFlagMsg().isMuted() ? "inactive" : "active";
        MutableComponent hover = Component.translatableWithFallback("cli.flag.msg.mute.set.link.hover", "Activate flag alert for '%s' in '%s'", flag.getName(), region.getName());
        MutableComponent text = Component.translatableWithFallback("cli.flag.msg.mute.set.link.text." + !flag.getFlagMsg().isMuted(), fallback);
        if (shortLink) {
            text = Component.translatableWithFallback("cli.flag.msg.mute.set.link.Component.toggle", "m");
        }
        ChatFormatting textChatFormatting = !flag.getFlagMsg().isMuted() ? GREEN : GRAY;
        String cmd = buildFlagMsgMuteToggleCmd(region, flag.getName());
        return buildExecuteCmdComponent(text, hover, cmd, RUN_COMMAND, textChatFormatting);
    }

    /**
     * Builds a TextComponent for the given flag and region. <br>
     * Currently not used in the CLI for obvious reasons. <br>
     */
    public static MutableComponent buildRemoveAllRegionsAttemptLink(DimensionRegionCache dimCache) {
        String cmd = buildCommandStr(DIM.toString(), dimCache.getDimensionalRegion().getName(), DELETE_ALL.toString(), REGIONS.toString());
        MutableComponent hover = Component.translatableWithFallback("cli.msg.info.dim.region.remove.all.link.hover", "Remove all regions from %s", dimCache.getDimensionalRegion().getName());
        MutableComponent text = Component.translatableWithFallback("cli.link.remove", "x");
        return buildExecuteCmdComponent(text, hover, cmd, RUN_COMMAND, REMOVE_CMD_COLOR);
    }

    public static MutableComponent buildRemoveAllRegionsLink(DimensionRegionCache dimCache) {
        String cmd = buildCommandStr(DIM.toString(), dimCache.getDimensionalRegion().getName(), DELETE_ALL.toString(), REGIONS.toString(), FOREVER.toString(), SERIOUSLY.toString());
        MutableComponent hover = Component.translatableWithFallback("cli.msg.info.dim.region.remove.all.link.hover", "Remove all regions from %s", dimCache.getDimensionalRegion().getName());
        MutableComponent text = Component.translatableWithFallback("cli.link.remove", "x");
        return buildExecuteCmdComponent(text, hover, cmd, RUN_COMMAND, REMOVE_CMD_COLOR);
    }

    public static MutableComponent buildRemoveRegionLink(IProtectedRegion region) {
        String cmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), DELETE.toString(), region.getName(), "-y");
        MutableComponent hover = Component.translatableWithFallback("cli.msg.info.dim.region.remove.link.hover", "Remove region %s", region.getName());
        MutableComponent text = Component.translatableWithFallback("cli.link.remove", "x");
        return buildExecuteCmdComponent(text, hover, cmd, RUN_COMMAND, REMOVE_CMD_COLOR);
    }

    // [x]
    public static MutableComponent buildParentClearLink(IMarkableRegion region) {
        String clearRegionParentCmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), PARENT.toString(), REMOVE.toString());
        MutableComponent parentClearLinkText = Component.translatableWithFallback("cli.link.remove", "x");
        MutableComponent parentClearHoverText = Component.translatableWithFallback("cli.msg.info.region.parent.clear.link.hover", "Clear '%s' as parent region", region.getParent().getName());
        return buildExecuteCmdComponent(parentClearLinkText, parentClearHoverText, clearRegionParentCmd, RUN_COMMAND, REMOVE_CMD_COLOR);
    }

    // No parent set [+]
    private static MutableComponent createParentAddLink(IProtectedRegion region) {
        String setRegionParentCmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), PARENT.toString(), SET.toString(), "");
        MutableComponent setParentLinkText = Component.translatableWithFallback("cli.link.add", "+");
        MutableComponent setParentHoverText = Component.translatableWithFallback("cli.msg.info.region.parent.set.link.hover", "Set parent for region %s", region.getName());
        return Messages.substitutable("%s %s", Component.translatableWithFallback("cli.msg.info.region.parent.null", "No parent set"), buildExecuteCmdComponent(setParentLinkText, setParentHoverText, setRegionParentCmd, RUN_COMMAND, GREEN));
    }

    // [n regions] [+]
    public static MutableComponent buildDimRegionsLink(DimensionRegionCache dimCache) {
        DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
        String command = buildCommandStr(DIM.toString(), dimRegion.getDim().location().toString(), LIST.toString(), CommandConstants.LOCAL.toString());
        MutableComponent text = Component.translatableWithFallback("cli.msg.dim.info.region.list.link.text", "%s region(s)", dimCache.getRegionCount());
        MutableComponent hover = Component.translatableWithFallback("cli.msg.dim.info.region.list.link.hover", "List regions in %s", dimRegion.getName());
        MutableComponent listLocalRegionsLink = buildExecuteCmdComponent(text, hover, command, RUN_COMMAND, LINK_COLOR);
        MutableComponent createRegionLink = buildDimCreateRegionLink(dimRegion);
        if (dimRegion.getChildren().isEmpty()) {
            return Messages.substitutable("%s %s", text, createRegionLink);
        }
        return Messages.substitutable("%s %s", listLocalRegionsLink, createRegionLink);
    }

    public static MutableComponent buildRegionListChildrenLink(IProtectedRegion region) {
        MutableComponent text = Component.translatableWithFallback("cli.msg.info.region.children.list.link.text", "%s child regions(s)", region.getChildren().size());
        MutableComponent hover = Component.translatableWithFallback("cli.msg.info.region.children.list.link.hover", "List direct child regions of '%s'", region.getName());
        String command = Commands.buildListChildRegionCommand(region);
        return switch (region.getRegionType()) {
            case GLOBAL -> {
                // [n dimensions(s)]
                Collection<String> dimensionList = RegionDataManager.get().getDimensionList();
                MutableComponent listDimRegionsLinkText = Component.translatableWithFallback("cli.msg.global.info.region.list.link.text", "%s dimensions(s)", dimensionList.size());
                MutableComponent listDimRegionsHoverText = Component.translatableWithFallback("cli.msg.global.info.region.list.link.hover", "List all Dimensional Regions");
                yield buildExecuteCmdComponent(listDimRegionsLinkText, listDimRegionsHoverText, command, RUN_COMMAND, LINK_COLOR);
            }
            case DIMENSION -> {
                // [n children] [+]
                MutableComponent listDimRegionsListLink = buildExecuteCmdComponent(text, hover, command, RUN_COMMAND, LINK_COLOR);
                if (region.getChildren().isEmpty()) {
                    yield Messages.substitutable("%s %s", text, buildDimCreateRegionLink(region));
                }
                yield Messages.substitutable("%s %s", listDimRegionsListLink, buildDimCreateRegionLink(region));
            }
            case LOCAL -> {
                // [n children] [+]
                MutableComponent regionChildrenLink = buildExecuteCmdComponent(text, hover, command, RUN_COMMAND, LINK_COLOR);
                MutableComponent addChildrenLink = buildRegionAddChildrenLink(region);
                if (region.getChildren().isEmpty()) {
                    yield Messages.substitutable("%s %s", text, addChildrenLink);
                }
                yield Messages.substitutable("%s %s", regionChildrenLink, addChildrenLink);
            }
            default -> throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        };
    }

    public static MutableComponent buildRegionAddChildrenLink(IProtectedRegion region) {
        String addChildrenCmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), ADD.toString(), CHILD.toString(), "");
        MutableComponent addChildrenLinkText = Component.translatableWithFallback("cli.link.add", "+");
        MutableComponent addChildrenHoverText = Component.translatableWithFallback("cli.msg.info.region.children.add.link.hover", "Add child to region %s", region.getName());
        return buildExecuteCmdComponent(addChildrenLinkText, addChildrenHoverText, addChildrenCmd, SUGGEST_COMMAND, ADD_CMD_COLOR);
    }

    public static MutableComponent buildDimCreateRegionLink(IProtectedRegion region) {
        String dimCreateRegionCmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), CREATE.toString(), CommandConstants.LOCAL.toString(), "");
        MutableComponent createRegionLinkText = Component.translatableWithFallback("cli.link.add", "+");
        MutableComponent createRegionHoverText = Component.translatableWithFallback("cli.msg.dim.info.region.create.link.hover", "Create region in dimension %s", region.getName());
        return buildExecuteCmdComponent(createRegionLinkText, createRegionHoverText, dimCreateRegionCmd, SUGGEST_COMMAND, ADD_CMD_COLOR);
    }

    /**
     * [n] responsible flag(s) | [m] flag(s) [+]
     * With the responsible flags being the applied flags for this region, including the parent flags
     * and the later are only the flags defined in this region, whether they are applied, active or not
     *
     * @param region the to build the flag list link component for
     * @return [n] responsible flag(s) | [m] flag(s) [+]
     */
    public static MutableComponent buildResponsibleFlagListLink(IProtectedRegion region) {
        Map<String, FlagCorrelation> flagsInHierarchy = getFlagMapRecursive(region, null);
        MutableComponent responsibleFlagsNumber = buildTextWithHoverMsg(Messages.substitutable("%s", flagsInHierarchy.size()), Component.translatableWithFallback("cli.msg.info.region.flag.responsible.number.hover", "%s responsible flag(s) applicable for %s", flagsInHierarchy.size(), region.getName()), LINK_COLOR);
        MutableComponent responsibleFlagListHoverText = Component.translatableWithFallback("cli.msg.info.region.flag.responsible.link.hover", "Show responsible region flags for %s", region.getName());
        String flagListCmd = buildListFlagsCommand(region);
        MutableComponent responsibleFlagListLink = buildExecuteCmdComponent(responsibleFlagsNumber, responsibleFlagListHoverText, flagListCmd, RUN_COMMAND, LINK_COLOR);
        MutableComponent responsibleFlagsComp = flagsInHierarchy.isEmpty() ? responsibleFlagsNumber : responsibleFlagListLink;
        return Component.translatableWithFallback("cli.msg.info.region.flag.responsible.link.text", "%s responsible flag(s)", responsibleFlagsComp);
    }

    // [m] flag(s) [+]
    public static MutableComponent buildRegionFlagListLink(IProtectedRegion region) {
        MutableComponent regionFlagNumber = buildTextWithHoverMsg(Messages.substitutable("%s", region.getFlags().size()), Component.translatableWithFallback("cli.msg.info.region.flag.number.hover", "%s flag(s)", region.getFlags().size(), region.getName()), LINK_COLOR);
        MutableComponent flagListHoverText = Component.translatableWithFallback("cli.msg.info.region.flag.link.hover", "%s flag(s)", region.getName());
        String regionFlagListCmd = buildListRegionFlagsCommand(region);
        MutableComponent regionFlagListLink = buildExecuteCmdComponent(regionFlagNumber, flagListHoverText, regionFlagListCmd, RUN_COMMAND, LINK_COLOR);
        MutableComponent regionFlagsComp = region.getFlags().isEmpty() ? regionFlagNumber : regionFlagListLink;
        return Messages.substitutable("%s %s", Component.translatableWithFallback("cli.msg.info.region.flag.region.link.text", "%s flag(s)", regionFlagsComp), buildSuggestAddFlagLink(region));
    }

    public static MutableComponent buildSuggestAddFlagLink(IProtectedRegion region) {
        return buildAddFlagLink(region, "", SUGGEST_COMMAND);
    }

    public static MutableComponent buildAddFlagLink(IProtectedRegion region, String flag) {
        return buildAddFlagLink(region, flag, RUN_COMMAND);
    }

    public static MutableComponent buildAddFlagLink(IProtectedRegion region, String flag, ClickEvent.Action action) {
        MutableComponent hoverText = Component.translatableWithFallback("cli.msg.info.region.flag.add.link.hover", "Add new flag to region %s", region.getName());
        MutableComponent linkText = Component.translatableWithFallback("cli.link.add", "+");
        String cmd = buildAddFlagCommand(region, flag);
        return buildExecuteCmdComponent(linkText, hoverText, cmd, action, ADD_CMD_COLOR);
    }

    /**
     * [X,Y,Z]
     */
    public static MutableComponent buildDimensionalBlockTpLink(ResourceKey<Level> dim, BlockPos target) {
        String teleportCmd = buildTeleportCmd(dim, "@s", target);
        MutableComponent text = Component.translatableWithFallback("cli.msg.info.region.area.tp.block.link.text", "%s", buildBlockPosLinkText(target));
        MutableComponent hover = Component.translatableWithFallback("cli.msg.info.region.area.tp.block.link.hover", "Teleport to block");
        return buildExecuteCmdComponent(text, hover, teleportCmd, RUN_COMMAND, TP_COLOR);
    }

    /**
     * [region] @ [X,Y,Z]
     */
    public static MutableComponent buildRegionInfoAndTpLink(IMarkableRegion region) {
        return Messages.substitutable("%s @ %s", buildRegionInfoLink(region), buildRegionTeleportLink(region, null));
    }

    public static MutableComponent buildRegionInfoAndTpLink(IMarkableRegion region, MutableComponent regionInfoLinkWithIndicator) {
        return Messages.substitutable("%s @ %s", regionInfoLinkWithIndicator, buildRegionTeleportLink(region, null));
    }

    public static MutableComponent buildRegionTeleportLink(IMarkableRegion region, Player player) {
        String regionTpCmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), AREA.toString(), TELEPORT.toString());
        if (player != null) {
            regionTpCmd = appendSubCommand(regionTpCmd, player.getScoreboardName());
        }
        MutableComponent text = Component.translatableWithFallback("cli.msg.info.region.area.tp.link.text", "%s", buildBlockPosLinkText(region.getTpTarget()));
        MutableComponent hover = Component.translatableWithFallback("cli.msg.info.region.area.tp.link.hover", "Teleport to region '%s' in dimension '%s", region.getName(), region.getDim().location().toString());
        return buildExecuteCmdComponent(text, hover, regionTpCmd, RUN_COMMAND, TP_COLOR);
    }

    public static MutableComponent buildRegionSetTpLink(IMarkableRegion region) {
        String setTpPosCmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), AREA.toString(), TELEPORT.toString(), SET.toString(), "");
        MutableComponent linkText = Component.translatableWithFallback("cli.msg.info.region.area.tp.set.link.text", "set TP");
        MutableComponent hoverText = Component.translatableWithFallback("cli.msg.info.region.area.tp.set.link.hover", "Set new teleport anchor for '%s'", region.getName());
        return buildExecuteCmdComponent(linkText, hoverText, setTpPosCmd, SUGGEST_COMMAND, LINK_COLOR);
    }

    public static MutableComponent buildDimSuggestRegionRemovalLink(IMarkableRegion region) {
        String cmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), DELETE.toString(), region.getName());
        MutableComponent hover = Component.translatableWithFallback("cli.msg.info.dim.region.remove.link.hover", "Remove region %s", region.getName());
        MutableComponent text = Component.translatableWithFallback("cli.link.remove", "x");
        return buildExecuteCmdComponent(text, hover, cmd, SUGGEST_COMMAND, REMOVE_CMD_COLOR);
    }

    public static MutableComponent buildRegionRemoveChildLink(IProtectedRegion region, IProtectedRegion child) {
        String command = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), REMOVE.toString(), CHILD.toString(), child.getName());
        MutableComponent linkText = Component.translatableWithFallback("cli.link.remove", "x");
        MutableComponent linkHoverText = Component.translatableWithFallback("cli.msg.info.region.children.remove.link.hover", "Remove child '%s' from region %s", child.getName(), region.getName());
        return buildExecuteCmdComponent(linkText, linkHoverText, command, SUGGEST_COMMAND, REMOVE_CMD_COLOR);
    }

    public static MutableComponent buildRegionActionUndoLink(String cmd, CommandConstants toReplace, CommandConstants replacement) {
        String revertCmd = ArgumentUtil.revertCommand(cmd, toReplace, replacement);
        MutableComponent revertLinkText = Component.translatableWithFallback("cli.link.action.undo.text", "<-");
        MutableComponent revertLinkHover = Component.translatableWithFallback("cli.link.action.undo.hover", "Undo action.");
        return buildExecuteCmdComponent(revertLinkText, revertLinkHover, revertCmd, RUN_COMMAND, DARK_RED);
    }

    public static MutableComponent buildRegionActionUndoLink(String cmd, String toReplace, String replacement) {
        String revertCmd = ArgumentUtil.revertCommand(cmd, toReplace, replacement);
        MutableComponent revertLinkText = Component.translatableWithFallback("cli.link.action.undo.text", "<-");
        MutableComponent revertLinkHover = Component.translatableWithFallback("cli.link.action.undo.hover", "Undo action.");
        return buildExecuteCmdComponent(revertLinkText, revertLinkHover, revertCmd, RUN_COMMAND, DARK_RED);
    }

    // TODO: Combine from ChatComponentBuilder::buildRemoveGroupEntry
    public static MutableComponent buildRemoveGroupMemberLink(IProtectedRegion region, String name, GroupType groupType, String group, MutableComponent linkText, MutableComponent hoverText) {
        String cmd = Commands.buildRemoveGroupMemberCommand(region, groupType, group, name);
        return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, REMOVE_CMD_COLOR);
    }

    public static MutableComponent buildRemoveLinkForOfflinePlayer(IProtectedRegion region, String name, GroupType groupType, String group, MutableComponent linkText, MutableComponent hoverText) {
        String cmd = buildRemoveOfflinePlayerCommand(region, group, groupType, name);
        return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, REMOVE_CMD_COLOR);
    }
}
