package de.z0rdak.yawp.util;

import de.z0rdak.yawp.api.commands.CommandConstants;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.area.IMarkableArea;
import de.z0rdak.yawp.core.area.SphereArea;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.group.GroupType;
import de.z0rdak.yawp.core.group.PlayerContainer;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.data.region.RegionDataManager;
import de.z0rdak.yawp.util.text.Messages;
import net.minecraft.ChatFormatting;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.network.chat.*;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.levelgen.structure.BoundingBox;
import net.minecraft.world.scores.Team;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.api.commands.CommandConstants.DIM;
import static de.z0rdak.yawp.api.commands.Commands.buildCommandStr;
import static de.z0rdak.yawp.api.permission.Permissions.GROUP_LIST;
import static de.z0rdak.yawp.util.ChatLinkBuilder.*;
import static de.z0rdak.yawp.util.text.Messages.LINK_COLOR;
import static de.z0rdak.yawp.util.text.Messages.REMOVE_CMD_COLOR;
import static net.minecraft.ChatFormatting.*;
import static net.minecraft.network.chat.ClickEvent.Action.RUN_COMMAND;
import static net.minecraft.network.chat.ClickEvent.Action.SUGGEST_COMMAND;

public class ChatComponentBuilder {

    private ChatComponentBuilder() {
    }

    public static String buildBlockCoordinateStr(BlockPos target) {
        return target.getX() + " " + target.getY() + " " + target.getZ();
    }

    public static String shortBlockPos(BlockPos target) {
        return "[X=" + target.getX() + ", Y=" + target.getY() + ", Z=" + target.getZ() + "]";
    }

    public static String tinyBlockPos(BlockPos target) {
        return "[" + buildBlockCoordinateStr(target) + "]";
    }

    public static String buildBlockPosLinkText(BlockPos target) {
        return target.getX() + ", " + target.getY() + ", " + target.getZ();
    }

    public static MutableComponent buildHeader(MutableComponent header) {
        // return Messages.substitutable("%s %s %s", ChatFormatting.BOLD, header, ChatFormatting.BOLD);
        return header;
    }

    public static MutableComponent buildExecuteCmdComponent(MutableComponent linkText, MutableComponent hoverText, String command, ClickEvent.Action eventAction, ChatFormatting color) {
        MutableComponent text = ComponentUtils.wrapInSquareBrackets(linkText);
        return text.setStyle(text.getStyle()
                .withColor(color)
                .withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, hoverText))
                .withClickEvent(new ClickEvent(eventAction, command)));
    }

    public static MutableComponent buildPlayerHoverComponent(Player player) {
        MutableComponent playerName = Component.literal(player.getScoreboardName());
        MutableComponent playerInfo = Messages.substitutable("%s (%s)", player.getDisplayName(), player.getUUID().toString());
        playerName.setStyle(playerName.getStyle()
                .withColor(LINK_COLOR)
                .withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, playerInfo))
                .withClickEvent(new ClickEvent(SUGGEST_COMMAND, "/tell " + playerName.getString() + " ")));
        return playerName;
    }

    public static MutableComponent buildTeamHoverComponent(Team team) {
        MutableComponent playerName = Component.literal(team.getName());
        playerName.setStyle(playerName.getStyle()
                .withColor(LINK_COLOR)
                .withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, Component.translatableWithFallback("cli.msg.info.region.group.link.hover", "Click to display team info")))
                .withClickEvent(new ClickEvent(RUN_COMMAND, "/team list " + team.getName())));
        return playerName;
    }

    public static MutableComponent buildRegionAreaDetailComponent(IMarkableRegion region) {
        IMarkableArea area = region.getArea();
        MutableComponent areaInfo = Component.literal(area.getAreaType().areaType);
        switch (area.getAreaType()) {
            case CUBOID:
                return Messages.substitutable("%s, %s", areaInfo, buildCuboidAreaInfo((CuboidArea) area));
            case SPHERE:
                return Messages.substitutable("%s, %s", areaInfo, buildSphereAreaInfo((SphereArea) area));
            default:
                throw new IllegalArgumentException("Invalid area type");
        }
    }

    /**
     * Size: X=69, Y=10, Z=42
     */
    private static MutableComponent buildCuboidAreaInfo(CuboidArea cuboidArea) {
        return Component.translatableWithFallback("cli.msg.info.region.area.area.size.Component.cuboid", "Size: %s %s %s",
                buildAreaAxisInfoComponent(cuboidArea, Direction.Axis.X),
                buildAreaAxisInfoComponent(cuboidArea, Direction.Axis.Y),
                buildAreaAxisInfoComponent(cuboidArea, Direction.Axis.Z));
    }

    /**
     * Center: [X,Y,Z], Radius: 5, Diameter: 11
     */
    private static MutableComponent buildSphereAreaInfo(SphereArea sphereArea) {
        int diameter = (sphereArea.getRadius() * 2) + 1;
        MutableComponent centerPos = Component.literal(buildBlockPosLinkText(sphereArea.getCenterPos()));
        return Component.translatableWithFallback("cli.msg.info.region.area.area.size.Component.sphere", "Center: %s, Radius: %s, Diameter: %s",
                buildTextWithHoverAndBracketsMsg(centerPos, centerPos, WHITE), sphereArea.getRadius(), diameter);
    }

    /**
     * Builds component showing size of the area for the given axis with a hover text displaying the block range of the axis.
     * Axis=N, e.g. X=5
     */
    private static MutableComponent buildAreaAxisInfoComponent(CuboidArea cuboidArea, Direction.Axis axis) {
        BoundingBox area = cuboidArea.getArea();
        int axisSize = AreaUtil.blocksOnAxis(area, axis);
        String axisName = axis.getName().toUpperCase();
        int min = axis.choose(area.minX(), area.minY(), area.minZ());
        int max = axis.choose(area.maxX(), area.maxY(), area.maxZ());
        return buildTextWithHoverAndBracketsMsg(Component.literal(axisName + "=" + axisSize), Component.literal(axisName + ": " + min + " - " + max), WHITE);
    }

    public static MutableComponent buildTextWithHoverAndBracketsMsg(MutableComponent text, MutableComponent hoverText, ChatFormatting color) {
        MutableComponent bracketedText = ComponentUtils.wrapInSquareBrackets(text);
        return buildTextWithHoverMsg(bracketedText, hoverText, color);
    }

    public static MutableComponent buildTextWithHoverMsg(MutableComponent text, MutableComponent hoverText, ChatFormatting color) {
        text.setStyle(text.getStyle().withColor(color).withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, hoverText)));
        return text;
    }

    public static MutableComponent buildHelpStartComponent() {
        String command = buildCommandStr(CommandConstants.GLOBAL.toString(), CommandConstants.INFO.toString());
        MutableComponent text = Component.translatableWithFallback("help.hint.link.text", "Start here");
        MutableComponent hover = Component.translatableWithFallback("help.hint.link.hover", "Use '/%s global info' as a starting point to manage the global region", "/" + Constants.MOD_ID);
        return buildExecuteCmdComponent(text, hover, command, ClickEvent.Action.RUN_COMMAND, LINK_COLOR);
    }

    public static List<String> getGroupsForRegion(IProtectedRegion region) {
        return GROUP_LIST;
    }

    public static int getGroupSize(IProtectedRegion region, String groupName) {
        PlayerContainer group = region.getGroup(groupName);
        return group.getPlayers().size() + group.getTeams().size();
    }

    public static MutableComponent buildGroupListHeader(IProtectedRegion region, String group) {
        MutableComponent groupLink = buildGroupLink(region, group, getGroupSize(region, group));
        return buildHeader(Component.translatableWithFallback("cli.msg.info.header.in", "== %s in %s ==", groupLink, buildRegionInfoLink(region)));
    }

    public static MutableComponent buildGroupTypeHeader(IProtectedRegion region, String group, GroupType groupType) {
        String fallback = "== Region '%s' " + groupType.name + " in %s ==";
        return Component.translatableWithFallback("cli.msg.info.region.group." + groupType.name + ".list", fallback, buildRegionInfoLink(region), group);
    }

    public static MutableComponent buildFlagStateComponent(IProtectedRegion region, IFlag flag) {
        FlagState state = flag.getState();
        MutableComponent text = Component.literal(state.name);
        MutableComponent hover = Component.empty();
        ChatFormatting color = WHITE;
        switch (state) {
            case ALLOWED:
                color = GREEN;
                hover = Component.translatableWithFallback("cli.flag.state.allowed.info.hover", "A flag with allowed state does not prevent the related action");
                break;
            case DENIED:
                color = RED;
                hover = Component.translatableWithFallback("cli.flag.state.denied.info.hover", "A flag with denied state prevents the related action");
                break;
            case DISABLED:
                color = GRAY;
                hover = Component.translatableWithFallback("cli.flag.state.disabled.info.hover", "A disabled flag is not considered in flag checks");
                break;
        }
        MutableComponent stateInfo = buildTextWithHoverAndBracketsMsg(text, hover, color);
        return Messages.substitutable("%s %s", stateInfo, buildFlagStateSuggestionLink(region, flag));
    }

    public static MutableComponent buildFlagMessageHoverText(IProtectedRegion region, IFlag flag) {
        MutableComponent flagMsgText = truncateMsg(flag);
        MutableComponent hoverText = Component.literal(flag.getFlagMsg().msg());
        // if flag has default msg, use default msg
        if (flag.getFlagMsg().isDefault()) {
            String hoverFallback = "[{region}]: The '{flag}' flag denies this action here!";
            hoverText = Component.translatableWithFallback("flag.msg.deny." + region.getRegionType().type + ".default", hoverFallback);
        }
        return buildTextWithHoverAndBracketsMsg(flagMsgText, hoverText, WHITE);
    }

    public static MutableComponent truncateMsg(IFlag flag, int length) {
        String flagMsg = flag.getFlagMsg().msg();
        if (flag.getFlagMsg().msg().length() > length) {
            flagMsg = flagMsg.substring(0, length) + "...";
        }
        return Component.literal(flagMsg);
    }

    public static MutableComponent truncateMsg(IFlag flag) {
        return truncateMsg(flag, 30);
    }

    /**
     * Message: [set] [x]: 'msg' <br>
     */
    public static MutableComponent buildFlagMessageComponent(IProtectedRegion region, IFlag flag) {
        MutableComponent editLink = buildFlagMessageEditLink(region, flag);
        MutableComponent clearLink = buildFlagMessageClearLink(region, flag);
        MutableComponent flagMsgTextWithHover = buildFlagMessageHoverText(region, flag);
        return Messages.substitutable("%s %s '%s'", editLink, clearLink, flagMsgTextWithHover);
    }

    public static List<Component> buildRemoveRegionEntries(IProtectedRegion parent, List<IProtectedRegion> regions) {
        return regions.stream().map(region -> buildRemoveRegionEntry(parent, region)).collect(Collectors.toList());
    }

    public static MutableComponent buildRemoveRegionEntry(IProtectedRegion parent, IProtectedRegion region) {
        MutableComponent regionRemoveLink;
        switch (parent.getRegionType()) {
            case GLOBAL: {
                regionRemoveLink = Messages.substitutable("%s %s", buildDimResetComponent((DimensionalRegion) region), buildRegionInfoLink(region));
                break;
            }
            case DIMENSION: {
                MutableComponent removeLink = Component.empty();
                MutableComponent regionInfoLinkWithIndicator = Component.empty();
                MutableComponent childCompInfo = Component.translatableWithFallback("cli.msg.info.dim.region.child.hover", "This is a direct child region of the Dimensional Region");
                MutableComponent childIndicator = buildTextWithHoverAndBracketsMsg(Component.literal("*"), childCompInfo, GOLD);
                if (parent.hasChild(region)) {
                    regionInfoLinkWithIndicator = Messages.substitutable("%s%s", buildRegionInfoLink(region), childIndicator);
                } else {
                    regionInfoLinkWithIndicator = Messages.substitutable("%s", buildRegionInfoLink(region));
                }
                removeLink = buildDimSuggestRegionRemovalLink((IMarkableRegion) region);
                regionRemoveLink = Messages.substitutable("%s %s", removeLink, buildRegionInfoAndTpLink((IMarkableRegion) region, regionInfoLinkWithIndicator));
                break;
            }
            case LOCAL: {
                regionRemoveLink = Messages.substitutable("%s %s", buildRegionRemoveChildLink(parent, region), buildRegionInfoLink(region));
                break;
            }
            default:
                throw new IllegalArgumentException();
        }
        return Messages.substitutable(" - %s", regionRemoveLink);
    }

    private static MutableComponent buildDimResetComponent(DimensionalRegion region) {
        String cmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), CommandConstants.RESET.toString(), DIM.toString());
        MutableComponent hover = Component.translatableWithFallback("cli.dim.reset.dim.link.hover", "Reset Dimensional Region '%s'", region.getName());
        MutableComponent text = Component.translatableWithFallback("cli.link.action.undo.text", "<-");
        return buildExecuteCmdComponent(text, hover, cmd, SUGGEST_COMMAND, REMOVE_CMD_COLOR);
    }

    public static MutableComponent buildInfoComponent(String subjectLangKey, String fallback, MutableComponent payload) {
        return Messages.substitutable("%s: %s", Component.translatableWithFallback(subjectLangKey, fallback), payload);
    }


    public static String buildExecuteCommandString(ResourceKey<Level> dim, String command) {
        return "/execute in " + dim.location() + " run " + command;
    }

    public static String buildTeleportCmd(ResourceKey<Level> dim, String tpSource, BlockPos target) {
        return buildExecuteCommandString(dim, "tp " + tpSource + " " + buildBlockCoordinateStr(target));
    }

    /**
     * @param region    the region to build the link for
     * @param names     the names of the players or teams of the group
     * @param groupType the type of the group (player or team)
     * @param group     the name of the group
     * @return a list of links to remove the group from the region
     */
    public static List<Component> buildRemoveGroupMemberEntries(IProtectedRegion region, List<String> names, GroupType groupType, String group) {
        return names.stream().map(name -> buildRemoveGroupEntry(region, name, groupType, group)).collect(Collectors.toList());
    }

    public static MutableComponent buildRemoveGroupEntry(IProtectedRegion region, String name, GroupType groupType, String group) {
        MutableComponent linkText = Component.translatableWithFallback("cli.link.remove", "x");
        MutableComponent hoverText = Component.translatableWithFallback("cli.msg.info.region.group." + groupType.name + ".remove.link.hover", "Remove " + groupType.name + " '%s' from region %s", name, region.getName());
        MutableComponent regionRemoveLink;
        // TODO: could be moved to ChatLinkBuilder
        if (groupType == GroupType.PLAYER) {
            Player player = RegionDataManager.serverInstance.getPlayerList().getPlayerByName(name);
            boolean isOffline = player == null;
            if (isOffline) {
                MutableComponent offlinePlayerRemoveLink = buildRemoveLinkForOfflinePlayer(region, name, groupType, group, linkText, hoverText);
                return Messages.substitutable(" - %s %s", offlinePlayerRemoveLink, buildGroupInfo(region, name, groupType));
            }
        }
        regionRemoveLink = buildRemoveGroupMemberLink(region, name, groupType, group, linkText, hoverText);
        return Messages.substitutable(" - %s %s", regionRemoveLink, buildGroupInfo(region, name, groupType));
    }

    public static MutableComponent buildGroupInfo(IProtectedRegion region, String groupMemberName, GroupType groupType) {
        return switch (groupType) {
            case PLAYER -> {
                Player player = RegionDataManager.serverInstance.getPlayerList().getPlayerByName(groupMemberName);
                if (player == null) {
                    yield Component.translatable("%s %s", Component.literal(groupMemberName).withStyle(GRAY), Component.translatableWithFallback("cli.msg.info.player.list.entry.offline", "(offline)"));
                } else {
                    yield buildPlayerHoverComponent(player);
                }
            }
            case TEAM -> {
                Team team = RegionDataManager.serverInstance.getScoreboard().getPlayerTeam(groupMemberName);
                yield team == null ? Component.literal(groupMemberName) : buildTeamHoverComponent(team);
            }
        };
    }

    public static List<String> getGroupList(IProtectedRegion region, String group, GroupType groupType) {
        switch (groupType) {
            case PLAYER:
                return getPlayerNamesByState(region, group);
            case TEAM:
                return region.getGroup(group).getTeams().stream().sorted().collect(Collectors.toList());
            default:
                return new ArrayList<>();
        }
    }

    /**
     * Lookup which players are online and put them first, sorted alphabetical by their name
     * Adds the offline players after the online players, also sorted alphabetical by their name
     */
    private static @NotNull List<String> getPlayerNamesByState(IProtectedRegion region, String group) {
        List<String> names = new ArrayList<>(region.getGroup(group).getPlayers().values());
        // Lookup which players are online and put them first, sorted alphabetical by their name
        List<String> onlinePlayerNames = names.stream()
                .map(name -> Map.entry(name, RegionDataManager.serverInstance.getPlayerList().getPlayerByName(name) != null))
                .filter(Map.Entry::getValue)
                .map(Map.Entry::getKey)
                .sorted().toList();
        names.removeAll(onlinePlayerNames);
        // add the offline players after the online players, also sorted alphabetical by their name
        List<String> offlinePlayers = names.stream().sorted().toList();
        List<String> playerNames = new ArrayList<>(onlinePlayerNames);
        playerNames.addAll(offlinePlayers);
        return playerNames;
    }


    public static MutableComponent buildFlagInfoHeader(IProtectedRegion region, IFlag flag) {
        return buildHeader(Component.translatableWithFallback("cli.msg.info.header.flag.in", "== Flag %s in %s ==",
                buildFlagInfoLink(region, flag),
                buildRegionInfoLink(region)));
    }
}
