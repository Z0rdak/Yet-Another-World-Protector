package de.z0rdak.yawp.util;

import de.z0rdak.yawp.commands.CommandConstants;
import de.z0rdak.yawp.commands.RegionCommands;
import de.z0rdak.yawp.commands.arguments.ArgumentUtil;
import de.z0rdak.yawp.config.server.CommandPermissionConfig;
import de.z0rdak.yawp.config.server.RegionConfig;
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
import de.z0rdak.yawp.core.region.RegionType;
import de.z0rdak.yawp.handler.flags.FlagCorrelation;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.ChatFormatting;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.nbt.NbtUtils;
import net.minecraft.network.chat.*;
import net.minecraft.resources.ResourceKey;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.minecraft.world.scores.Team;
import net.minecraftforge.server.ServerLifecycleHooks;
import org.apache.commons.lang3.NotImplementedException;

import java.util.*;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.commands.CommandUtil.GROUP_LIST;
import static de.z0rdak.yawp.commands.arguments.ArgumentUtil.*;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.getFlagMapRecursive;
import static de.z0rdak.yawp.util.LocalRegions.getFlagsWithState;
import static net.minecraft.ChatFormatting.*;
import static net.minecraft.network.chat.ClickEvent.Action.*;


public class ChatComponentBuilder {

    public final static ChatFormatting SUGGEST_COLOR = BLUE;
    public final static ChatFormatting TP_COLOR = GREEN;
    public final static ChatFormatting LINK_COLOR = AQUA;
    public final static ChatFormatting INACTIVE_LINK_COLOR = GRAY;
    public final static ChatFormatting ADD_CMD_COLOR = DARK_GREEN;
    public final static ChatFormatting REMOVE_CMD_COLOR = DARK_RED;
    public static int FIRST_PAGE_IDX = 0;
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
        return Component.translatable("%s %s %s", ChatFormatting.BOLD, header, ChatFormatting.BOLD);
    }

    public static MutableComponent buildExecuteCmdComponent(MutableComponent linkText, MutableComponent hoverText, String command, ClickEvent.Action eventAction, ChatFormatting color) {
        MutableComponent text = ComponentUtils.wrapInSquareBrackets(linkText);
        return text.setStyle(text.getStyle()
                .withColor(color)
                .withClickEvent(new ClickEvent(eventAction, command))
                .withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, hoverText)));
    }

    public static MutableComponent buildPlayerHoverComponent(Player player) {
        HoverEvent.EntityTooltipInfo entityTooltipInfo = new HoverEvent.EntityTooltipInfo(EntityType.PLAYER, player.getUUID(), player.getName());
        MutableComponent playerName = Component.literal(player.getScoreboardName());
        playerName.setStyle(playerName.getStyle()
                .withColor(LINK_COLOR)
                .withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_ENTITY, entityTooltipInfo))
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

    /***
     * [X,Y,Z], ..., [X,Y,Z]
     */
    public static MutableComponent buildAreaMarkedBlocksTpLinks(IMarkableRegion region) {
        List<MutableComponent> tpLinks = region.getArea().getMarkedBlocks()
                .stream()
                .map(pos -> buildDimensionalBlockTpLink(region.getDim(), pos))
                .collect(Collectors.toList());
        MutableComponent blockPosTpLinkList = Component.literal("");
        tpLinks.forEach(tpLink -> blockPosTpLinkList.append(tpLink).append(" "));
        return blockPosTpLinkList;
    }

    public static MutableComponent buildRegionAreaDetailComponent(IMarkableRegion region) {
        IMarkableArea area = region.getArea();
        MutableComponent areaInfo = Component.literal(area.getAreaType().areaType);
        switch (area.getAreaType()) {
            case CUBOID:
                return Component.translatable("%s, %s", areaInfo, buildCuboidAreaInfo((CuboidArea) area));
            case CYLINDER:
                throw new NotImplementedException("cylinder");
            case SPHERE:
                return Component.translatable("%s, %s", areaInfo, buildSphereAreaInfo((SphereArea) area));
            case POLYGON_3D:
                throw new NotImplementedException("polygon");
            case PRISM:
                throw new NotImplementedException("prism");
            default:
                throw new IllegalArgumentException("Invalid area type");
        }
    }

    /**
     * Size: X=69, Y=10, Z=42
     */
    private static MutableComponent buildCuboidAreaInfo(CuboidArea cuboidArea) {
        return Component.translatableWithFallback("cli.msg.info.region.area.area.size.text.cuboid", "Size: %s %s %s",
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
        return Component.translatableWithFallback("cli.msg.info.region.area.area.size.text.sphere", "Center: %s, Radius: %s, Diameter: %s",
                buildTextWithHoverAndBracketsMsg(centerPos, centerPos, WHITE), sphereArea.getRadius(), diameter);
    }

    /**
     * Builds component showing size of the area for the given axis with a hover text displaying the block range of the axis.
     * Axis=N, e.g. X=5
     */
    private static MutableComponent buildAreaAxisInfoComponent(CuboidArea cuboidArea, Direction.Axis axis) {
        int min = (int) Math.floor(cuboidArea.getArea().min(axis));
        int max = (int) Math.floor(cuboidArea.getArea().max(axis));
        int axisSize = Math.max(Math.abs(max - min), 1);
        String axisName = axis.getName().toUpperCase();
        return buildTextWithHoverAndBracketsMsg(
                Component.literal(axisName + "=" + axisSize),
                Component.literal(axisName + ": " + min + " - " + max), WHITE);
    }

    /**
     * [<=expand=>] [<=max=>]
     */
    public static MutableComponent buildRegionAreaExpandLink(IMarkableRegion region) {
        MutableComponent linkText = Component.translatableWithFallback("cli.msg.info.region.area.area.expand.link.text", "<=expand=>");
        MutableComponent linkHover = Component.translatableWithFallback("cli.msg.info.region.area.area.expand.link.hover", "Expand the area for '%s'", region.getName());
        String expandCmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), AREA.toString(), EXPAND.toString(), region.getArea().getAreaType().areaType);
        switch (region.getArea().getAreaType()) {
            case CUBOID: {
                CuboidArea cuboidArea = (CuboidArea) region.getArea();
                int areaLowerLimit = (int) Math.floor(cuboidArea.getArea().minZ);
                int areaUpperLimit = (int) Math.floor(cuboidArea.getArea().maxZ);
                // [<=expand=>]
                String expandCmdSuggestion = appendSubCommand(expandCmd, String.valueOf(areaLowerLimit), String.valueOf(areaUpperLimit));
                MutableComponent expandLink = buildExecuteCmdComponent(linkText, linkHover, expandCmdSuggestion, SUGGEST_COMMAND, LINK_COLOR);
                // [<=max=>]
                MutableComponent maxExpandLinkText = Component.translatableWithFallback("cli.msg.info.region.area.area.expand-max.link.text", "<=max=>");
                MutableComponent maxExpandLinkHover = Component.translatableWithFallback("cli.msg.info.region.area.area.expand-max.link.hover", "Expand area to build limit");
                String maxExpandCmd = appendSubCommand(expandCmd, String.valueOf(RegionCommands.MIN_BUILD_LIMIT), String.valueOf(RegionCommands.MAX_BUILD_LIMIT));
                MutableComponent maxExpandLink = buildExecuteCmdComponent(maxExpandLinkText, maxExpandLinkHover, maxExpandCmd, RUN_COMMAND, LINK_COLOR);
                return Component.translatable("%s %s", expandLink, maxExpandLink);
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
        String blocks = String.join(" ", region.getArea().getMarkedBlocks().stream()
                .map(ChatComponentBuilder::buildBlockCoordinateStr)
                .collect(Collectors.toSet()));
        String setAreaCmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), AREA.toString(), SET.toString(), region.getArea().getAreaType().areaType, blocks);
        return buildExecuteCmdComponent(setAreaLinkText, setAreaLinkHover, setAreaCmd, SUGGEST_COMMAND, LINK_COLOR);
    }

    /**
     * [set area] [set TP] [show area] [<=expand=>] [<=max=>]
     */
    public static MutableComponent buildRegionAreaActionLinks(IMarkableRegion region) {
        return Component.translatable("%s %s %s", buildAreaUpdateLink(region), buildRegionSetTpLink(region), buildRegionAreaExpandLink(region));
        // buildShowAreaToggleLink(region)
    }

    public static MutableComponent buildTextWithHoverAndBracketsMsg(MutableComponent text, MutableComponent hoverText, ChatFormatting color) {
        MutableComponent bracketedText = ComponentUtils.wrapInSquareBrackets(text);
        bracketedText.setStyle(bracketedText.getStyle().withColor(color).withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, hoverText)));
        return bracketedText;
    }

    public static MutableComponent buildTextWithHoverMsg(MutableComponent text, MutableComponent hoverText, ChatFormatting color) {
        text.setStyle(text.getStyle().withColor(color).withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, hoverText)));
        return text;
    }

    public static MutableComponent buildHelpStartComponent() {
        String command = ArgumentUtil.buildCommandStr(CommandConstants.GLOBAL.toString(), CommandConstants.INFO.toString());
        MutableComponent text = Component.translatableWithFallback("help.hint.link.text", "Start here");
        MutableComponent hover = Component.translatableWithFallback("help.hint.link.hover", "Use '/%s global info' as a starting point to manage the global region", CommandPermissionConfig.BASE_CMD);
        return buildExecuteCmdComponent(text, hover, command, ClickEvent.Action.RUN_COMMAND, LINK_COLOR);
    }

    public static MutableComponent buildWikiLink() {
        MutableComponent wikiLinkHover = Component.translatableWithFallback("help.tooltip.wiki.link.hover", "https://github.com/Z0rdak/Yet-Another-World-Protector/wiki");
        MutableComponent wikiLink = Component.translatableWithFallback("help.tooltip.wiki.link.text", "Open Wiki in default browser");
        return buildExecuteCmdComponent(wikiLink, wikiLinkHover, "https://github.com/Z0rdak/Yet-Another-World-Protector/wiki", OPEN_URL, AQUA);
    }

    public static MutableComponent buildAllLocalEnableComponent(DimensionRegionCache dimCache) {
        MutableComponent enableLinkTextKey = Component.translatableWithFallback("cli.msg.info.region.state.alert.all.true.link.text", "all-on");
        MutableComponent enableHoverTextKey = Component.translatableWithFallback("cli.msg.info.region.state.alert.all.true.link.hover", "Activates all local regions of %s", dimCache.getDimensionalRegion().getName());
        MutableComponent disableLinkTextKey = Component.translatableWithFallback("cli.msg.info.region.state.alert.all.false.link.text", "all-off");
        MutableComponent disableHoverTextKey = Component.translatableWithFallback("cli.msg.info.region.state.alert.all.false.link.hover", "Disables all local regions of %s", dimCache.getDimensionalRegion().getName());
        String enableCmd = ArgumentUtil.buildCommandStr(DIM.toString(), dimCache.getDimensionalRegion().getName(), STATE.toString(), ENABLE_LOCAL.toString(), Boolean.TRUE.toString());
        String disableCmd = ArgumentUtil.buildCommandStr(DIM.toString(), dimCache.getDimensionalRegion().getName(), STATE.toString(), ENABLE_LOCAL.toString(), Boolean.FALSE.toString());
        MutableComponent activeAlertLink = buildExecuteCmdComponent(enableLinkTextKey, enableHoverTextKey, enableCmd, RUN_COMMAND, GREEN);
        MutableComponent disableAlertLink = buildExecuteCmdComponent(disableLinkTextKey, disableHoverTextKey, disableCmd, RUN_COMMAND, RED);
        return Component.translatableWithFallback("%s %s", "%s %s", activeAlertLink, disableAlertLink);
    }

    public static MutableComponent buildAllLocalAlertToggleLink(DimensionRegionCache dimCache) {
        MutableComponent enableLinkTextKey = Component.translatableWithFallback("cli.msg.info.region.state.enable.all.true.link.text", "all-on");
        MutableComponent enableHoverTextKey = Component.translatableWithFallback("cli.msg.info.region.state.enable.all.true.link.hover", "Enables alert for all local regions of %s", dimCache.getDimensionalRegion().getName());
        MutableComponent disableLinkTextKey = Component.translatableWithFallback("cli.msg.info.region.state.enable.all.false.link.text", "all-off");
        MutableComponent disableHoverTextKey = Component.translatableWithFallback("cli.msg.info.region.state.enable.all.false.link.hover", "Disables alert for all local regions of %s", dimCache.getDimensionalRegion().getName());
        String enableCmd = ArgumentUtil.buildCommandStr(DIM.toString(), dimCache.getDimensionalRegion().getName(), STATE.toString(), ALERT_LOCAL.toString(), Boolean.TRUE.toString());
        String disableCmd = ArgumentUtil.buildCommandStr(DIM.toString(), dimCache.getDimensionalRegion().getName(), STATE.toString(), ALERT_LOCAL.toString(), Boolean.FALSE.toString());
        MutableComponent activeAlertLink = buildExecuteCmdComponent(enableLinkTextKey, enableHoverTextKey, enableCmd, RUN_COMMAND, GREEN);
        MutableComponent disableAlertLink = buildExecuteCmdComponent(disableLinkTextKey, disableHoverTextKey, disableCmd, RUN_COMMAND, RED);
        return Component.translatableWithFallback("%s %s", "%s %s", activeAlertLink, disableAlertLink);
    }

    public static MutableComponent buildRegionEnableComponent(IProtectedRegion region) {
        String linkTextKey = "cli.msg.info.region.state.enable." + region.isActive() + ".link.text";
        String linkFallback = region.isActive() ? "yes" : "no";
        String hoverTextKey = "cli.msg.info.region.state.enable." + !region.isActive() + ".link.hover";
        String hoverFallback = !region.isActive() ? "Enable flag checks" : "Disable flag checks";
        MutableComponent linkText = Component.translatableWithFallback(linkTextKey, linkFallback);
        MutableComponent hoverText = Component.translatableWithFallback(hoverTextKey, hoverFallback);
        ChatFormatting color = region.isActive() ? GREEN : GRAY;
        return switch (region.getRegionType()) {
            case GLOBAL -> {
                String cmd = ArgumentUtil.buildCommandStr(GLOBAL.toString(), STATE.toString(), ENABLE.toString());
                yield buildExecuteCmdComponent(linkText, hoverText, cmd, ClickEvent.Action.RUN_COMMAND, color);
            }
            case DIMENSION -> {
                String cmd = ArgumentUtil.buildCommandStr(DIM.toString(), region.getDim().location().toString(), STATE.toString(), ENABLE.toString());
                yield buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, color);
            }
            case LOCAL -> {
                String cmd = ArgumentUtil.buildCommandStr(LOCAL.toString(), region.getDim().location().toString(), region.getName(), STATE.toString(), ENABLE.toString());
                yield buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, color);
            }
            default -> throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        };
    }

    public static MutableComponent buildRegionPriorityComponent(IMarkableRegion region) {
        int defaultPriorityInc = RegionConfig.getDefaultPriorityInc();
        String incPriorityCmd = ArgumentUtil.buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), STATE.toString(), PRIORITY.toString(), INC.toString(), String.valueOf(defaultPriorityInc));
        MutableComponent incLinkText = Component.translatableWithFallback("cli.msg.info.region.state.priority.increase.link.text", "+%s", defaultPriorityInc);
        MutableComponent incHoverText = Component.translatableWithFallback("cli.msg.info.region.state.priority.increase.link.hover", "Increase region priority by %s", defaultPriorityInc);
        MutableComponent increaseLink = buildExecuteCmdComponent(incLinkText, incHoverText, incPriorityCmd, RUN_COMMAND, ADD_CMD_COLOR);
        String decPriorityCmd = ArgumentUtil.buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), STATE.toString(), PRIORITY.toString(), DEC.toString(), String.valueOf(defaultPriorityInc));
        MutableComponent decLinkText = Component.translatableWithFallback("cli.msg.info.region.state.priority.decrease.link.text", "+%s", defaultPriorityInc);
        MutableComponent decHoverText = Component.translatableWithFallback("cli.msg.info.region.state.priority.decrease.link.hover", "Decrease region priority by %s", defaultPriorityInc);
        MutableComponent decreaseLink = buildExecuteCmdComponent(decLinkText, decHoverText, decPriorityCmd, RUN_COMMAND, REMOVE_CMD_COLOR);
        String setPriorityCmd = ArgumentUtil.buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), STATE.toString(), PRIORITY.toString(), "");
        MutableComponent setPriorityLinkText = Component.translatableWithFallback("cli.msg.info.region.state.priority.set.link.text", "%s", region.getPriority());
        MutableComponent setPriorityHoverText = Component.translatableWithFallback("cli.msg.info.region.state.priority.set.link.hover", "Set priority for region");
        MutableComponent setPriorityLink = buildExecuteCmdComponent(setPriorityLinkText, setPriorityHoverText, setPriorityCmd, SUGGEST_COMMAND, LINK_COLOR);
        return Component.translatable("%s %s %s", setPriorityLink, increaseLink, decreaseLink);
    }

    public static MutableComponent buildRegionAlertToggleLink(IProtectedRegion region) {
        String linkTextKey = "cli.msg.info.region.state.alert." + !region.isMuted() + ".link.text";
        String hoverTextKey = "cli.msg.info.region.state.alert." + region.isMuted() + ".link.hover";
        String linkFallback = region.isMuted() ? "off" : "on";
        String hoverFallback = !region.isMuted() ? "Turn flag alerts off" : "Turn flag alerts on";
        MutableComponent linkText = Component.translatableWithFallback(linkTextKey, linkFallback);
        MutableComponent hoverText = Component.translatableWithFallback(hoverTextKey, hoverFallback);
        ChatFormatting color = region.isMuted() ? GRAY : GREEN;
        return switch (region.getRegionType()) {
            case GLOBAL -> {
                String cmd = buildCommandStr(GLOBAL.toString(), STATE.toString(), ALERT.toString());
                yield buildExecuteCmdComponent(linkText, hoverText, cmd, ClickEvent.Action.RUN_COMMAND, color);
            }
            case DIMENSION -> {
                String cmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), STATE.toString(), ALERT.toString());
                yield buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, color);
            }
            case LOCAL -> {
                String cmd = buildCommandStr(LOCAL.toString(), region.getDim().location().toString(), region.getName(), STATE.toString(), ALERT.toString());
                yield buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, color);
            }
            default -> throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        };
    }

    public static MutableComponent buildRegionRenameLink(IProtectedRegion region) {
        String cmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), RENAME.toString(), "");
        MutableComponent text = Component.translatableWithFallback("cli.msg.info.region.state.rename.link.text", "rename");
        MutableComponent hover = Component.translatableWithFallback("cli.msg.info.region.state.rename.link.hover", "Rename region '%s'", region.getName());
        return buildExecuteCmdComponent(text, hover, cmd, SUGGEST_COMMAND, LINK_COLOR);
    }

    public static MutableComponent buildRegionInfoLink(IProtectedRegion region) {
        return buildRegionInfoLink(region, Component.translatableWithFallback("cli.msg.info.region.link.hover", "Show region info for %s", region.getName()));
    }

    public static MutableComponent buildRegionInfoLink(IProtectedRegion region, MutableComponent linkText, MutableComponent hoverText) {
        return switch (region.getRegionType()) {
            case GLOBAL -> {
                String command = buildCommandStr(GLOBAL.toString(), INFO.toString());
                yield buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, LINK_COLOR);
            }
            case DIMENSION -> {
                String command = buildCommandStr(DIM.toString(), region.getDim().location().toString(), INFO.toString());
                yield buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, LINK_COLOR);
            }
            case LOCAL -> {
                String cmd = buildCommandStr(LOCAL.toString(), region.getDim().location().toString(), region.getName(), INFO.toString());
                yield buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            default -> throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        };
    }

    public static MutableComponent buildRegionInfoLink(IProtectedRegion region, MutableComponent hoverText) {
        MutableComponent linkText = Component.literal(region.getName());
        return buildRegionInfoLink(region, linkText, hoverText);
    }

    public static MutableComponent buildRegionAreaLink(IMarkableRegion region) {
        String showSpatialPropLink = buildCommandStr(LOCAL.toString(), region.getDim().location().toString(), region.getName(), AREA.toString());
        MutableComponent spatialPropLinkText = Component.translatableWithFallback("cli.msg.info.region.area.link.text", "Area Properties");
        MutableComponent spatialPropHoverText = Component.translatableWithFallback("cli.msg.info.region.area.link.hover", "Show region area properties for %s", region.getName());
        return buildExecuteCmdComponent(spatialPropLinkText, spatialPropHoverText, showSpatialPropLink, RUN_COMMAND, LINK_COLOR);
    }

    public static MutableComponent buildRegionOverviewHeader(IProtectedRegion region) {
        switch (region.getRegionType()) {
            case GLOBAL: {
                MutableComponent dumpLinkText = Component.translatableWithFallback("cli.msg.global.overview.header.dump.link.text", "Global overview");
                MutableComponent dumpLinkHover = Component.translatableWithFallback("cli.msg.global.overview.header.dump.link.hover", "Copy Global Region NBT to clipboard");
                MutableComponent clipBoardDumpLink = buildExecuteCmdComponent(dumpLinkText, dumpLinkHover, NbtUtils.prettyPrint(region.serializeNBT()), ClickEvent.Action.COPY_TO_CLIPBOARD, GOLD);
                return buildHeader(Component.translatableWithFallback("cli.msg.info.header.for", "== %s for %s ==", clipBoardDumpLink, buildRegionInfoLink(region)));
            }
            case DIMENSION: {
                MutableComponent dumpLinkText = Component.translatableWithFallback("cli.msg.dim.overview.header.dump.link.text", "Dimension overview");
                MutableComponent dumpLinkHover = Component.translatableWithFallback("cli.msg.dim.overview.header.dump.link.hover", "Copy Dimensional Region NBT to clipboard");
                MutableComponent clipBoardDumpLink = buildExecuteCmdComponent(dumpLinkText, dumpLinkHover, NbtUtils.prettyPrint(region.serializeNBT()), ClickEvent.Action.COPY_TO_CLIPBOARD, GOLD);
                return buildHeader(Component.translatableWithFallback("cli.msg.info.header.for", "== %s for %s ==", clipBoardDumpLink, buildRegionInfoLink(region)));
            }
            case LOCAL: {
                MutableComponent dumpLinkText = Component.translatableWithFallback("cli.msg.local.overview.header.dump.link.text", "Region overview");
                MutableComponent dumpLinkHover = Component.translatableWithFallback("cli.msg.local.overview.header.dump.link.hover", "Copy Local Region NBT to clipboard");
                MutableComponent clipBoardDumpLink = buildExecuteCmdComponent(dumpLinkText, dumpLinkHover, NbtUtils.prettyPrint(region.serializeNBT()), ClickEvent.Action.COPY_TO_CLIPBOARD, GOLD);
                return buildHeader(Component.translatableWithFallback("cli.msg.info.header.for", "== %s for %s ==", clipBoardDumpLink, buildRegionInfoLink(region)));
            }
            default:
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        }
    }

    /**
     * Players: [n player(s)] [+]
     * // TODO:
     */
    public static MutableComponent buildPlayerListLink(IProtectedRegion region, PlayerContainer players, String group) {
        MutableComponent hoverText = Component.translatableWithFallback("cli.msg.info.region.group.player.list.link.hover", "List players of group '%s' in region %s", group, region.getName());
        MutableComponent linkText = Component.translatableWithFallback("cli.msg.info.region.group.player.list.link.text", "%s player(s)", players.getPlayers().size());
        return switch (region.getRegionType()) {
            case GLOBAL -> {
                String cmd = buildCommandStr(GLOBAL.toString(), LIST.toString(), GROUP.toString(), group, PLAYER.toString());
                yield buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            case DIMENSION -> {
                String cmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), LIST.toString(), GROUP.toString(), group, PLAYER.toString());
                yield buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            case LOCAL -> {
                String cmd = buildCommandStr(LOCAL.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), GROUP.toString(), group, PLAYER.toString());
                yield buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            default -> throw new IllegalArgumentException();
        };
    }

    /**
     * Teams: [n team(s)] [+]
     * // TODO:
     */
    public static MutableComponent buildTeamListLink(IProtectedRegion region, PlayerContainer teams, String group) {
        MutableComponent hoverText = Component.translatableWithFallback("cli.msg.info.region.group.team.list.link.hover", "List teams of group '%s' in region %s", group, region.getName());
        MutableComponent linkText = Component.translatableWithFallback("cli.msg.info.region.group.team.list.link.text", "%s team(s)", teams.getTeams().size());
        switch (region.getRegionType()) {
            case GLOBAL: {
                String cmd = buildCommandStr(GLOBAL.toString(), LIST.toString(), GROUP.toString(), group, TEAM.toString());
                return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            case DIMENSION: {
                String cmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), LIST.toString(), GROUP.toString(), group, TEAM.toString());
                return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            case LOCAL: {
                String cmd = buildCommandStr(LOCAL.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), GROUP.toString(), group, TEAM.toString());
                return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            default:
                throw new IllegalArgumentException();
        }
    }

    public static MutableComponent buildAddToGroupLink(IProtectedRegion region, String group, GroupType groupType) {
        MutableComponent linkText = Component.translatableWithFallback("cli.link.add", "+");
        String fallback = "Add " + groupType.name + " as '%s' to region %s";
        MutableComponent hoverText = Component.translatableWithFallback("cli.msg.info.region.group." + groupType.name + ".add.link.hover", fallback, group, region.getName());
        String subCmd = buildSubCmdStr(ADD.toString(), groupType.name, group, "");
        switch (region.getRegionType()) {
            case GLOBAL: {
                String cmd = buildCommandStr(GLOBAL.toString(), subCmd);
                return buildExecuteCmdComponent(linkText, hoverText, cmd, SUGGEST_COMMAND, ADD_CMD_COLOR);
            }
            case DIMENSION: {
                String cmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), subCmd);
                return buildExecuteCmdComponent(linkText, hoverText, cmd, SUGGEST_COMMAND, ADD_CMD_COLOR);
            }
            case LOCAL: {
                String cmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), subCmd);
                return buildExecuteCmdComponent(linkText, hoverText, cmd, SUGGEST_COMMAND, ADD_CMD_COLOR);
            }
            default:
                throw new IllegalArgumentException();
        }
    }

    public static MutableComponent buildGroupLinks(IProtectedRegion region) {
        return getGroupsForRegion(region).stream()
                .map(group -> buildGroupLink(region, group, getGroupSize(region, group)))
                .reduce(Component.literal(""), (link1, link2) -> link1.append(" ").append(link2));
    }

    public static List<String> getGroupsForRegion(IProtectedRegion region) {
        return GROUP_LIST;
    }

    private static int getGroupSize(IProtectedRegion region, String groupName) {
        PlayerContainer group = region.getGroup(groupName);
        return group.getPlayers().size() + group.getTeams().size();
    }

    public static MutableComponent buildGroupHeader(IProtectedRegion region, String group) {
        MutableComponent groupLink = buildGroupLink(region, group, getGroupSize(region, group));
        return buildHeader(Component.translatableWithFallback("cli.msg.info.header.in", "== %s in %s ==", "", groupLink, buildRegionInfoLink(region)));
    }

    public static MutableComponent buildGroupHeader(IProtectedRegion region, String group, GroupType groupType) {
        String fallback = "== Region '%s' " + groupType.name + " in %s ==";
        return Component.translatableWithFallback("cli.msg.info.region.group." + groupType.name + ".list", fallback, buildRegionInfoLink(region), group);
    }

    public static MutableComponent buildGroupLink(IProtectedRegion region, String group, int groupSie) {
        MutableComponent linkText = Component.translatableWithFallback("cli.msg.info.region.group.list.link.text", "%s %s(s)", groupSie, group);
        MutableComponent hoverText = Component.translatableWithFallback("cli.msg.info.region.group.list.link.hover", "List '%s' for region %s", group, region.getName());
        switch (region.getRegionType()) {
            case GLOBAL: {
                String cmd = buildCommandStr(GLOBAL.toString(), LIST.toString(), group);
                return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            case LOCAL: {
                String cmd = buildCommandStr(LOCAL.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), group);
                return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            case DIMENSION: {
                String cmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), LIST.toString(), group);
                return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            default:
                throw new IllegalArgumentException();
        }
    }

    public static MutableComponent buildGroupTeamListLink(IProtectedRegion region, String group) {
        // Teams: [n team(s)] [+]
        PlayerContainer playerContainer = region.getGroup(group);
        MutableComponent teamAddLink = buildAddToGroupLink(region, group, GroupType.TEAM);
        MutableComponent teamListLink = playerContainer.hasTeams()
                ? buildTeamListLink(region, playerContainer, group)
                : Component.translatableWithFallback("cli.msg.info.region.group.team.list.link.text", "%s team(s)", playerContainer.getTeams().size());
        return Component.translatableWithFallback("%s: %s %s", "%s: %s %s",
                Component.translatableWithFallback("cli.msg.info.region.group.team", "Teams"),
                teamListLink,
                teamAddLink);
    }

    public static MutableComponent buildGroupPlayerListLink(IProtectedRegion region, String group) {
        // Players: [n player(s)] [+]
        PlayerContainer playerContainer = region.getGroup(group);
        MutableComponent playersAddLink = buildAddToGroupLink(region, group, GroupType.PLAYER);
        MutableComponent playerListLink = playerContainer.hasPlayers()
                ? buildPlayerListLink(region, playerContainer, group)
                : Component.translatableWithFallback("cli.msg.info.region.group.player.list.link.text", "%s player(s)", playerContainer.getPlayers().size());
        return Component.translatableWithFallback("%s: %s %s", "%s: %s %s",
                Component.translatableWithFallback("cli.msg.info.region.group.player", "Players"),
                playerListLink,
                playersAddLink);
    }

    /**
     * Creates a TextComponent for flag removal, followed by the flag infos
     *
     * @return - [x] [flagname] [<region-indicator] [] []
     */
    private static MutableComponent buildRemoveFlagEntry(IProtectedRegion region, IFlag flag, ChatFormatting flagLinkColor, ChatFormatting... formattings) {
        String cmd;
        switch (region.getRegionType()) {
            case GLOBAL: {
                cmd = buildCommandStr(GLOBAL.toString(), REMOVE.toString(), FLAG.toString(), flag.getName());
                break;
            }
            case DIMENSION: {
                cmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), REMOVE.toString(), FLAG.toString(), flag.getName());
                break;
            }
            case LOCAL: {
                cmd = buildCommandStr(LOCAL.toString(), region.getDim().location().toString(), region.getName(), REMOVE.toString(), FLAG.toString(), flag.getName());
                break;
            }
            default:
                throw new IllegalArgumentException();
        }
        MutableComponent hoverText = Component.translatableWithFallback("cli.msg.info.region.flag.remove.link.hover", "Remove flag '%s' from region %s", flag.getName(), region.getName());
        MutableComponent linkText = Component.translatableWithFallback("cli.link.remove", "x");
        MutableComponent flagRemoveLink = buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, REMOVE_CMD_COLOR);
        MutableComponent flagQuickActionComponent = buildFlagQuickActionComponent(region, flag, flagLinkColor);
        flagQuickActionComponent.withStyle(formattings);
        return Component.translatable(" - %s %s", flagRemoveLink, flagQuickActionComponent);
    }

    /**
     * Creates a TextComponent with a Link for displaying the flag info. <br></br>
     * Text: [flagname] [regionTypeIdentifier] [s] [m] [o] <br></br>
     * Where <br></br>
     * - [s] is a suggest link to change the flag state, <br></br>
     * - [m] is a quick link to toggle the flag mute state, <br></br>
     * - [o] is a quick link to toggle the flag override state, <br></br>
     *
     * @param region
     * @param flag
     * @return text component for quick flag actions [flagname] [regionTypeIdentifier] [s] [m] [o]
     */
    private static MutableComponent buildFlagQuickActionComponent(IProtectedRegion region, IFlag flag, ChatFormatting flagLinkColor) {
        MutableComponent regionTypeIndicator = Component.literal(region.getRegionType().type.substring(0, 1).toUpperCase());
        MutableComponent hoverText = Component.translatableWithFallback("cli.flag.info.hover", "Show %s flag info of region '%s'", flag.getName(), region.getName());
        MutableComponent flagInfoLink = buildFlagInfoLink(region, flag, flagLinkColor);
        return Component.translatable("%s %s %s %s %s",
                flagInfoLink,
                buildFlagInfoLink(region, flag, regionTypeIndicator, hoverText, DARK_PURPLE),
                buildFlagStateSuggestionLink(region, flag),
                buildFlagMuteToggleLink(region, flag, true),
                buildFlagOverrideToggleLink(region, flag, true));
    }

    /**
     * Creates a TextComponent for displaying the flag info  <br></br>
     * Text: [$flag-name] <br></br>
     * Link: /wp flag dim $dim $flag-name info  <br></br>
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
        switch (region.getRegionType()) {
            case GLOBAL: {
                String cmd = buildCommandStr(FLAG.toString(), GLOBAL.toString(), flag.getName(), INFO.toString());
                return buildExecuteCmdComponent(text, hoverText, cmd, RUN_COMMAND, linkColor);
            }
            case DIMENSION: {
                String cmd = buildCommandStr(FLAG.toString(), DIM.toString(), region.getDim().location().toString(), flag.getName(), INFO.toString());
                return buildExecuteCmdComponent(text, hoverText, cmd, RUN_COMMAND, linkColor);
            }
            case LOCAL: {
                String cmd = buildCommandStr(FLAG.toString(), CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), flag.getName(), INFO.toString());
                return buildExecuteCmdComponent(text, hoverText, cmd, RUN_COMMAND, linkColor);
            }
            default:
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        }
    }

    public static MutableComponent buildFlagInfoLink(IProtectedRegion region, IFlag flag) {
        return buildFlagInfoLink(region, flag, LINK_COLOR);
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
        return Component.translatable("%s %s", stateInfo, buildFlagStateSuggestionLink(region, flag));
    }

    public static MutableComponent buildFlagStateSuggestionLink(IProtectedRegion region, IFlag flag) {
        MutableComponent hover = Component.translatableWithFallback("cli.flag.state.set.link.hover", "Set flag state for '%s' in '%s'", flag.getName(), region.getName());
        MutableComponent text = Component.translatableWithFallback("cli.flag.state.set.link.text", "s");
        switch (region.getRegionType()) {
            case GLOBAL: {
                String cmd = buildCommandStr(FLAG.toString(), GLOBAL.toString(), flag.getName(), STATE.toString(), "");
                return buildExecuteCmdComponent(text, hover, cmd, SUGGEST_COMMAND, LINK_COLOR);
            }
            case DIMENSION: {
                String cmd = buildCommandStr(FLAG.toString(), DIM.toString(), region.getDim().location().toString(), flag.getName(), STATE.toString(), "");
                return buildExecuteCmdComponent(text, hover, cmd, SUGGEST_COMMAND, LINK_COLOR);
            }
            case LOCAL: {
                String cmd = buildCommandStr(FLAG.toString(), CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), flag.getName(), STATE.toString(), "");
                return buildExecuteCmdComponent(text, hover, cmd, SUGGEST_COMMAND, LINK_COLOR);
            }
            case TEMPLATE:
                throw new NotImplementedException("No supported yet");
            default:
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        }
    }

    public static MutableComponent buildFlagOverrideInfoComponent(IFlag flag) {
        String fallback = flag.doesOverride() ? "active" : "inactive";
        MutableComponent linkText = Component.translatableWithFallback("cli.flag.msg.text.link.text." + flag.doesOverride(), fallback);
        MutableComponent hoverText = Component.translatableWithFallback("cli.flag.msg.text.link.hover." + flag.doesOverride(), fallback);
        ChatFormatting color = flag.doesOverride() ? ChatFormatting.GREEN : ChatFormatting.GRAY;
        return buildTextWithHoverAndBracketsMsg(linkText, hoverText, color);
    }


    public static MutableComponent buildFlagOverrideToggleLink(IProtectedRegion region, IFlag flag, boolean shortLink) {
        String fallback = flag.doesOverride() ? "active" : "inactive";
        String fallbackHover = (!flag.doesOverride() ? "Disable" : "Enable") + " flag override for '%s' of '%s'";
        MutableComponent linkText = Component.translatableWithFallback("cli.flag.override.link.text." + flag.doesOverride(), fallback);
        MutableComponent hoverText = Component.translatableWithFallback("cli.flag.override.link.hover." + !flag.doesOverride(), fallbackHover, flag.getName(), region.getName());
        if (shortLink) {
            linkText = Component.translatableWithFallback("cli.flag.override.link.text.toggle", "o");
        }
        ChatFormatting color = flag.doesOverride() ? ChatFormatting.GREEN : ChatFormatting.GRAY;
        switch (region.getRegionType()) {
            case GLOBAL: {
                String cmd = buildCommandStr(FLAG.toString(), GLOBAL.toString(), flag.getName(), OVERRIDE.toString());
                return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, color);
            }
            case DIMENSION: {
                String cmd = buildCommandStr(FLAG.toString(), DIM.toString(), region.getDim().location().toString(), flag.getName(), OVERRIDE.toString());
                return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, color);
            }
            case LOCAL: {
                String cmd = buildCommandStr(FLAG.toString(), CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), flag.getName(), OVERRIDE.toString());
                return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, color);
            }
            case TEMPLATE:
                throw new NotImplementedException("No supported yet");
            default:
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        }
    }

    public static MutableComponent buildFlagMessageEditLink(IProtectedRegion region, IFlag flag) {
        MutableComponent hover = Component.translatableWithFallback("cli.flag.msg.text.set.link.hover", "Change the message shown when the flag '%s' of '%s' is triggered", flag.getName(), region.getName());
        MutableComponent text = Component.translatableWithFallback("cli.flag.msg.text.set.link.text", "Edit");
        String msg = "\"" + flag.getFlagMsg().getMsg() + "\"";
        switch (region.getRegionType()) {
            case GLOBAL: {
                String cmd = buildCommandStr(FLAG.toString(), GLOBAL.toString(), flag.getName(), MSG.toString(), SET.toString(), msg);
                return buildExecuteCmdComponent(text, hover, cmd, SUGGEST_COMMAND, LINK_COLOR);
            }
            case DIMENSION: {
                String cmd = buildCommandStr(FLAG.toString(), DIM.toString(), region.getDim().location().toString(), flag.getName(), MSG.toString(), SET.toString(), msg);
                return buildExecuteCmdComponent(text, hover, cmd, SUGGEST_COMMAND, LINK_COLOR);
            }
            case LOCAL: {
                String cmd = buildCommandStr(FLAG.toString(), CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), flag.getName(), MSG.toString(), SET.toString(), msg);
                return buildExecuteCmdComponent(text, hover, cmd, SUGGEST_COMMAND, LINK_COLOR);
            }
            default:
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        }
    }

    private static MutableComponent buildFlagMessageClearLink(IProtectedRegion region, IFlag flag) {
        MutableComponent hover = Component.translatableWithFallback("cli.flag.msg.text.set.default", "Reset flag message for flag '%s' of '%s' to config default", flag.getName(), region.getName());
        MutableComponent text = Component.translatableWithFallback("cli.link.remove", "x");
        switch (region.getRegionType()) {
            case GLOBAL: {
                String cmd = buildCommandStr(FLAG.toString(), GLOBAL.toString(), flag.getName(), MSG.toString(), CLEAR.toString());
                return buildExecuteCmdComponent(text, hover, cmd, RUN_COMMAND, REMOVE_CMD_COLOR);
            }
            case DIMENSION: {
                String cmd = buildCommandStr(FLAG.toString(), DIM.toString(), region.getDim().location().toString(), flag.getName(), MSG.toString(), CLEAR.toString());
                return buildExecuteCmdComponent(text, hover, cmd, RUN_COMMAND, REMOVE_CMD_COLOR);
            }
            case LOCAL: {
                String cmd = buildCommandStr(FLAG.toString(), CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), flag.getName(), MSG.toString(), CLEAR.toString());
                return buildExecuteCmdComponent(text, hover, cmd, RUN_COMMAND, REMOVE_CMD_COLOR);
            }
            default:
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        }
    }

    public static MutableComponent buildFlagMuteToggleLink(IProtectedRegion region, IFlag flag, boolean shortLink) {
        String fallback = !flag.getFlagMsg().isMuted() ? "inactive" : "active";
        MutableComponent hover = Component.translatableWithFallback("cli.flag.msg.mute.set.link.hover", "Activate flag alert for '%s' in '%s'", flag.getName(), region.getName());
        MutableComponent text = Component.translatableWithFallback("cli.flag.msg.mute.set.link.text." + !flag.getFlagMsg().isMuted(), fallback);
        if (shortLink) {
            text = Component.translatableWithFallback("cli.flag.msg.mute.set.link.text.toggle", "m");
        }
        ChatFormatting textFormatting = !flag.getFlagMsg().isMuted() ? GREEN : GRAY;
        switch (region.getRegionType()) {
            case GLOBAL: {
                String cmd = buildCommandStr(FLAG.toString(), GLOBAL.toString(), flag.getName(), MSG.toString(), MUTE.toString());
                return buildExecuteCmdComponent(text, hover, cmd, RUN_COMMAND, textFormatting);
            }
            case DIMENSION: {
                String cmd = buildCommandStr(FLAG.toString(), DIM.toString(), region.getDim().location().toString(), flag.getName(), MSG.toString(), MUTE.toString());
                return buildExecuteCmdComponent(text, hover, cmd, RUN_COMMAND, textFormatting);
            }
            case LOCAL: {
                String cmd = buildCommandStr(FLAG.toString(), CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), flag.getName(), MSG.toString(), MUTE.toString());
                return buildExecuteCmdComponent(text, hover, cmd, RUN_COMMAND, textFormatting);
            }
            case TEMPLATE:
                throw new NotImplementedException("No supported yet");
            default:
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        }
    }

    public static MutableComponent buildFlagMessageHoverText(IProtectedRegion region, IFlag flag) {
        String flagMsg = flag.getFlagMsg().getMsg();
        if (flag.getFlagMsg().getMsg().length() > 30) {
            flagMsg = flagMsg.substring(0, 30) + "...";
        }
        MutableComponent flagMsgText = Component.literal(flagMsg);
        MutableComponent hoverText = Component.literal(flag.getFlagMsg().getMsg());
        // if flag has default msg, use default msg
        if (flag.getFlagMsg().isDefault()) {
            String hoverFallback = "[{region}]: The '{flag}' flag denies this action here!";
            hoverText = Component.translatableWithFallback("flag.msg.deny." + region.getRegionType().type + ".default", hoverFallback);
        }
        return buildTextWithHoverAndBracketsMsg(flagMsgText, hoverText, WHITE);
    }

    /**
     * Message: [set] [x]: 'msg' <br></br>
     */
    public static MutableComponent buildFlagMessageComponent(IProtectedRegion region, IFlag flag) {
        MutableComponent editLink = buildFlagMessageEditLink(region, flag);
        MutableComponent clearLink = buildFlagMessageClearLink(region, flag);
        MutableComponent flagMsgTextWithHover = buildFlagMessageHoverText(region, flag);
        return Component.translatable("%s %s '%s'", editLink, clearLink, flagMsgTextWithHover);
    }

    /**
     * Show flags for region, with a color of their state
     * But also show parent flags in italic
     * How do we want to handle flags defined in the region but also in the parent?
     * If the child flag is dominant, we display the child flag, and add a hint to the parent
     * If the flag is overriden, we format them with strikethrough and add a link to the parent which overrides it
     */
    public static List<MutableComponent> buildFlagEntries(IProtectedRegion region) {
        List<MutableComponent> flagEntries = new ArrayList<>();
        Map<String, FlagCorrelation> flagMapRecursive = getFlagMapRecursive(region, null);
        Map<FlagState, List<FlagCorrelation>> flagStateListMap = LocalRegions.sortFlagsByState(flagMapRecursive);
        flagEntries.addAll(buildFlagEntries(flagStateListMap, FlagState.ALLOWED));
        flagEntries.addAll(buildFlagEntries(flagStateListMap, FlagState.DENIED));
        flagEntries.addAll(buildFlagEntries(flagStateListMap, FlagState.DISABLED));
        return flagEntries;
    }

    public static List<MutableComponent> buildRegionFlagEntries(IProtectedRegion region) {
        List<MutableComponent> flagEntries = new ArrayList<>();
        flagEntries.addAll(buildRegionFlagEntries(region, FlagState.ALLOWED));
        flagEntries.addAll(buildRegionFlagEntries(region, FlagState.DENIED));
        flagEntries.addAll(buildRegionFlagEntries(region, FlagState.DISABLED));
        return flagEntries;
    }

    private static List<MutableComponent> buildRegionFlagEntries(IProtectedRegion region, FlagState state) {
        List<IFlag> flagsByState = getFlagsWithState(region.getFlagContainer(), state);
        flagsByState.sort(Comparator.comparing(IFlag::getName));
        return flagsByState.stream()
                .map(flag -> buildRemoveFlagEntry(region, flag, colorForState(state)))
                .collect(Collectors.toList());
    }

    private static List<MutableComponent> buildFlagEntries(Map<FlagState, List<FlagCorrelation>> flagStateListMap, FlagState state) {
        List<FlagCorrelation> flagsByState = flagStateListMap.get(state);
        flagsByState.sort(Comparator.comparing(flagCorrelation -> flagCorrelation.getFlag().getName()));
        return flagsByState.stream()
                .map(flagCorrelation -> buildRemoveFlagEntry(flagCorrelation.getRegion(), flagCorrelation.getFlag(), colorForState(state)))
                .collect(Collectors.toList());
    }

    private static ChatFormatting colorForState(FlagState state) {
        switch (state) {
            case ALLOWED:
                return GREEN;
            case DENIED:
                return RED;
            case DISABLED:
                return GRAY;
            default:
                throw new IllegalArgumentException();
        }
    }

    public static List<MutableComponent> buildRemoveRegionEntries(IProtectedRegion parent, List<IProtectedRegion> regions) {
        return regions.stream()
                .map(region -> buildRemoveRegionEntry(parent, region))
                .collect(Collectors.toList());
    }

    /**
     * Builds a TextComponent for the given flag and region. <br></br>
     * Currently not used in the CLI for obvious reasons. <br></br>
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

    public static MutableComponent buildRemoveRegionEntry(IProtectedRegion parent, IProtectedRegion region) {
        MutableComponent regionRemoveLink;
        switch (parent.getRegionType()) {
            case GLOBAL: {
                regionRemoveLink = Component.translatable("%s %s", buildDimResetComponent((DimensionalRegion) region), buildRegionInfoLink(region));
                break;
            }
            case DIMENSION: {
                MutableComponent removeLink = Component.empty();
                MutableComponent childCompInfo = Component.translatableWithFallback("cli.msg.info.dim.region.child.hover", "This is a direct child region of the Dimensional Region");
                MutableComponent childIndicator = buildTextWithHoverAndBracketsMsg(Component.literal("*"), childCompInfo, GOLD);
                if (parent.hasChild(region)) {
                    removeLink = Component.translatable("%s %s%s", buildDimSuggestRegionRemovalLink((IMarkableRegion) region), buildRegionInfoLink(region), childIndicator);
                } else {
                    removeLink = Component.translatable("%s %s", buildDimSuggestRegionRemovalLink((IMarkableRegion) region), buildRegionInfoLink(region));
                }
                regionRemoveLink = Component.translatable("%s @ %s", removeLink, buildRegionInfoAndTpLink((IMarkableRegion) region));
                break;
            }
            case LOCAL: {
                regionRemoveLink = Component.translatable("%s %s", buildRegionRemoveChildLink(parent, region), buildRegionInfoLink(region));
                break;
            }
            default:
                throw new IllegalArgumentException();
        }
        return Component.translatable(" - %s", regionRemoveLink);
    }

    private static MutableComponent buildDimResetComponent(DimensionalRegion region) {
        String cmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), CommandConstants.RESET.toString(), DIM.toString());
        MutableComponent hover = Component.translatableWithFallback("cli.dim.reset.dim.link.hover", "Reset Dimensional Region '%s'", region.getName());
        MutableComponent text = Component.translatableWithFallback("cli.link.action.undo.text", "←");
        return buildExecuteCmdComponent(text, hover, cmd, SUGGEST_COMMAND, REMOVE_CMD_COLOR);
    }

    private static String buildPageCommand(String cmd, int page) {
        return cmd + " " + page;
    }

    public static List<MutableComponent> buildPaginationComponents(MutableComponent description, String cmd, List<MutableComponent> entries, int pageNo, MutableComponent addEmptyLink) {
        List<MutableComponent> paginationComponents = new ArrayList<>();
        int numberOfPages = entries.size() / RegionConfig.getPaginationSize();
        if (numberOfPages == 0 || entries.size() % RegionConfig.getPaginationSize() != 0) {
            numberOfPages += 1;
        }
        if (pageNo < FIRST_PAGE_IDX || pageNo >= numberOfPages) {
            paginationComponents.add(Component.translatableWithFallback("cli.msg.info.pagination.error.index", "Invalid page index supplied: %s (Try [0..%s])", pageNo, numberOfPages - 1).withStyle(RED));
            return paginationComponents;
        }
        boolean hasMultiplePages = numberOfPages > 1;

        MutableComponent paginationControl = buildPaginationControl(
                buildFirstLinkArrow(cmd, pageNo, hasMultiplePages),
                buildPrevLinkArrow(cmd, pageNo, hasMultiplePages),
                pageNo, numberOfPages,
                buildNextLinkArrow(cmd, pageNo, numberOfPages, hasMultiplePages),
                buildLastLinkArrow(cmd, pageNo, numberOfPages, hasMultiplePages)
        );

        int from = pageNo * RegionConfig.getPaginationSize();
        int to = Math.min(RegionConfig.getPaginationSize() + (RegionConfig.getPaginationSize() * pageNo), entries.size());
        List<MutableComponent> entriesForPage = entries.subList(from, to);

        paginationComponents.add(description);
        paginationComponents.addAll(entriesForPage);
        if (hasMultiplePages) {
            int numberOfEmptyEntries = RegionConfig.getPaginationSize() - entriesForPage.size();
            for (int i = 0; i < numberOfEmptyEntries; i++) {
                paginationComponents.add(addEmptyLink);
            }
            paginationComponents.add(paginationControl);
        }
        return paginationComponents;
    }

    private static MutableComponent buildLastLinkArrow(String cmd, int pageNo, int numberOfPages, boolean hasMultiplePages) {
        MutableComponent last = Component.translatableWithFallback("cli.msg.info.pagination.last.text", "⏭");
        MutableComponent lastHover = Component.translatableWithFallback("cli.msg.info.pagination.last.hover", "Last page");
        return hasMultiplePages && pageNo < numberOfPages - 1
                ? buildExecuteCmdComponent(last, lastHover, buildPageCommand(cmd, numberOfPages - 1), RUN_COMMAND, LINK_COLOR)
                : ComponentUtils.wrapInSquareBrackets(last).withStyle(INACTIVE_LINK_COLOR);
    }

    private static MutableComponent buildNextLinkArrow(String cmd, int pageNo, int numberOfPages, boolean hasMultiplePages) {
        MutableComponent next = Component.translatableWithFallback("cli.msg.info.pagination.next.text", "▶");
        MutableComponent nextHover = Component.translatableWithFallback("cli.msg.info.pagination.next.hover", "Next page");
        return hasMultiplePages && pageNo < numberOfPages - 1
                ? buildExecuteCmdComponent(next, nextHover, buildPageCommand(cmd, Math.min(pageNo + 1, numberOfPages - 1)), RUN_COMMAND, LINK_COLOR)
                : ComponentUtils.wrapInSquareBrackets(next).withStyle(INACTIVE_LINK_COLOR);
    }

    private static MutableComponent buildPrevLinkArrow(String cmd, int pageNo, boolean hasMultiplePages) {
        MutableComponent previous = Component.translatableWithFallback("cli.msg.info.pagination.previous.text", "◀");
        MutableComponent previousHover = Component.translatableWithFallback("cli.msg.info.pagination.previous.hover", "Previous page");
        return hasMultiplePages && pageNo > FIRST_PAGE_IDX
                ? buildExecuteCmdComponent(previous, previousHover, buildPageCommand(cmd, Math.max(pageNo - 1, FIRST_PAGE_IDX)), RUN_COMMAND, LINK_COLOR)
                : ComponentUtils.wrapInSquareBrackets(previous).withStyle(INACTIVE_LINK_COLOR);
    }

    private static MutableComponent buildFirstLinkArrow(String cmd, int pageNo, boolean hasMultiplePages) {
        MutableComponent first = Component.translatableWithFallback("cli.msg.info.pagination.first.text", "⏮");
        MutableComponent firstHover = Component.translatableWithFallback("cli.msg.info.pagination.first.hover", "First page");
        return hasMultiplePages && pageNo != FIRST_PAGE_IDX
                ? buildExecuteCmdComponent(first, firstHover, buildPageCommand(cmd, FIRST_PAGE_IDX), RUN_COMMAND, LINK_COLOR)
                : ComponentUtils.wrapInSquareBrackets(first).withStyle(INACTIVE_LINK_COLOR);
    }

    private static MutableComponent buildPaginationControl(MutableComponent front, MutableComponent back, int pageNo, int maxPage, MutableComponent forward, MutableComponent last) {
        // [<<]  [<]  x/n  [>]  [>>]
        MutableComponent pageIndicator = Component.literal((pageNo + 1) + "/" + (maxPage));
        return Component.translatable(" %s  %s  %s  %s  %s", front, back, pageIndicator, forward, last);
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
        return Component.translatable("%s %s",
                Component.translatableWithFallback("cli.msg.info.region.parent.null", "No parent set"),
                buildExecuteCmdComponent(setParentLinkText, setParentHoverText, setRegionParentCmd, RUN_COMMAND, GREEN));
    }


    public static MutableComponent buildRegionListHeader(IProtectedRegion region) {
        DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(region.getDim());
        return buildHeader(Component.translatableWithFallback("cli.msg.info.header.in", "== %s in %s ==", buildDimRegionsLink(dimCache), buildRegionInfoLink(region)));
    }


    // [n regions] [+]
    public static MutableComponent buildDimRegionsLink(DimensionRegionCache dimCache) {
        DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
        String command = buildCommandStr(DIM.toString(), dimRegion.getDim().location().toString(), LIST.toString(), CommandConstants.LOCAL.toString());
        MutableComponent text = Component.translatableWithFallback("cli.msg.dim.info.region.list.link.text", "%s region(s)", dimCache.getRegions().size());
        MutableComponent hover = Component.translatableWithFallback("cli.msg.dim.info.region.list.link.hover", "List regions in %s", dimRegion.getName());
        MutableComponent listLocalRegionsLink = buildExecuteCmdComponent(text, hover, command, RUN_COMMAND, LINK_COLOR);
        MutableComponent createRegionLink = buildDimCreateRegionLink(dimRegion);
        if (dimRegion.getChildren().isEmpty()) {
            return Component.translatable("%s %s", text, createRegionLink);
        }
        return Component.translatable("%s %s", listLocalRegionsLink, createRegionLink);
    }

    public static MutableComponent buildRegionListChildrenLink(IProtectedRegion region) {
        MutableComponent text = Component.translatableWithFallback("cli.msg.info.region.children.list.link.text", "%s child regions(s)", region.getChildren().size());
        MutableComponent hover = Component.translatableWithFallback("cli.msg.info.region.children.list.link.hover", "List direct child regions of '%s'", region.getName());
        return switch (region.getRegionType()) {
            case GLOBAL -> {
                // [n dimensions(s)]
                Collection<String> dimensionList = RegionDataManager.get().getDimensionList();
                String command = buildCommandStr(GLOBAL.toString(), LIST.toString(), CHILDREN.toString());
                MutableComponent listDimRegionsLinkText = Component.translatableWithFallback("cli.msg.global.info.region.list.link.text", "%s dimensions(s)", dimensionList.size());
                MutableComponent listDimRegionsHoverText = Component.translatableWithFallback("cli.msg.global.info.region.list.link.hover", "List all Dimensional Regions");
                yield buildExecuteCmdComponent(listDimRegionsLinkText, listDimRegionsHoverText, command, RUN_COMMAND, LINK_COLOR);
            }
            case DIMENSION -> {
                // [n children] [+]
                String command = buildCommandStr(DIM.toString(), region.getDim().location().toString(), LIST.toString(), CHILDREN.toString());
                MutableComponent listDimRegionsListLink = buildExecuteCmdComponent(text, hover, command, RUN_COMMAND, LINK_COLOR);
                if (region.getChildren().isEmpty()) {
                    yield Component.translatable("%s %s", text, buildDimCreateRegionLink(region));
                }
                yield Component.translatable("%s %s", listDimRegionsListLink, buildDimCreateRegionLink(region));
            }
            case LOCAL -> {
                // [n children] [+]
                String regionChildrenListLink = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), CHILDREN.toString());
                MutableComponent regionChildrenLink = buildExecuteCmdComponent(text, hover, regionChildrenListLink, RUN_COMMAND, LINK_COLOR);
                MutableComponent addChildrenLink = buildRegionAddChildrenLink(region);
                if (region.getChildren().isEmpty()) {
                    yield Component.translatable("%s %s", text, addChildrenLink);
                }
                yield Component.translatable("%s %s", regionChildrenLink, addChildrenLink);
            }
            default -> throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        };
    }

    public static MutableComponent buildRegionAddChildrenLink(IProtectedRegion region) {
        String addChildrenCmd = buildCommandStr(LOCAL.toString(), region.getDim().location().toString(), region.getName(), ADD.toString(), CHILD.toString(), "");
        MutableComponent addChildrenLinkText = Component.translatableWithFallback("cli.link.add", "+");
        MutableComponent addChildrenHoverText = Component.translatableWithFallback("cli.msg.info.region.children.add.link.hover", "Add child to region %s", region.getName());
        return buildExecuteCmdComponent(addChildrenLinkText, addChildrenHoverText, addChildrenCmd, SUGGEST_COMMAND, ADD_CMD_COLOR);
    }

    public static MutableComponent buildDimCreateRegionLink(IProtectedRegion region) {
        String dimCreateRegionCmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), CREATE.toString(), LOCAL.toString(), "");
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
        MutableComponent responsibleFlagsNumber = buildTextWithHoverMsg(Component.translatable("%s", flagsInHierarchy.size()),
                Component.translatableWithFallback("cli.msg.info.region.flag.responsible.number.hover",
                        "%s responsible flag(s) applicable for %s", flagsInHierarchy.size(), region.getName()), LINK_COLOR);
        MutableComponent responsibleFlagListHoverText = Component.translatableWithFallback("cli.msg.info.region.flag.responsible.link.hover",
                "Show responsible region flags for %s", region.getName());
        switch (region.getRegionType()) {
            case GLOBAL: {
                String flagListCmd = buildCommandStr(GLOBAL.toString(), LIST.toString(), FLAG.toString());
                MutableComponent responsibleFlagListLink = buildExecuteCmdComponent(responsibleFlagsNumber, responsibleFlagListHoverText, flagListCmd, RUN_COMMAND, LINK_COLOR);
                MutableComponent responsibleFlagsComp = flagsInHierarchy.isEmpty() ? responsibleFlagsNumber : responsibleFlagListLink;
                return Component.translatableWithFallback("cli.msg.info.region.flag.responsible.link.text", "%s responsible flag(s)", responsibleFlagsComp);
            }
            case DIMENSION: {
                String flagListCmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), LIST.toString(), FLAG.toString());
                MutableComponent responsibleFlagListLink = buildExecuteCmdComponent(responsibleFlagsNumber, responsibleFlagListHoverText, flagListCmd, RUN_COMMAND, LINK_COLOR);
                MutableComponent responsibleFlagsComp = flagsInHierarchy.isEmpty() ? responsibleFlagsNumber : responsibleFlagListLink;
                return Component.translatableWithFallback("cli.msg.info.region.flag.responsible.link.text", "%s responsible flag(s)", responsibleFlagsComp);
            }
            case LOCAL: {
                String flagListCmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), FLAG.toString());
                MutableComponent responsibleFlagListLink = buildExecuteCmdComponent(responsibleFlagsNumber, responsibleFlagListHoverText, flagListCmd, RUN_COMMAND, LINK_COLOR);
                MutableComponent responsibleFlagsComp = flagsInHierarchy.isEmpty() ? responsibleFlagsNumber : responsibleFlagListLink;
                return Component.translatableWithFallback("cli.msg.info.region.flag.responsible.link.text", "%s responsible flag(s)", responsibleFlagsComp);
            }
            default:
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        }
    }

    public static MutableComponent buildFlagsListLink(IProtectedRegion region) {
        if (region.getRegionType() == RegionType.GLOBAL) {
            return buildRegionFlagListLink(region);
        }
        return Component.translatable("%s | %s", buildResponsibleFlagListLink(region), buildRegionFlagListLink(region));
    }

    // [m] flag(s) [+]
    public static MutableComponent buildRegionFlagListLink(IProtectedRegion region) {
        MutableComponent regionFlagNumber = buildTextWithHoverMsg(Component.translatable("%s", region.getFlags().size()),
                Component.translatableWithFallback("cli.msg.info.region.flag.number.hover", "%s flag(s)", region.getFlags().size(), region.getName()), LINK_COLOR);
        MutableComponent flagListHoverText = Component.translatableWithFallback("cli.msg.info.region.flag.link.hover", "%s flag(s)", region.getName());
        switch (region.getRegionType()) {
            case GLOBAL: {
                String regionFlagListCmd = buildCommandStr(GLOBAL.toString(), LIST.toString(), REGION_FLAG.toString());
                MutableComponent regionFlagListLink = buildExecuteCmdComponent(regionFlagNumber, flagListHoverText, regionFlagListCmd, RUN_COMMAND, LINK_COLOR);
                MutableComponent regionFlagsComp = region.getFlags().isEmpty() ? regionFlagNumber : regionFlagListLink;
                return Component.translatable("%s %s",
                        Component.translatableWithFallback("cli.msg.info.region.flag.region.link.text", "%s flag(s)", regionFlagsComp),
                        buildAddFlagLink(region));
            }
            case DIMENSION: {
                String regionFlagListCmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), LIST.toString(), REGION_FLAG.toString());
                MutableComponent regionFlagListLink = buildExecuteCmdComponent(regionFlagNumber, flagListHoverText, regionFlagListCmd, RUN_COMMAND, LINK_COLOR);
                MutableComponent regionFlagsComp = region.getFlags().isEmpty() ? regionFlagNumber : regionFlagListLink;
                return Component.translatable("%s %s",
                        Component.translatableWithFallback("cli.msg.info.region.flag.region.link.text", "%s flag(s)", regionFlagsComp),
                        buildAddFlagLink(region));
            }
            case LOCAL: {
                String regionFlagListCmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), REGION_FLAG.toString());
                MutableComponent regionFlagListLink = buildExecuteCmdComponent(regionFlagNumber, flagListHoverText, regionFlagListCmd, RUN_COMMAND, LINK_COLOR);
                MutableComponent regionFlagsComp = region.getFlags().isEmpty() ? regionFlagNumber : regionFlagListLink;
                return Component.translatable("%s %s",
                        Component.translatableWithFallback("cli.msg.info.region.flag.region.link.text", "%s flag(s)", regionFlagsComp),
                        buildAddFlagLink(region));
            }
            default:
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        }
    }

    public static MutableComponent buildAddFlagLink(IProtectedRegion region) {
        MutableComponent hoverText = Component.translatableWithFallback("cli.msg.info.region.flag.add.link.hover", "Add new flag to region %s", region.getName());
        MutableComponent linkText = Component.translatableWithFallback("cli.link.add", "+");
        switch (region.getRegionType()) {
            case GLOBAL: {
                String command = buildCommandStr(GLOBAL.toString(), ADD.toString(), FLAG.toString(), "");
                return buildExecuteCmdComponent(linkText, hoverText, command, SUGGEST_COMMAND, ADD_CMD_COLOR);
            }
            case DIMENSION: {
                String command = buildCommandStr(DIM.toString(), region.getDim().location().toString(), ADD.toString(), FLAG.toString(), "");
                return buildExecuteCmdComponent(linkText, hoverText, command, SUGGEST_COMMAND, ADD_CMD_COLOR);
            }
            case LOCAL: {
                String addCmd = buildCommandStr(LOCAL.toString(), region.getDim().location().toString(), region.getName(), ADD.toString(), FLAG.toString(), "");
                return buildExecuteCmdComponent(linkText, hoverText, addCmd, SUGGEST_COMMAND, ADD_CMD_COLOR);
            }
            default:
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        }
    }

    public static MutableComponent buildRegionStateLink(IProtectedRegion region) {
        MutableComponent linkText = Component.translatableWithFallback("cli.msg.info.region.state.link.text", "State");
        MutableComponent hoverText = Component.translatableWithFallback("cli.msg.info.region.state.link.hover", "Show region state for %s", region.getName());
        switch (region.getRegionType()) {
            case GLOBAL: {
                String command = buildCommandStr(GLOBAL.toString(), STATE.toString());
                return buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, LINK_COLOR);
            }
            case DIMENSION: {
                String command = buildCommandStr(DIM.toString(), region.getDim().location().toString(), STATE.toString());
                return buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, LINK_COLOR);
            }
            case LOCAL: {
                String showStateCmd = buildCommandStr(LOCAL.toString(), region.getDim().location().toString(), region.getName(), STATE.toString());
                return buildExecuteCmdComponent(linkText, hoverText, showStateCmd, RUN_COMMAND, LINK_COLOR);
            }
            default:
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        }
    }

    public static MutableComponent buildInfoComponent(String subjectLangKey, String fallback, MutableComponent payload) {
        return Component.translatable("%s: %s", Component.translatable(subjectLangKey, fallback), payload);
    }


    public static String buildExecuteCommandString(ResourceKey<Level> dim, String command) {
        return "/execute in " + dim.location() + " run " + command;
    }

    public static String buildTeleportCmd(ResourceKey<Level> dim, String tpSource, BlockPos target) {
        return buildExecuteCommandString(dim, "tp " + tpSource + " " + buildBlockCoordinateStr(target));
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
        return Component.translatable("%s @ %s", buildRegionInfoLink(region), buildRegionTeleportLink(region, null));
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
        String setTpPosCmd = buildCommandStr(LOCAL.toString(), region.getDim().location().toString(), region.getName(), AREA.toString(), TELEPORT.toString(), SET.toString(), "");
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

    /**
     * @param region    the region to build the link for
     * @param names     the names of the players or teams of the group
     * @param groupType the type of the group (player or team)
     * @param group     the name of the group
     * @return a list of links to remove the group from the region
     */
    public static List<MutableComponent> buildRemoveGroupMemberEntries(IProtectedRegion region, List<String> names, GroupType groupType, String group) {
        return names.stream().sorted().map(name -> buildRemoveGroupEntry(region, name, groupType, group)).collect(Collectors.toList());
    }

    public static MutableComponent buildRemoveGroupEntry(IProtectedRegion region, String name, GroupType groupType, String group) {
        MutableComponent linkText = Component.translatableWithFallback("cli.link.remove", "x");
        MutableComponent hoverText = Component.translatableWithFallback("cli.msg.info.region.group." + groupType.name + ".remove.link.hover", name, region.getName());
        MutableComponent regionRemoveLink;
        if (groupType == GroupType.PLAYER) {
            Player player = ServerLifecycleHooks.getCurrentServer().getPlayerList().getPlayerByName(name);
            boolean isOffline = player == null;
            if (isOffline) {
                MutableComponent offlinePlayerRemoveLink = buildRemoveLinkForOfflinePlayer(region, name, groupType, group, linkText, hoverText);
                return Component.translatable(" - %s %s", offlinePlayerRemoveLink, buildGroupInfo(region, name, groupType));
            }
        }
        regionRemoveLink = switch (region.getRegionType()) {
            case GLOBAL -> {
                String command = buildCommandStr(GLOBAL.toString(), REMOVE.toString(), groupType.name, group, name);
                yield buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, REMOVE_CMD_COLOR);
            }
            case DIMENSION -> {
                String command = buildCommandStr(DIM.toString(), region.getDim().location().toString(), REMOVE.toString(), groupType.name, group, name);
                yield buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, REMOVE_CMD_COLOR);
            }
            case LOCAL -> {
                String command = buildCommandStr(LOCAL.toString(), region.getDim().location().toString(), region.getName(), REMOVE.toString(), groupType.name, group, name);
                yield buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, REMOVE_CMD_COLOR);
            }
            default -> throw new IllegalArgumentException();
        };
        return Component.translatable(" - %s %s", regionRemoveLink, buildGroupInfo(region, name, groupType));
    }

    private static MutableComponent buildRemoveLinkForOfflinePlayer(IProtectedRegion region, String name, GroupType groupType, String group, MutableComponent linkText, MutableComponent hoverText) {
        return switch (region.getRegionType()) {
            case GLOBAL -> {
                String command = buildCommandStr(GLOBAL.toString(), REMOVE.toString(), groupType.name, group, BY_NAME.toString(), name);
                yield buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, REMOVE_CMD_COLOR);
            }
            case DIMENSION -> {
                String command = buildCommandStr(DIM.toString(), region.getDim().location().toString(), REMOVE.toString(), groupType.name, group, BY_NAME.toString(), name);
                yield buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, REMOVE_CMD_COLOR);
            }
            case LOCAL -> {
                String command = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), REMOVE.toString(), groupType.name, group, BY_NAME.toString(), name);
                yield buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, REMOVE_CMD_COLOR);
            }
            default -> throw new IllegalArgumentException();
        };
    }

    public static MutableComponent buildGroupInfo(IProtectedRegion region, String groupMemberName, GroupType groupType) {
        return switch (groupType) {
            case PLAYER -> {
                Player player = ServerLifecycleHooks.getCurrentServer().getPlayerList().getPlayerByName(groupMemberName);
                if (player == null) {
                    yield Component.translatable("%s %s",
                            Component.literal(groupMemberName).withStyle(GRAY),
                            Component.translatableWithFallback("cli.msg.info.player.list.entry.offline", "(offline)"));
                } else {
                    yield buildPlayerHoverComponent(player);
                }

            }
            case TEAM -> {
                ServerLevel level = ServerLifecycleHooks.getCurrentServer().getLevel(region.getDim());
                if (level != null) {
                    Team team = level.getScoreboard().getPlayerTeam(groupMemberName);
                    yield team == null ? Component.literal(groupMemberName) : buildTeamHoverComponent(team);
                } else {
                    yield Component.literal(groupMemberName);
                }
            }
        };
    }

    public static List<String> getGroupList(IProtectedRegion region, String group, GroupType groupType) {
        switch (groupType) {
            case PLAYER:
                return region.getGroup(group).getPlayers().values().stream().sorted().collect(Collectors.toList());
            case TEAM:
                return region.getGroup(group).getTeams().stream().sorted().collect(Collectors.toList());
            default:
                return new ArrayList<>();
        }
    }


    // /wp local minecraft:overworld house remove child oven
    // /wp local minecraft:overworld <parent> remove child <child>
    public static MutableComponent buildRegionRemoveChildLink(IProtectedRegion region, IProtectedRegion child) {
        String command = buildCommandStr(LOCAL.toString(), region.getDim().location().toString(), region.getName(), REMOVE.toString(), CHILD.toString(), child.getName());
        MutableComponent linkText = Component.translatableWithFallback("cli.link.remove", "x");
        MutableComponent linkHoverText = Component.translatableWithFallback("cli.msg.info.region.children.remove.link.hover", "Remove child '%s' from region %s", child.getName(), region.getName());
        return buildExecuteCmdComponent(linkText, linkHoverText, command, SUGGEST_COMMAND, REMOVE_CMD_COLOR);
    }

    public static MutableComponent buildRegionActionUndoLink(String cmd, CommandConstants toReplace, CommandConstants replacement) {
        String revertCmd = ArgumentUtil.revertCommand(cmd, toReplace, replacement);
        MutableComponent revertLinkText = Component.translatableWithFallback("cli.link.action.undo.text", "←");
        MutableComponent revertLinkHover = Component.translatableWithFallback("cli.link.action.undo.hover", "Undo action.");
        return buildExecuteCmdComponent(revertLinkText, revertLinkHover, revertCmd, RUN_COMMAND, DARK_RED);
    }

    public static MutableComponent buildRegionActionUndoLink(String cmd, String toReplace, String replacement) {
        String revertCmd = ArgumentUtil.revertCommand(cmd, toReplace, replacement);
        MutableComponent revertLinkText = Component.translatableWithFallback("cli.link.action.undo.text", "←");
        MutableComponent revertLinkHover = Component.translatableWithFallback("cli.link.action.undo.hover", "Undo action.");
        return buildExecuteCmdComponent(revertLinkText, revertLinkHover, revertCmd, RUN_COMMAND, DARK_RED);
    }

    public static MutableComponent buildRegionFlagInfoHeader(IProtectedRegion region, MutableComponent flagListLink) {
        return buildHeader(Component.translatableWithFallback("cli.msg.info.header.in", "== %s in %s ==", flagListLink, buildRegionInfoLink(region)));
    }

    public static MutableComponent buildFlagInfoHeader(IProtectedRegion region, IFlag flag) {
        return buildHeader(Component.translatableWithFallback("cli.msg.info.header.flag.in", "== Flag %s in %s ==", buildFlagInfoLink(region, flag), buildRegionInfoLink(region)));
    }
}
