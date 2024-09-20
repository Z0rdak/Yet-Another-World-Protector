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
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.group.GroupType;
import de.z0rdak.yawp.core.group.PlayerContainer;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.core.region.RegionType;
import de.z0rdak.yawp.handler.flags.FlagCorrelation;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.entity.EntityType;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.NbtHelper;
import net.minecraft.registry.RegistryKey;
import net.minecraft.scoreboard.Team;
import net.minecraft.server.command.ServerCommandSource;
import net.minecraft.server.world.ServerWorld;
import net.minecraft.text.*;
import net.minecraft.util.Formatting;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Direction;
import net.minecraft.world.World;
import org.apache.commons.lang3.NotImplementedException;

import java.util.*;
import java.util.stream.Collectors;

import static com.ibm.icu.impl.ValidIdentifiers.Datatype.region;
import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.commands.CommandUtil.GROUP_LIST;
import static de.z0rdak.yawp.commands.arguments.ArgumentUtil.appendSubCommand;
import static de.z0rdak.yawp.commands.arguments.ArgumentUtil.buildCommandStr;
import static de.z0rdak.yawp.commands.arguments.ArgumentUtil.buildSubCmdStr;
import static de.z0rdak.yawp.config.server.RegionConfig.CLI_REGION_DEFAULT_PRIORITY_INC;
import static de.z0rdak.yawp.core.region.RegionType.LOCAL;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.getFlagMapRecursive;
import static de.z0rdak.yawp.util.LocalRegions.getFlagsWithState;
import static net.minecraft.text.ClickEvent.Action.*;
import static net.minecraft.util.Formatting.RESET;
import static net.minecraft.util.Formatting.*;


public class ChatComponentBuilder {

    public final static Formatting SUGGEST_COLOR = BLUE;
    public final static Formatting TP_COLOR = GREEN;
    public final static Formatting LINK_COLOR = AQUA;
    public final static Formatting INACTIVE_LINK_COLOR = GRAY;
    public final static Formatting ADD_CMD_COLOR = DARK_GREEN;
    public final static Formatting REMOVE_CMD_COLOR = DARK_RED;
    public static int FIRST_PAGE_IDX = 0;
    private ChatComponentBuilder() {
    }

    public static MutableText buildHeader(String translationKey) {
        return buildHeader(Text.translatable(translationKey));
    }

    public static MutableText buildHeader(String translationKey, String fallback) {
        return buildHeader(Text.translatableWithFallback(translationKey, fallback));
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

    public static MutableText buildHeader(MutableText header) {
        return Text.translatable("%s %s %s", Formatting.BOLD, header, Formatting.BOLD);
    }

    public static MutableText buildExecuteCmdComponent(MutableText linkText, MutableText hoverText, String command, ClickEvent.Action eventAction, Formatting color) {
        MutableText text = Texts.bracketed(linkText);
        return text.setStyle(text.getStyle()
                .withColor(color)
                .withClickEvent(new ClickEvent(eventAction, command))
                .withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, hoverText)));
    }

    public static MutableText buildPlayerHoverComponent(PlayerEntity player) {
        HoverEvent.EntityContent entityTooltipInfo = new HoverEvent.EntityContent(EntityType.PLAYER, player.getUuid(), player.getName());
        MutableText playerName = Text.literal(player.getEntityName());
        playerName.setStyle(playerName.getStyle()
                .withColor(LINK_COLOR)
                .withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_ENTITY, entityTooltipInfo))
                .withClickEvent(new ClickEvent(SUGGEST_COMMAND, "/tell " + playerName.getString() + " ")));
        return playerName;
    }

    public static MutableText buildTeamHoverComponent(Team team) {
        MutableText playerName = Text.literal(team.getName());
        playerName.setStyle(playerName.getStyle()
                .withColor(LINK_COLOR)
                .withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, Text.translatableWithFallback("cli.msg.info.region.group.link.hover", "Click to display team info")))
                .withClickEvent(new ClickEvent(RUN_COMMAND, "/team list " + team.getName())));
        return playerName;
    }

    /***
     * [X,Y,Z], ..., [X,Y,Z]
     */
    public static MutableText buildAreaMarkedBlocksTpLinks(IMarkableRegion region) {
        List<MutableText> tpLinks = region.getArea().getMarkedBlocks()
                .stream()
                .map(pos -> buildDimensionalBlockTpLink(region.getDim(), pos))
                .collect(Collectors.toList());
        MutableText blockPosTpLinkList = Text.literal("");
        tpLinks.forEach(tpLink -> blockPosTpLinkList.append(tpLink).append(" "));
        return blockPosTpLinkList;
    }

    public static MutableText buildRegionAreaDetailComponent(IMarkableRegion region) {
        IMarkableArea area = region.getArea();
        MutableText areaInfo = Text.literal(area.getAreaType().areaType);
        switch (area.getAreaType()) {
            case CUBOID:
                return Text.translatable("%s, %s", areaInfo, buildCuboidAreaInfo((CuboidArea) area));
            case CYLINDER:
                throw new NotImplementedException("cylinder");
            case SPHERE:
                return Text.translatable("%s, %s", areaInfo, buildSphereAreaInfo((SphereArea) area));
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
    private static MutableText buildCuboidAreaInfo(CuboidArea cuboidArea) {
        return Text.translatableWithFallback("cli.msg.info.region.area.area.size.text.cuboid", "Size: %s %s %s",
                buildAreaAxisInfoComponent(cuboidArea, Direction.Axis.X),
                buildAreaAxisInfoComponent(cuboidArea, Direction.Axis.Y),
                buildAreaAxisInfoComponent(cuboidArea, Direction.Axis.Z));

    }

    /**
     * Center: [X,Y,Z], Radius: 5, Diameter: 11
     */
    private static MutableText buildSphereAreaInfo(SphereArea sphereArea) {
        int diameter = (sphereArea.getRadius() * 2) + 1;
        MutableText centerPos = Text.literal(buildBlockPosLinkText(sphereArea.getCenterPos()));
        return Text.translatableWithFallback("cli.msg.info.region.area.area.size.text.sphere", "Center: %s, Radius: %s, Diameter: %s",
                buildTextWithHoverAndBracketsMsg(centerPos, centerPos, WHITE), sphereArea.getRadius(), diameter);
    }

    /**
     * Builds component showing size of the area for the given axis with a hover text displaying the block range of the axis.
     * Axis=N, e.g. X=5
     */
    private static MutableText buildAreaAxisInfoComponent(CuboidArea cuboidArea, Direction.Axis axis) {
        int min = (int) Math.floor(cuboidArea.getArea().getMin(axis));
        int max = (int) Math.floor(cuboidArea.getArea().getMax(axis));
        int axisSize = Math.max(Math.abs(max - min), 1);
        String axisName = axis.getName().toUpperCase();
        return buildTextWithHoverAndBracketsMsg(
                Text.literal(axisName + "=" + axisSize),
                Text.literal(axisName + ": " + min + " - " + max), WHITE);
    }

    /**
     * [<=expand=>] [<=max=>]
     */
    public static MutableText buildRegionAreaExpandLink(IMarkableRegion region) {
        MutableText linkText = Text.translatableWithFallback("cli.msg.info.region.area.area.expand.link.text", "<=expand=>");
        MutableText linkHover = Text.translatableWithFallback("cli.msg.info.region.area.area.expand.link.hover", "Expand the area for '%s'", region.getName());
        String expandCmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().getValue().toString(), region.getName(), AREA.toString(), EXPAND.toString(), region.getArea().getAreaType().areaType);
        switch (region.getArea().getAreaType()) {
            case CUBOID: {
                CuboidArea cuboidArea = (CuboidArea) region.getArea();
                int areaLowerLimit = (int) Math.floor(cuboidArea.getArea().minZ);
                int areaUpperLimit = (int) Math.floor(cuboidArea.getArea().maxZ);
                // [<=expand=>]
                String expandCmdSuggestion = appendSubCommand(expandCmd, String.valueOf(areaLowerLimit), String.valueOf(areaUpperLimit));
                MutableText expandLink = buildExecuteCmdComponent(linkText, linkHover, expandCmdSuggestion, SUGGEST_COMMAND, LINK_COLOR);
                // [<=max=>]
                MutableText maxExpandLinkText = Text.translatableWithFallback("cli.msg.info.region.area.area.expand-max.link.text", "<=max=>");
                MutableText maxExpandLinkHover = Text.translatableWithFallback("cli.msg.info.region.area.area.expand-max.link.hover", "Expand area to build limit");
                String maxExpandCmd = appendSubCommand(expandCmd, String.valueOf(RegionCommands.MIN_BUILD_LIMIT), String.valueOf(RegionCommands.MAX_BUILD_LIMIT));
                MutableText maxExpandLink = buildExecuteCmdComponent(maxExpandLinkText, maxExpandLinkHover, maxExpandCmd, RUN_COMMAND, LINK_COLOR);
                return Text.translatable("%s %s", expandLink, maxExpandLink);
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

    private static MutableText buildShowAreaToggleLink(IMarkableRegion region) {
        MutableText showAreaLinkText = Text.translatableWithFallback("cli.msg.info.region.area.area.show.link", "Show");
        MutableText showAreaLinkHover = Text.translatableWithFallback("cli.msg.info.region.area.area.show.hover", "Toggle visible bounding box of '%s'", region.getName());
        String showAreaCmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().getValue().toString(), region.getName(), AREA.toString(), "show");
        return buildExecuteCmdComponent(showAreaLinkText, showAreaLinkHover, showAreaCmd, RUN_COMMAND, LINK_COLOR);
    }

    /**
     * [set area]
     */
    public static MutableText buildAreaUpdateLink(IMarkableRegion region) {
        MutableText setAreaLinkText = Text.translatableWithFallback("cli.msg.info.region.area.area.set.link", "set area");
        MutableText setAreaLinkHover = Text.translatableWithFallback("cli.msg.info.region.area.area.set.hover", "Update area of region '%s'", region.getName());
        String blocks = String.join(" ", region.getArea().getMarkedBlocks().stream()
                .map(ChatComponentBuilder::buildBlockCoordinateStr)
                .collect(Collectors.toSet()));
        String setAreaCmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().getValue().toString(), region.getName(), AREA.toString(), SET.toString(), region.getArea().getAreaType().areaType, blocks);
        return buildExecuteCmdComponent(setAreaLinkText, setAreaLinkHover, setAreaCmd, SUGGEST_COMMAND, LINK_COLOR);
    }

    /**
     * [set area] [set TP] [show area] [<=expand=>] [<=max=>]
     */
    public static MutableText buildRegionAreaActionLinks(IMarkableRegion region) {
        return Text.translatable("%s %s %s", buildAreaUpdateLink(region), buildRegionSetTpLink(region), buildRegionAreaExpandLink(region));
        // buildShowAreaToggleLink(region)
    }

    public static MutableText buildTextWithHoverAndBracketsMsg(MutableText text, MutableText hoverText, Formatting color) {
        MutableText bracketedText = Texts.bracketed(text);
        bracketedText.setStyle(bracketedText.getStyle().withColor(color).withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, hoverText)));
        return bracketedText;
    }

    public static MutableText buildTextWithHoverMsg(MutableText text, MutableText hoverText, Formatting color) {
        text.setStyle(text.getStyle().withColor(color).withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, hoverText)));
        return text;
    }

    public static MutableText buildHelpStartComponent() {
        String command = buildCommandStr(CommandConstants.GLOBAL.toString(), CommandConstants.INFO.toString());
        MutableText text = Text.translatableWithFallback("help.hint.link.text", "Start here");
        MutableText hover = Text.translatableWithFallback("help.hint.link.hover", "Use '/%s global info' as a starting point to manage the global region", CommandPermissionConfig.BASE_CMD);
        return buildExecuteCmdComponent(text, hover, command, ClickEvent.Action.RUN_COMMAND, LINK_COLOR);
    }

    public static MutableText buildWikiLink() {
        MutableText wikiLinkHover = Text.translatableWithFallback("help.tooltip.wiki.link.hover", "https://github.com/Z0rdak/Yet-Another-World-Protector/wiki");
        MutableText wikiLink = Text.translatableWithFallback("help.tooltip.wiki.link.text", "Open Wiki in default browser");
        return buildExecuteCmdComponent(wikiLink, wikiLinkHover, "https://github.com/Z0rdak/Yet-Another-World-Protector/wiki", OPEN_URL, AQUA);
    }

    public static MutableText buildAllLocalEnableComponent(DimensionRegionCache dimCache) {
        MutableText enableLinkTextKey = Text.translatableWithFallback("cli.msg.info.region.state.alert.all.true.link.text", "all-on");
        MutableText enableHoverTextKey = Text.translatableWithFallback("cli.msg.info.region.state.alert.all.true.link.hover", "Activates all local regions of %s", dimCache.getDimensionalRegion().getName());
        MutableText disableLinkTextKey = Text.translatableWithFallback("cli.msg.info.region.state.alert.all.false.link.text", "all-off");
        MutableText disableHoverTextKey = Text.translatableWithFallback("cli.msg.info.region.state.alert.all.false.link.hover", "Disables all local regions of %s", dimCache.getDimensionalRegion().getName());
        String enableCmd = buildCommandStr(DIM.toString(), dimCache.getDimensionalRegion().getName(), STATE.toString(), ENABLE_LOCAL.toString(), Boolean.TRUE.toString());
        String disableCmd = buildCommandStr(DIM.toString(), dimCache.getDimensionalRegion().getName(), STATE.toString(), ENABLE_LOCAL.toString(), Boolean.FALSE.toString());
        MutableText activeAlertLink = buildExecuteCmdComponent(enableLinkTextKey, enableHoverTextKey, enableCmd, RUN_COMMAND, GREEN);
        MutableText disableAlertLink = buildExecuteCmdComponent(disableLinkTextKey, disableHoverTextKey, disableCmd, RUN_COMMAND, RED);
        return Text.translatableWithFallback("%s %s", "%s %s", activeAlertLink, disableAlertLink);
    }

    public static MutableText buildAllLocalAlertToggleLink(DimensionRegionCache dimCache) {
        MutableText enableLinkTextKey = Text.translatableWithFallback("cli.msg.info.region.state.enable.all.true.link.text", "all-on");
        MutableText enableHoverTextKey = Text.translatableWithFallback("cli.msg.info.region.state.enable.all.true.link.hover", "Enables alert for all local regions of %s", dimCache.getDimensionalRegion().getName());
        MutableText disableLinkTextKey = Text.translatableWithFallback("cli.msg.info.region.state.enable.all.false.link.text", "all-off");
        MutableText disableHoverTextKey = Text.translatableWithFallback("cli.msg.info.region.state.enable.all.false.link.hover", "Disables alert for all local regions of %s", dimCache.getDimensionalRegion().getName());
        String enableCmd = buildCommandStr(DIM.toString(), dimCache.getDimensionalRegion().getName(), STATE.toString(), ALERT_LOCAL.toString(), Boolean.TRUE.toString());
        String disableCmd = buildCommandStr(DIM.toString(), dimCache.getDimensionalRegion().getName(), STATE.toString(), ALERT_LOCAL.toString(), Boolean.FALSE.toString());
        MutableText activeAlertLink = buildExecuteCmdComponent(enableLinkTextKey, enableHoverTextKey, enableCmd, RUN_COMMAND, GREEN);
        MutableText disableAlertLink = buildExecuteCmdComponent(disableLinkTextKey, disableHoverTextKey, disableCmd, RUN_COMMAND, RED);
        return Text.translatableWithFallback("%s %s", "%s %s", activeAlertLink, disableAlertLink);
    }

    public static MutableText buildRegionEnableComponent(IProtectedRegion region) {
        String linkTextKey = "cli.msg.info.region.state.enable." + region.isActive() + ".link.text";
        String linkFallback = region.isActive() ? "yes" : "no";
        String hoverTextKey = "cli.msg.info.region.state.enable." + !region.isActive() + ".link.hover";
        String hoverFallback = !region.isActive() ? "Enable flag checks" : "Disable flag checks";
        MutableText linkText = Text.translatableWithFallback(linkTextKey, linkFallback);
        MutableText hoverText = Text.translatableWithFallback(hoverTextKey, hoverFallback);
        Formatting color = region.isActive() ? GREEN : GRAY;
        return switch (region.getRegionType()) {
            case GLOBAL -> {
                String cmd = buildCommandStr(GLOBAL.toString(), STATE.toString(), ENABLE.toString());
                yield buildExecuteCmdComponent(linkText, hoverText, cmd, ClickEvent.Action.RUN_COMMAND, color);
            }
            case DIMENSION -> {
                String cmd = buildCommandStr(DIM.toString(), region.getDim().getValue().toString(), STATE.toString(), ENABLE.toString());
                yield buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, color);
            }
            case LOCAL -> {
                String cmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().getValue().toString(), region.getName(), STATE.toString(), ENABLE.toString());
                yield buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, color);
            }
            default -> throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        };
    }

    public static MutableText buildRegionPriorityComponent(IMarkableRegion region) {
        int defaultPriorityInc = RegionConfig.getDefaultPriorityInc();
        String incPriorityCmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().getValue().toString(), region.getName(), STATE.toString(), PRIORITY.toString(), INC.toString(), String.valueOf(defaultPriorityInc));
        MutableText incLinkText = Text.translatableWithFallback("cli.msg.info.region.state.priority.increase.link.text", "+%s", defaultPriorityInc);
        MutableText incHoverText = Text.translatableWithFallback("cli.msg.info.region.state.priority.increase.link.hover", "Increase region priority by %s", defaultPriorityInc);
        MutableText increaseLink = buildExecuteCmdComponent(incLinkText, incHoverText, incPriorityCmd, RUN_COMMAND, ADD_CMD_COLOR);
        String decPriorityCmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().getValue().toString(), region.getName(), STATE.toString(), PRIORITY.toString(), DEC.toString(), String.valueOf(defaultPriorityInc));
        MutableText decLinkText = Text.translatableWithFallback("cli.msg.info.region.state.priority.decrease.link.text", "+%s", defaultPriorityInc);
        MutableText decHoverText = Text.translatableWithFallback("cli.msg.info.region.state.priority.decrease.link.hover", "Decrease region priority by %s", defaultPriorityInc);
        MutableText decreaseLink = buildExecuteCmdComponent(decLinkText, decHoverText, decPriorityCmd, RUN_COMMAND, REMOVE_CMD_COLOR);
        String setPriorityCmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().getValue().toString(), region.getName(), STATE.toString(), PRIORITY.toString(), "");
        MutableText setPriorityLinkText = Text.translatableWithFallback("cli.msg.info.region.state.priority.set.link.text", "%s", region.getPriority());
        MutableText setPriorityHoverText = Text.translatableWithFallback("cli.msg.info.region.state.priority.set.link.hover", "Set priority for region");
        MutableText setPriorityLink = buildExecuteCmdComponent(setPriorityLinkText, setPriorityHoverText, setPriorityCmd, SUGGEST_COMMAND, LINK_COLOR);
        return Text.translatable("%s %s %s", setPriorityLink, increaseLink, decreaseLink);
    }

    public static MutableText buildRegionAlertToggleLink(IProtectedRegion region) {
        String linkTextKey = "cli.msg.info.region.state.alert." + !region.isMuted() + ".link.text";
        String hoverTextKey = "cli.msg.info.region.state.alert." + region.isMuted() + ".link.hover";
        String linkFallback = region.isMuted() ? "off" : "on";
        String hoverFallback = !region.isMuted() ? "Turn flag alerts off" : "Turn flag alerts on";
        MutableText linkText = Text.translatableWithFallback(linkTextKey, linkFallback);
        MutableText hoverText = Text.translatableWithFallback(hoverTextKey, hoverFallback);
        Formatting color = region.isMuted() ? GRAY : GREEN;
        return switch (region.getRegionType()) {
            case GLOBAL -> {
                String cmd = buildCommandStr(GLOBAL.toString(), STATE.toString(), ALERT.toString());
                yield buildExecuteCmdComponent(linkText, hoverText, cmd, ClickEvent.Action.RUN_COMMAND, color);
            }
            case DIMENSION -> {
                String cmd = buildCommandStr(DIM.toString(), region.getDim().getValue().toString(), STATE.toString(), ALERT.toString());
                yield buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, color);
            }
            case LOCAL -> {
                String cmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().getValue().toString(), region.getName(), STATE.toString(), ALERT.toString());
                yield buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, color);
            }
            default -> throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        };
    }

    public static MutableText buildRegionRenameLink(IProtectedRegion region) {
        String cmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().getValue().toString(), region.getName(), RENAME.toString(), "");
        MutableText text = Text.translatableWithFallback("cli.msg.info.region.state.rename.link.text", "rename");
        MutableText hover = Text.translatableWithFallback("cli.msg.info.region.state.rename.link.hover", "Rename region '%s'", region.getName());
        return buildExecuteCmdComponent(text, hover, cmd, SUGGEST_COMMAND, LINK_COLOR);
    }

    public static MutableText buildRegionInfoLink(IProtectedRegion region) {
        return buildRegionInfoLink(region, Text.translatableWithFallback("cli.msg.info.region.link.hover", "Show region info for %s", region.getName()));
    }

    public static MutableText buildRegionInfoLink(IProtectedRegion region, MutableText linkText, MutableText hoverText) {
        return switch (region.getRegionType()) {
            case GLOBAL -> {
                String command = buildCommandStr(GLOBAL.toString(), INFO.toString());
                yield buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, LINK_COLOR);
            }
            case DIMENSION -> {
                String command = buildCommandStr(DIM.toString(), region.getDim().getValue().toString(), INFO.toString());
                yield buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, LINK_COLOR);
            }
            case LOCAL -> {
                String cmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().getValue().toString(), region.getName(), INFO.toString());
                yield buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            default -> throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        };
    }

    public static MutableText buildRegionInfoLink(IProtectedRegion region, MutableText hoverText) {
        MutableText linkText = Text.literal(region.getName());
        return buildRegionInfoLink(region, linkText, hoverText);
    }

    public static MutableText buildRegionAreaLink(IMarkableRegion region) {
        String showSpatialPropLink = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().getValue().toString(), region.getName(), AREA.toString());
        MutableText spatialPropLinkText = Text.translatableWithFallback("cli.msg.info.region.area.link.text", "Area Properties");
        MutableText spatialPropHoverText = Text.translatableWithFallback("cli.msg.info.region.area.link.hover", "Show region area properties for %s", region.getName());
        return buildExecuteCmdComponent(spatialPropLinkText, spatialPropHoverText, showSpatialPropLink, RUN_COMMAND, LINK_COLOR);
    }

    public static MutableText buildRegionOverviewHeader(IProtectedRegion region) {
        switch (region.getRegionType()) {
            case GLOBAL: {
                MutableText dumpLinkText = Text.translatableWithFallback("cli.msg.global.overview.header.dump.link.text", "Global overview");
                MutableText dumpLinkHover = Text.translatableWithFallback("cli.msg.global.overview.header.dump.link.hover", "Copy Global Region NBT to clipboard");
                MutableText clipBoardDumpLink = buildExecuteCmdComponent(dumpLinkText, dumpLinkHover, NbtHelper.toPrettyPrintedText(region.serializeNBT()).getString(), ClickEvent.Action.COPY_TO_CLIPBOARD, GOLD);
                return buildHeader(Text.translatableWithFallback("cli.msg.info.header.for", "== %s for %s ==", clipBoardDumpLink, buildRegionInfoLink(region)));
            }
            case DIMENSION: {
                MutableText dumpLinkText = Text.translatableWithFallback("cli.msg.dim.overview.header.dump.link.text", "Dimension overview");
                MutableText dumpLinkHover = Text.translatableWithFallback("cli.msg.dim.overview.header.dump.link.hover", "Copy Dimensional Region NBT to clipboard");
                MutableText clipBoardDumpLink = buildExecuteCmdComponent(dumpLinkText, dumpLinkHover, NbtHelper.toPrettyPrintedText(region.serializeNBT()).getString(), ClickEvent.Action.COPY_TO_CLIPBOARD, GOLD);
                return buildHeader(Text.translatableWithFallback("cli.msg.info.header.for", "== %s for %s ==", clipBoardDumpLink, buildRegionInfoLink(region)));
            }
            case LOCAL: {
                MutableText dumpLinkText = Text.translatableWithFallback("cli.msg.local.overview.header.dump.link.text", "Region overview");
                MutableText dumpLinkHover = Text.translatableWithFallback("cli.msg.local.overview.header.dump.link.hover", "Copy Local Region NBT to clipboard");
                MutableText clipBoardDumpLink = buildExecuteCmdComponent(dumpLinkText, dumpLinkHover, NbtHelper.toPrettyPrintedText(region.serializeNBT()).getString(), ClickEvent.Action.COPY_TO_CLIPBOARD, GOLD);
                return buildHeader(Text.translatableWithFallback("cli.msg.info.header.for", "== %s for %s ==", clipBoardDumpLink, buildRegionInfoLink(region)));
            }
            default:
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        }
    }

    /**
     * Players: [n player(s)] [+]
     * // TODO:
     */
    public static MutableText buildPlayerListLink(IProtectedRegion region, PlayerContainer players, String group) {
        MutableText hoverText = Text.translatableWithFallback("cli.msg.info.region.group.player.list.link.hover", "List players of group '%s' in region %s", group, region.getName());
        MutableText linkText = Text.translatableWithFallback("cli.msg.info.region.group.player.list.link.text", "%s player(s)", players.getPlayers().size());
        return switch (region.getRegionType()) {
            case GLOBAL -> {
                String cmd = buildCommandStr(GLOBAL.toString(), LIST.toString(), GROUP.toString(), group, PLAYER.toString());
                yield buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            case DIMENSION -> {
                String cmd = buildCommandStr(DIM.toString(), region.getDim().getValue().toString(), LIST.toString(), GROUP.toString(), group, PLAYER.toString());
                yield buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            case LOCAL -> {
                String cmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().getValue().toString(), region.getName(), LIST.toString(), GROUP.toString(), group, PLAYER.toString());
                yield buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            default -> throw new IllegalArgumentException();
        };
    }

    /**
     * Teams: [n team(s)] [+]
     * // TODO:
     */
    public static MutableText buildTeamListLink(IProtectedRegion region, PlayerContainer teams, String group) {
        MutableText hoverText = Text.translatableWithFallback("cli.msg.info.region.group.team.list.link.hover", "List teams of group '%s' in region %s", group, region.getName());
        MutableText linkText = Text.translatableWithFallback("cli.msg.info.region.group.team.list.link.text", "%s team(s)", teams.getTeams().size());
        switch (region.getRegionType()) {
            case GLOBAL: {
                String cmd = buildCommandStr(GLOBAL.toString(), LIST.toString(), GROUP.toString(), group, TEAM.toString());
                return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            case DIMENSION: {
                String cmd = buildCommandStr(DIM.toString(), region.getDim().getValue().toString(), LIST.toString(), GROUP.toString(), group, TEAM.toString());
                return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            case LOCAL: {
                String cmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().getValue().toString(), region.getName(), LIST.toString(), GROUP.toString(), group, TEAM.toString());
                return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            default:
                throw new IllegalArgumentException();
        }
    }

    public static MutableText buildAddToGroupLink(IProtectedRegion region, String group, GroupType groupType) {
        MutableText linkText = Text.translatableWithFallback("cli.link.add", "+");
        String fallback = "Add " + groupType.name + " as '%s' to region %s";
        MutableText hoverText = Text.translatableWithFallback("cli.msg.info.region.group." + groupType.name + ".add.link.hover", fallback, group, region.getName());
        String subCmd = buildSubCmdStr(ADD.toString(), groupType.name, group, "");
        switch (region.getRegionType()) {
            case GLOBAL: {
                String cmd = buildCommandStr(GLOBAL.toString(), subCmd);
                return buildExecuteCmdComponent(linkText, hoverText, cmd, SUGGEST_COMMAND, ADD_CMD_COLOR);
            }
            case DIMENSION: {
                String cmd = buildCommandStr(DIM.toString(), region.getDim().getValue().toString(), subCmd);
                return buildExecuteCmdComponent(linkText, hoverText, cmd, SUGGEST_COMMAND, ADD_CMD_COLOR);
            }
            case LOCAL: {
                String cmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().getValue().toString(), region.getName(), subCmd);
                return buildExecuteCmdComponent(linkText, hoverText, cmd, SUGGEST_COMMAND, ADD_CMD_COLOR);
            }
            default:
                throw new IllegalArgumentException();
        }
    }

    public static MutableText buildGroupLinks(IProtectedRegion region) {
        return getGroupsForRegion(region).stream()
                .map(group -> buildGroupLink(region, group, getGroupSize(region, group)))
                .reduce(Text.literal(""), (link1, link2) -> link1.append(" ").append(link2));
    }

    public static List<String> getGroupsForRegion(IProtectedRegion region) {
        return GROUP_LIST;
    }

    private static int getGroupSize(IProtectedRegion region, String groupName) {
        PlayerContainer group = region.getGroup(groupName);
        return group.getPlayers().size() + group.getTeams().size();
    }

    public static MutableText buildGroupHeader(IProtectedRegion region, String group) {
        MutableText groupLink = buildGroupLink(region, group, getGroupSize(region, group));
        return buildHeader(Text.translatableWithFallback("cli.msg.info.header.in", "== %s in %s ==", groupLink, buildRegionInfoLink(region)));
    }

    public static MutableText buildGroupHeader(IProtectedRegion region, String group, GroupType groupType) {
        String fallback = "== Region '%s' " + groupType.name + " in %s ==";
        return Text.translatableWithFallback("cli.msg.info.region.group." + groupType.name + ".list", fallback, buildRegionInfoLink(region), group);
    }

    public static MutableText buildGroupLink(IProtectedRegion region, String group, int groupSize) {
        MutableText linkText = Text.translatableWithFallback("cli.msg.info.region.group.list.link.text", "%s %s(s)", groupSize, group);
        MutableText hoverText = Text.translatableWithFallback("cli.msg.info.region.group.list.link.hover", "List '%s' for region %s", group, region.getName());
        switch (region.getRegionType()) {
            case GLOBAL: {
                String cmd = buildCommandStr(GLOBAL.toString(), LIST.toString(), CommandConstants.GROUP.toString(), group);
                return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            case LOCAL: {
                String cmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().getValue().toString(), region.getName(), LIST.toString(), CommandConstants.GROUP.toString(), group);
                return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            case DIMENSION: {
                String cmd = buildCommandStr(DIM.toString(), region.getDim().getValue().toString(), LIST.toString(), CommandConstants.GROUP.toString(), group);
                return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            default:
                throw new IllegalArgumentException();
        }
    }

    public static MutableText buildGroupTeamListLink(IProtectedRegion region, String group) {
        // Teams: [n team(s)] [+]
        PlayerContainer playerContainer = region.getGroup(group);
        MutableText teamAddLink = buildAddToGroupLink(region, group, GroupType.TEAM);
        MutableText teamListLink = playerContainer.hasTeams()
                ? buildTeamListLink(region, playerContainer, group)
                : Text.translatableWithFallback("cli.msg.info.region.group.team.list.link.text", "%s team(s)", playerContainer.getTeams().size());
        return Text.translatableWithFallback("%s: %s %s", "%s: %s %s",
                Text.translatableWithFallback("cli.msg.info.region.group.team", "Teams"),
                teamListLink,
                teamAddLink);
    }

    public static MutableText buildGroupPlayerListLink(IProtectedRegion region, String group) {
        // Players: [n player(s)] [+]
        PlayerContainer playerContainer = region.getGroup(group);
        MutableText playersAddLink = buildAddToGroupLink(region, group, GroupType.PLAYER);
        MutableText playerListLink = playerContainer.hasPlayers()
                ? buildPlayerListLink(region, playerContainer, group)
                : Text.translatableWithFallback("cli.msg.info.region.group.player.list.link.text", "%s player(s)", playerContainer.getPlayers().size());
        return Text.translatableWithFallback("%s: %s %s", "%s: %s %s",
                Text.translatableWithFallback("cli.msg.info.region.group.player", "Players"),
                playerListLink,
                playersAddLink);
    }

    /**
     * Creates a TextComponent for flag removal, followed by the flag infos
     *
     * @return - [x] [flagname] [<region-indicator] [] []
     */
    private static MutableText buildRemoveFlagEntry(IProtectedRegion region, IFlag flag, Formatting flagLinkColor, Formatting... formattings) {
        String cmd;
        switch (region.getRegionType()) {
            case GLOBAL: {
                cmd = buildCommandStr(CommandConstants.GLOBAL.toString(), REMOVE.toString(), FLAG.toString(), flag.getName());
                break;
            }
            case DIMENSION: {
                cmd = buildCommandStr(CommandConstants.DIM.toString(), region.getDim().getValue().toString(), REMOVE.toString(), FLAG.toString(), flag.getName());
                break;
            }
            case LOCAL: {
                cmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().getValue().toString(), region.getName(), REMOVE.toString(), FLAG.toString(), flag.getName());
                break;
            }
            default:
                throw new IllegalArgumentException();
        }
        MutableText hoverText = Text.translatableWithFallback("cli.msg.info.region.flag.remove.link.hover", "Remove flag '%s' from region %s", flag.getName(), region.getName());
        MutableText linkText = Text.translatableWithFallback("cli.link.remove", "x");
        MutableText flagRemoveLink = buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, REMOVE_CMD_COLOR);
        MutableText flagQuickActionComponent = buildFlagQuickActionComponent(region, flag, flagLinkColor);
        flagQuickActionComponent.formatted(formattings);
        return Text.translatable(" - %s %s", flagRemoveLink, flagQuickActionComponent);
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
    private static MutableText buildFlagQuickActionComponent(IProtectedRegion region, IFlag flag, Formatting flagLinkColor) {
        MutableText regionTypeIndicator = Text.literal(region.getRegionType().type.substring(0, 1).toUpperCase());
        MutableText hoverText = Text.translatableWithFallback("cli.flag.info.hover", "Show %s flag info of region '%s'", flag.getName(), region.getName());
        MutableText flagInfoLink = buildFlagInfoLink(region, flag, flagLinkColor);
        return Text.translatable("%s %s %s %s %s",
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
    public static MutableText buildFlagInfoLink(IProtectedRegion region, IFlag flag, Formatting linkColor) {
        MutableText text = Text.literal(flag.getName());
        MutableText hoverText = Text.translatableWithFallback("cli.flag.info.hover", "Show %s flag info of region '%s'", flag.getName(), region.getName());
        return buildFlagInfoLink(region, flag, text, hoverText, linkColor);
    }

    public static MutableText buildFlagInfoLink(IProtectedRegion region, IFlag flag, MutableText text, MutableText hoverText, Formatting linkColor) {
        switch (region.getRegionType()) {
            case GLOBAL: {
                String cmd = buildCommandStr(FLAG.toString(), GLOBAL.toString(), flag.getName(), INFO.toString());
                return buildExecuteCmdComponent(text, hoverText, cmd, RUN_COMMAND, linkColor);
            }
            case DIMENSION: {
                String cmd = buildCommandStr(FLAG.toString(), DIM.toString(), region.getDim().getValue().toString(), flag.getName(), INFO.toString());
                return buildExecuteCmdComponent(text, hoverText, cmd, RUN_COMMAND, linkColor);
            }
            case LOCAL: {
                String cmd = buildCommandStr(FLAG.toString(), CommandConstants.LOCAL.toString(), region.getDim().getValue().toString(), region.getName(), flag.getName(), INFO.toString());
                return buildExecuteCmdComponent(text, hoverText, cmd, RUN_COMMAND, linkColor);
            }
            default:
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        }
    }

    public static MutableText buildFlagInfoLink(IProtectedRegion region, IFlag flag) {
        return buildFlagInfoLink(region, flag, LINK_COLOR);
    }

    public static MutableText buildFlagStateComponent(IProtectedRegion region, IFlag flag) {
        FlagState state = flag.getState();
        MutableText text = Text.literal(state.name);
        MutableText hover = Text.empty();
        Formatting color = WHITE;
        switch (state) {
            case ALLOWED:
                color = GREEN;
                hover = Text.translatableWithFallback("cli.flag.state.allowed.info.hover", "A flag with allowed state does not prevent the related action");
                break;
            case DENIED:
                color = RED;
                hover = Text.translatableWithFallback("cli.flag.state.denied.info.hover", "A flag with denied state prevents the related action");
                break;
            case DISABLED:
                color = GRAY;
                hover = Text.translatableWithFallback("cli.flag.state.disabled.info.hover", "A disabled flag is not considered in flag checks");
                break;
        }
        MutableText stateInfo = buildTextWithHoverAndBracketsMsg(text, hover, color);
        return Text.translatable("%s %s", stateInfo, buildFlagStateSuggestionLink(region, flag));
    }

    public static MutableText buildFlagStateSuggestionLink(IProtectedRegion region, IFlag flag) {
        MutableText hover = Text.translatableWithFallback("cli.flag.state.set.link.hover", "Set flag state for '%s' in '%s'", flag.getName(), region.getName());
        MutableText text = Text.translatableWithFallback("cli.flag.state.set.link.text", "s");
        switch (region.getRegionType()) {
            case GLOBAL: {
                String cmd = buildCommandStr(FLAG.toString(), GLOBAL.toString(), flag.getName(), STATE.toString(), "");
                return buildExecuteCmdComponent(text, hover, cmd, SUGGEST_COMMAND, LINK_COLOR);
            }
            case DIMENSION: {
                String cmd = buildCommandStr(FLAG.toString(), DIM.toString(), region.getDim().getValue().toString(), flag.getName(), STATE.toString(), "");
                return buildExecuteCmdComponent(text, hover, cmd, SUGGEST_COMMAND, LINK_COLOR);
            }
            case LOCAL: {
                String cmd = buildCommandStr(FLAG.toString(), CommandConstants.LOCAL.toString(), region.getDim().getValue().toString(), region.getName(), flag.getName(), STATE.toString(), "");
                return buildExecuteCmdComponent(text, hover, cmd, SUGGEST_COMMAND, LINK_COLOR);
            }
            case TEMPLATE:
                throw new NotImplementedException("No supported yet");
            default:
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        }
    }

    public static MutableText buildFlagOverrideInfoComponent(IFlag flag) {
        String fallback = flag.doesOverride() ? "active" : "inactive";
        MutableText linkText = Text.translatableWithFallback("cli.flag.msg.text.link.text." + flag.doesOverride(), fallback);
        MutableText hoverText = Text.translatableWithFallback("cli.flag.msg.text.link.hover." + flag.doesOverride(), fallback);
        Formatting color = flag.doesOverride() ? Formatting.GREEN : Formatting.GRAY;
        return buildTextWithHoverAndBracketsMsg(linkText, hoverText, color);
    }


    public static MutableText buildFlagOverrideToggleLink(IProtectedRegion region, IFlag flag, boolean shortLink) {
        String fallback = flag.doesOverride() ? "active" : "inactive";
        String fallbackHover = (!flag.doesOverride() ? "Disable" : "Enable") + " flag override for '%s' of '%s'";
        MutableText linkText = Text.translatableWithFallback("cli.flag.override.link.text." + flag.doesOverride(), fallback);
        MutableText hoverText = Text.translatableWithFallback("cli.flag.override.link.hover." + !flag.doesOverride(), fallbackHover, flag.getName(), region.getName());
        if (shortLink) {
            linkText = Text.translatableWithFallback("cli.flag.override.link.text.toggle", "o");
        }
        Formatting color = flag.doesOverride() ? Formatting.GREEN : Formatting.GRAY;
        switch (region.getRegionType()) {
            case GLOBAL: {
                String cmd = buildCommandStr(FLAG.toString(), GLOBAL.toString(), flag.getName(), OVERRIDE.toString());
                return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, color);
            }
            case DIMENSION: {
                String cmd = buildCommandStr(FLAG.toString(), DIM.toString(), region.getDim().getValue().toString(), flag.getName(), OVERRIDE.toString());
                return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, color);
            }
            case LOCAL: {
                String cmd = buildCommandStr(FLAG.toString(), CommandConstants.LOCAL.toString(), region.getDim().getValue().toString(), region.getName(), flag.getName(), OVERRIDE.toString());
                return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, color);
            }
            case TEMPLATE:
                throw new NotImplementedException("No supported yet");
            default:
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        }
    }

    public static MutableText buildFlagMessageEditLink(IProtectedRegion region, IFlag flag) {
        MutableText hover = Text.translatableWithFallback("cli.flag.msg.text.set.link.hover", "Change the message shown when the flag '%s' of '%s' is triggered", flag.getName(), region.getName());
        MutableText text = Text.translatableWithFallback("cli.flag.msg.text.set.link.text", "Edit");
        String msg = "\"" + flag.getFlagMsg().getMsg() + "\"";
        switch (region.getRegionType()) {
            case GLOBAL: {
                String cmd = buildCommandStr(FLAG.toString(), GLOBAL.toString(), flag.getName(), MSG.toString(), SET.toString(), msg);
                return buildExecuteCmdComponent(text, hover, cmd, SUGGEST_COMMAND, LINK_COLOR);
            }
            case DIMENSION: {
                String cmd = buildCommandStr(FLAG.toString(), DIM.toString(), region.getDim().getValue().toString(), flag.getName(), MSG.toString(), SET.toString(), msg);
                return buildExecuteCmdComponent(text, hover, cmd, SUGGEST_COMMAND, LINK_COLOR);
            }
            case LOCAL: {
                String cmd = buildCommandStr(FLAG.toString(), CommandConstants.LOCAL.toString(), region.getDim().getValue().toString(), region.getName(), flag.getName(), MSG.toString(), SET.toString(), msg);
                return buildExecuteCmdComponent(text, hover, cmd, SUGGEST_COMMAND, LINK_COLOR);
            }
            default:
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        }
    }

    private static MutableText buildFlagMessageClearLink(IProtectedRegion region, IFlag flag) {
        MutableText hover = Text.translatableWithFallback("cli.flag.msg.text.set.default", "Reset flag message for flag '%s' of '%s' to config default", flag.getName(), region.getName());
        MutableText text = Text.translatableWithFallback("cli.link.remove", "x");
        switch (region.getRegionType()) {
            case GLOBAL: {
                String cmd = buildCommandStr(FLAG.toString(), GLOBAL.toString(), flag.getName(), MSG.toString(), CLEAR.toString());
                return buildExecuteCmdComponent(text, hover, cmd, RUN_COMMAND, REMOVE_CMD_COLOR);
            }
            case DIMENSION: {
                String cmd = buildCommandStr(FLAG.toString(), DIM.toString(), region.getDim().getValue().toString(), flag.getName(), MSG.toString(), CLEAR.toString());
                return buildExecuteCmdComponent(text, hover, cmd, RUN_COMMAND, REMOVE_CMD_COLOR);
            }
            case LOCAL: {
                String cmd = buildCommandStr(FLAG.toString(), CommandConstants.LOCAL.toString(), region.getDim().getValue().toString(), region.getName(), flag.getName(), MSG.toString(), CLEAR.toString());
                return buildExecuteCmdComponent(text, hover, cmd, RUN_COMMAND, REMOVE_CMD_COLOR);
            }
            default:
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        }
    }

    public static MutableText buildFlagMuteToggleLink(IProtectedRegion region, IFlag flag, boolean shortLink) {
        String fallback = !flag.getFlagMsg().isMuted() ? "inactive" : "active";
        MutableText hover = Text.translatableWithFallback("cli.flag.msg.mute.set.link.hover", "Activate flag alert for '%s' in '%s'", flag.getName(), region.getName());
        MutableText text = Text.translatableWithFallback("cli.flag.msg.mute.set.link.text." + !flag.getFlagMsg().isMuted(), fallback);
        if (shortLink) {
            text = Text.translatableWithFallback("cli.flag.msg.mute.set.link.text.toggle", "m");
        }
        Formatting textFormatting = !flag.getFlagMsg().isMuted() ? GREEN : GRAY;
        switch (region.getRegionType()) {
            case GLOBAL: {
                String cmd = buildCommandStr(FLAG.toString(), GLOBAL.toString(), flag.getName(), MSG.toString(), MUTE.toString());
                return buildExecuteCmdComponent(text, hover, cmd, RUN_COMMAND, textFormatting);
            }
            case DIMENSION: {
                String cmd = buildCommandStr(FLAG.toString(), DIM.toString(), region.getDim().getValue().toString(), flag.getName(), MSG.toString(), MUTE.toString());
                return buildExecuteCmdComponent(text, hover, cmd, RUN_COMMAND, textFormatting);
            }
            case LOCAL: {
                String cmd = buildCommandStr(FLAG.toString(), CommandConstants.LOCAL.toString(), region.getDim().getValue().toString(), region.getName(), flag.getName(), MSG.toString(), MUTE.toString());
                return buildExecuteCmdComponent(text, hover, cmd, RUN_COMMAND, textFormatting);
            }
            case TEMPLATE:
                throw new NotImplementedException("No supported yet");
            default:
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        }
    }

    public static MutableText buildFlagMessageHoverText(IProtectedRegion region, IFlag flag) {
        String flagMsg = flag.getFlagMsg().getMsg();
        if (flag.getFlagMsg().getMsg().length() > 30) {
            flagMsg = flagMsg.substring(0, 30) + "...";
        }
        MutableText flagMsgText = Text.literal(flagMsg);
        MutableText hoverText = Text.literal(flag.getFlagMsg().getMsg());
        // if flag has default msg, use default msg
        if (flag.getFlagMsg().isDefault()) {
            String hoverFallback = "[{region}]: The '{flag}' flag denies this action here!";
            hoverText = Text.translatableWithFallback("flag.msg.deny." + region.getRegionType().type + ".default", hoverFallback);
        }
        return buildTextWithHoverAndBracketsMsg(flagMsgText, hoverText, WHITE);
    }

    /**
     * Message: [set] [x]: 'msg' <br></br>
     */
    public static MutableText buildFlagMessageComponent(IProtectedRegion region, IFlag flag) {
        MutableText editLink = buildFlagMessageEditLink(region, flag);
        MutableText clearLink = buildFlagMessageClearLink(region, flag);
        MutableText flagMsgTextWithHover = buildFlagMessageHoverText(region, flag);
        return Text.translatable("%s %s '%s'", editLink, clearLink, flagMsgTextWithHover);
    }

    /**
     * Show flags for region, with a color of their state
     * But also show parent flags in italic
     * How do we want to handle flags defined in the region but also in the parent?
     * If the child flag is dominant, we display the child flag, and add a hint to the parent
     * If the flag is overriden, we format them with strikethrough and add a link to the parent which overrides it
     */
    public static List<MutableText> buildFlagEntries(IProtectedRegion region) {
        List<MutableText> flagEntries = new ArrayList<>();
        Map<String, FlagCorrelation> flagMapRecursive = getFlagMapRecursive(region, null);
        Map<FlagState, List<FlagCorrelation>> flagStateListMap = LocalRegions.sortFlagsByState(flagMapRecursive);
        flagEntries.addAll(buildFlagEntries(flagStateListMap, FlagState.ALLOWED));
        flagEntries.addAll(buildFlagEntries(flagStateListMap, FlagState.DENIED));
        flagEntries.addAll(buildFlagEntries(flagStateListMap, FlagState.DISABLED));
        return flagEntries;
    }

    public static List<MutableText> buildRegionFlagEntries(IProtectedRegion region) {
        List<MutableText> flagEntries = new ArrayList<>();
        flagEntries.addAll(buildRegionFlagEntries(region, FlagState.ALLOWED));
        flagEntries.addAll(buildRegionFlagEntries(region, FlagState.DENIED));
        flagEntries.addAll(buildRegionFlagEntries(region, FlagState.DISABLED));
        return flagEntries;
    }

    private static List<MutableText> buildRegionFlagEntries(IProtectedRegion region, FlagState state) {
        List<IFlag> flagsByState = getFlagsWithState(region.getFlagContainer(), state);
        flagsByState.sort(Comparator.comparing(IFlag::getName));
        return flagsByState.stream()
                .map(flag -> buildRemoveFlagEntry(region, flag, colorForState(state)))
                .collect(Collectors.toList());
    }

    private static List<MutableText> buildFlagEntries(Map<FlagState, List<FlagCorrelation>> flagStateListMap, FlagState state) {
        List<FlagCorrelation> flagsByState = flagStateListMap.get(state);
        flagsByState.sort(Comparator.comparing(flagCorrelation -> flagCorrelation.getFlag().getName()));
        return flagsByState.stream()
                .map(flagCorrelation -> buildRemoveFlagEntry(flagCorrelation.getRegion(), flagCorrelation.getFlag(), colorForState(state)))
                .collect(Collectors.toList());
    }

    private static Formatting colorForState(FlagState state) {
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

    public static List<MutableText> buildRemoveRegionEntries(IProtectedRegion parent, List<IProtectedRegion> regions) {
        return regions.stream()
                .map(region -> buildRemoveRegionEntry(parent, region))
                .collect(Collectors.toList());
    }

    /**
     * Builds a TextComponent for the given flag and region. <br></br>
     * Currently not used in the CLI for obvious reasons. <br></br>
     */
    public static MutableText buildRemoveAllRegionsAttemptLink(DimensionRegionCache dimCache) {
        String cmd = buildCommandStr(DIM.toString(), dimCache.getDimensionalRegion().getName(), DELETE_ALL.toString(), REGIONS.toString());
        MutableText hover = Text.translatableWithFallback("cli.msg.info.dim.region.remove.all.link.hover", "Remove all regions from %s", dimCache.getDimensionalRegion().getName());
        MutableText text = Text.translatableWithFallback("cli.link.remove", "x");
        return buildExecuteCmdComponent(text, hover, cmd, RUN_COMMAND, REMOVE_CMD_COLOR);
    }

    public static MutableText buildRemoveAllRegionsLink(DimensionRegionCache dimCache) {
        String cmd = buildCommandStr(DIM.toString(), dimCache.getDimensionalRegion().getName(), DELETE_ALL.toString(), REGIONS.toString(), FOREVER.toString(), SERIOUSLY.toString());
        MutableText hover = Text.translatableWithFallback("cli.msg.info.dim.region.remove.all.link.hover", "Remove all regions from %s", dimCache.getDimensionalRegion().getName());
        MutableText text = Text.translatableWithFallback("cli.link.remove", "x");
        return buildExecuteCmdComponent(text, hover, cmd, RUN_COMMAND, REMOVE_CMD_COLOR);
    }


    public static MutableText buildRemoveRegionLink(IProtectedRegion region) {
        String cmd = buildCommandStr(DIM.toString(), region.getDim().getValue().toString(), DELETE.toString(), region.getName(), "-y");
        MutableText hover = Text.translatableWithFallback("cli.msg.info.dim.region.remove.link.hover", "Remove region %s", region.getName());
        MutableText text = Text.translatableWithFallback("cli.link.remove", "x");
        return buildExecuteCmdComponent(text, hover, cmd, RUN_COMMAND, REMOVE_CMD_COLOR);
    }

    public static MutableText buildRemoveRegionEntry(IProtectedRegion parent, IProtectedRegion region) {
        MutableText regionRemoveLink;
        switch (parent.getRegionType()) {
            case GLOBAL: {
                regionRemoveLink = Text.translatable("%s %s", buildDimResetComponent((DimensionalRegion) region), buildRegionInfoLink(region));
                break;
            }
            case DIMENSION: {
                MutableText removeLink = Text.empty();
                MutableText regionInfoLinkWithIndicator = Text.empty();
                MutableText childCompInfo = Text.translatableWithFallback("cli.msg.info.dim.region.child.hover", "This is a direct child region of the Dimensional Region");
                MutableText childIndicator = buildTextWithHoverAndBracketsMsg(Text.literal("*"), childCompInfo, GOLD);
                if (parent.hasChild(region)) {
                    regionInfoLinkWithIndicator = Text.translatable("%s%s", buildRegionInfoLink(region), childIndicator);
                } else {
                    regionInfoLinkWithIndicator = Text.translatable("%s", buildRegionInfoLink(region));
                }
                removeLink = buildDimSuggestRegionRemovalLink((IMarkableRegion) region);
                regionRemoveLink = Text.translatable("%s %s", removeLink, buildRegionInfoAndTpLink((IMarkableRegion) region, regionInfoLinkWithIndicator));
                break;
            }
            case LOCAL: {
                regionRemoveLink = Text.translatable("%s %s", buildRegionRemoveChildLink(parent, region), buildRegionInfoLink(region));
                break;
            }
            default:
                throw new IllegalArgumentException();
        }
        return Text.translatable(" - %s", regionRemoveLink);
    }

    private static MutableText buildDimResetComponent(DimensionalRegion region) {
        String cmd = buildCommandStr(DIM.toString(), region.getDim().getValue().toString(), CommandConstants.RESET.toString(), DIM.toString());
        MutableText hover = Text.translatableWithFallback("cli.dim.reset.dim.link.hover", "Reset Dimensional Region '%s'", region.getName());
        MutableText text = Text.translatableWithFallback("cli.link.action.undo.text", "←");
        return buildExecuteCmdComponent(text, hover, cmd, SUGGEST_COMMAND, REMOVE_CMD_COLOR);
    }

    private static String buildPageCommand(String cmd, int page) {
        return cmd + " " + page;
    }

    public static List<MutableText> buildPaginationComponents(MutableText description, String cmd, List<MutableText> entries, int pageNo, MutableText addEmptyLink) {
        List<MutableText> paginationComponents = new ArrayList<>();
        int numberOfPages = entries.size() / RegionConfig.getPaginationSize();
        if (numberOfPages == 0 || entries.size() % RegionConfig.getPaginationSize() != 0) {
            numberOfPages += 1;
        }
        if (pageNo < FIRST_PAGE_IDX || pageNo >= numberOfPages) {
            paginationComponents.add(Text.translatableWithFallback("cli.msg.info.pagination.error.index", "Invalid page index supplied: %s (Try [0..%s])", pageNo, numberOfPages - 1).formatted(RED));
            return paginationComponents;
        }
        boolean hasMultiplePages = numberOfPages > 1;

        MutableText paginationControl = buildPaginationControl(
                buildFirstLinkArrow(cmd, pageNo, hasMultiplePages),
                buildPrevLinkArrow(cmd, pageNo, hasMultiplePages),
                pageNo, numberOfPages,
                buildNextLinkArrow(cmd, pageNo, numberOfPages, hasMultiplePages),
                buildLastLinkArrow(cmd, pageNo, numberOfPages, hasMultiplePages)
        );

        int from = pageNo * RegionConfig.getPaginationSize();
        int to = Math.min(RegionConfig.getPaginationSize() + (RegionConfig.getPaginationSize() * pageNo), entries.size());
        List<MutableText> entriesForPage = entries.subList(from, to);

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

    private static MutableText buildLastLinkArrow(String cmd, int pageNo, int numberOfPages, boolean hasMultiplePages) {
        MutableText last = Text.translatableWithFallback("cli.msg.info.pagination.last.text", "⏭");
        MutableText lastHover = Text.translatableWithFallback("cli.msg.info.pagination.last.hover", "Last page");
        return hasMultiplePages && pageNo < numberOfPages - 1
                ? buildExecuteCmdComponent(last, lastHover, buildPageCommand(cmd, numberOfPages - 1), RUN_COMMAND, LINK_COLOR)
                : Texts.bracketed(last).formatted(INACTIVE_LINK_COLOR);
    }

    private static MutableText buildNextLinkArrow(String cmd, int pageNo, int numberOfPages, boolean hasMultiplePages) {
        MutableText next = Text.translatableWithFallback("cli.msg.info.pagination.next.text", "▶");
        MutableText nextHover = Text.translatableWithFallback("cli.msg.info.pagination.next.hover", "Next page");
        return hasMultiplePages && pageNo < numberOfPages - 1
                ? buildExecuteCmdComponent(next, nextHover, buildPageCommand(cmd, Math.min(pageNo + 1, numberOfPages - 1)), RUN_COMMAND, LINK_COLOR)
                : Texts.bracketed(next).formatted(INACTIVE_LINK_COLOR);
    }

    private static MutableText buildPrevLinkArrow(String cmd, int pageNo, boolean hasMultiplePages) {
        MutableText previous = Text.translatableWithFallback("cli.msg.info.pagination.previous.text", "◀");
        MutableText previousHover = Text.translatableWithFallback("cli.msg.info.pagination.previous.hover", "Previous page");
        return hasMultiplePages && pageNo > FIRST_PAGE_IDX
                ? buildExecuteCmdComponent(previous, previousHover, buildPageCommand(cmd, Math.max(pageNo - 1, FIRST_PAGE_IDX)), RUN_COMMAND, LINK_COLOR)
                : Texts.bracketed(previous).formatted(INACTIVE_LINK_COLOR);
    }

    private static MutableText buildFirstLinkArrow(String cmd, int pageNo, boolean hasMultiplePages) {
        MutableText first = Text.translatableWithFallback("cli.msg.info.pagination.first.text", "⏮");
        MutableText firstHover = Text.translatableWithFallback("cli.msg.info.pagination.first.hover", "First page");
        return hasMultiplePages && pageNo != FIRST_PAGE_IDX
                ? buildExecuteCmdComponent(first, firstHover, buildPageCommand(cmd, FIRST_PAGE_IDX), RUN_COMMAND, LINK_COLOR)
                : Texts.bracketed(first).formatted(INACTIVE_LINK_COLOR);
    }

    private static MutableText buildPaginationControl(MutableText front, MutableText back, int pageNo, int maxPage, MutableText forward, MutableText last) {
        // [<<]  [<]  x/n  [>]  [>>]
        MutableText pageIndicator = Text.literal((pageNo + 1) + "/" + (maxPage));
        return Text.translatable(" %s  %s  %s  %s  %s", front, back, pageIndicator, forward, last);
    }

    // [x]
    public static MutableText buildParentClearLink(IMarkableRegion region) {
        String clearRegionParentCmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().getValue().toString(), region.getName(), PARENT.toString(), REMOVE.toString());
        MutableText parentClearLinkText = Text.translatableWithFallback("cli.link.remove", "x");
        MutableText parentClearHoverText = Text.translatableWithFallback("cli.msg.info.region.parent.clear.link.hover", "Clear '%s' as parent region", region.getParent().getName());
        return buildExecuteCmdComponent(parentClearLinkText, parentClearHoverText, clearRegionParentCmd, RUN_COMMAND, REMOVE_CMD_COLOR);
    }

    // No parent set [+]
    private static MutableText createParentAddLink(IProtectedRegion region) {
        String setRegionParentCmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().getValue().toString(), region.getName(), PARENT.toString(), SET.toString(), "");
        MutableText setParentLinkText = Text.translatableWithFallback("cli.link.add", "+");
        MutableText setParentHoverText = Text.translatableWithFallback("cli.msg.info.region.parent.set.link.hover", "Set parent for region %s", region.getName());
        return Text.translatable("%s %s",
                Text.translatableWithFallback("cli.msg.info.region.parent.null", "No parent set"),
                buildExecuteCmdComponent(setParentLinkText, setParentHoverText, setRegionParentCmd, RUN_COMMAND, GREEN));
    }

    // [n regions] [+]
    public static MutableText buildDimRegionsLink(DimensionRegionCache dimCache) {
        DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
        String command = buildCommandStr(DIM.toString(), dimRegion.getDim().getValue().toString(), LIST.toString(), CommandConstants.LOCAL.toString());
        MutableText text = Text.translatableWithFallback("cli.msg.dim.info.region.list.link.text", "%s region(s)", dimCache.getRegions().size());
        MutableText hover = Text.translatableWithFallback("cli.msg.dim.info.region.list.link.hover", "List regions in %s", dimRegion.getName());
        MutableText listLocalRegionsLink = buildExecuteCmdComponent(text, hover, command, RUN_COMMAND, LINK_COLOR);
        MutableText createRegionLink = buildDimCreateRegionLink(dimRegion);
        if (dimRegion.getChildren().isEmpty()) {
            return Text.translatable("%s %s", text, createRegionLink);
        }
        return Text.translatable("%s %s", listLocalRegionsLink, createRegionLink);
    }

    public static MutableText buildRegionListChildrenLink(IProtectedRegion region) {
        MutableText text = Text.translatableWithFallback("cli.msg.info.region.children.list.link.text", "%s child regions(s)", region.getChildren().size());
        MutableText hover = Text.translatableWithFallback("cli.msg.info.region.children.list.link.hover", "List direct child regions of '%s'", region.getName());
        return switch (region.getRegionType()) {
            case GLOBAL -> {
                // [n dimensions(s)]
                Collection<String> dimensionList = RegionDataManager.get().getDimensionList();
                String command = buildCommandStr(GLOBAL.toString(), LIST.toString(), CHILDREN.toString());
                MutableText listDimRegionsLinkText = Text.translatableWithFallback("cli.msg.global.info.region.list.link.text", "%s dimensions(s)", dimensionList.size());
                MutableText listDimRegionsHoverText = Text.translatableWithFallback("cli.msg.global.info.region.list.link.hover", "List all Dimensional Regions");
                yield buildExecuteCmdComponent(listDimRegionsLinkText, listDimRegionsHoverText, command, RUN_COMMAND, LINK_COLOR);
            }
            case DIMENSION -> {
                // [n children] [+]
                String command = buildCommandStr(DIM.toString(), region.getDim().getValue().toString(), LIST.toString(), CHILDREN.toString());
                MutableText listDimRegionsListLink = buildExecuteCmdComponent(text, hover, command, RUN_COMMAND, LINK_COLOR);
                if (region.getChildren().isEmpty()) {
                    yield Text.translatable("%s %s", text, buildDimCreateRegionLink(region));
                }
                yield Text.translatable("%s %s", listDimRegionsListLink, buildDimCreateRegionLink(region));
            }
            case LOCAL -> {
                // [n children] [+]
                String regionChildrenListLink = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().getValue().toString(), region.getName(), LIST.toString(), CHILDREN.toString());
                MutableText regionChildrenLink = buildExecuteCmdComponent(text, hover, regionChildrenListLink, RUN_COMMAND, LINK_COLOR);
                MutableText addChildrenLink = buildRegionAddChildrenLink(region);
                if (region.getChildren().isEmpty()) {
                    yield Text.translatable("%s %s", text, addChildrenLink);
                }
                yield Text.translatable("%s %s", regionChildrenLink, addChildrenLink);
            }
            default -> throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        };
    }

    public static MutableText buildRegionAddChildrenLink(IProtectedRegion region) {
        String addChildrenCmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().getValue().toString(), region.getName(), ADD.toString(), CHILD.toString(), "");
        MutableText addChildrenLinkText = Text.translatableWithFallback("cli.link.add", "+");
        MutableText addChildrenHoverText = Text.translatableWithFallback("cli.msg.info.region.children.add.link.hover", "Add child to region %s", region.getName());
        return buildExecuteCmdComponent(addChildrenLinkText, addChildrenHoverText, addChildrenCmd, SUGGEST_COMMAND, ADD_CMD_COLOR);
    }

    public static MutableText buildDimCreateRegionLink(IProtectedRegion region) {
        String dimCreateRegionCmd = buildCommandStr(DIM.toString(), region.getDim().getValue().toString(), CREATE.toString(), CommandConstants.LOCAL.toString(), "");
        MutableText createRegionLinkText = Text.translatableWithFallback("cli.link.add", "+");
        MutableText createRegionHoverText = Text.translatableWithFallback("cli.msg.dim.info.region.create.link.hover", "Create region in dimension %s", region.getName());
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
    public static MutableText buildResponsibleFlagListLink(IProtectedRegion region) {
        Map<String, FlagCorrelation> flagsInHierarchy = getFlagMapRecursive(region, null);
        MutableText responsibleFlagsNumber = buildTextWithHoverMsg(Text.translatable("%s", flagsInHierarchy.size()),
                Text.translatableWithFallback("cli.msg.info.region.flag.responsible.number.hover",
                        "%s responsible flag(s) applicable for %s", flagsInHierarchy.size(), region.getName()), LINK_COLOR);
        MutableText responsibleFlagListHoverText = Text.translatableWithFallback("cli.msg.info.region.flag.responsible.link.hover",
                "Show responsible region flags for %s", region.getName());
        switch (region.getRegionType()) {
            case GLOBAL: {
                String flagListCmd = buildCommandStr(GLOBAL.toString(), LIST.toString(), FLAG.toString());
                MutableText responsibleFlagListLink = buildExecuteCmdComponent(responsibleFlagsNumber, responsibleFlagListHoverText, flagListCmd, RUN_COMMAND, LINK_COLOR);
                MutableText responsibleFlagsComp = flagsInHierarchy.isEmpty() ? responsibleFlagsNumber : responsibleFlagListLink;
                return Text.translatableWithFallback("cli.msg.info.region.flag.responsible.link.text", "%s responsible flag(s)", responsibleFlagsComp);
            }
            case DIMENSION: {
                String flagListCmd = buildCommandStr(DIM.toString(), region.getDim().getValue().toString(), LIST.toString(), FLAG.toString());
                MutableText responsibleFlagListLink = buildExecuteCmdComponent(responsibleFlagsNumber, responsibleFlagListHoverText, flagListCmd, RUN_COMMAND, LINK_COLOR);
                MutableText responsibleFlagsComp = flagsInHierarchy.isEmpty() ? responsibleFlagsNumber : responsibleFlagListLink;
                return Text.translatableWithFallback("cli.msg.info.region.flag.responsible.link.text", "%s responsible flag(s)", responsibleFlagsComp);
            }
            case LOCAL: {
                String flagListCmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().getValue().toString(), region.getName(), LIST.toString(), FLAG.toString());
                MutableText responsibleFlagListLink = buildExecuteCmdComponent(responsibleFlagsNumber, responsibleFlagListHoverText, flagListCmd, RUN_COMMAND, LINK_COLOR);
                MutableText responsibleFlagsComp = flagsInHierarchy.isEmpty() ? responsibleFlagsNumber : responsibleFlagListLink;
                return Text.translatableWithFallback("cli.msg.info.region.flag.responsible.link.text", "%s responsible flag(s)", responsibleFlagsComp);
            }
            default:
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        }
    }

    public static MutableText buildFlagsListLink(IProtectedRegion region) {
        if (region.getRegionType() == RegionType.GLOBAL) {
            return buildRegionFlagListLink(region);
        }
        return Text.translatable("%s | %s", buildResponsibleFlagListLink(region), buildRegionFlagListLink(region));
    }

    // [m] flag(s) [+]
    public static MutableText buildRegionFlagListLink(IProtectedRegion region) {
        MutableText regionFlagNumber = buildTextWithHoverMsg(Text.translatable("%s", region.getFlags().size()),
                Text.translatableWithFallback("cli.msg.info.region.flag.number.hover", "%s flag(s)", region.getFlags().size(), region.getName()), LINK_COLOR);
        MutableText flagListHoverText = Text.translatableWithFallback("cli.msg.info.region.flag.link.hover", "%s flag(s)", region.getName());
        switch (region.getRegionType()) {
            case GLOBAL: {
                String regionFlagListCmd = buildCommandStr(GLOBAL.toString(), LIST.toString(), REGION_FLAG.toString());
                MutableText regionFlagListLink = buildExecuteCmdComponent(regionFlagNumber, flagListHoverText, regionFlagListCmd, RUN_COMMAND, LINK_COLOR);
                MutableText regionFlagsComp = region.getFlags().isEmpty() ? regionFlagNumber : regionFlagListLink;
                return Text.translatable("%s %s",
                        Text.translatableWithFallback("cli.msg.info.region.flag.region.link.text", "%s flag(s)", regionFlagsComp),
                        buildAddFlagLink(region));
            }
            case DIMENSION: {
                String regionFlagListCmd = buildCommandStr(DIM.toString(), region.getDim().getValue().toString(), LIST.toString(), REGION_FLAG.toString());
                MutableText regionFlagListLink = buildExecuteCmdComponent(regionFlagNumber, flagListHoverText, regionFlagListCmd, RUN_COMMAND, LINK_COLOR);
                MutableText regionFlagsComp = region.getFlags().isEmpty() ? regionFlagNumber : regionFlagListLink;
                return Text.translatable("%s %s",
                        Text.translatableWithFallback("cli.msg.info.region.flag.region.link.text", "%s flag(s)", regionFlagsComp),
                        buildAddFlagLink(region));
            }
            case LOCAL: {
                String regionFlagListCmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().getValue().toString(), region.getName(), LIST.toString(), REGION_FLAG.toString());
                MutableText regionFlagListLink = buildExecuteCmdComponent(regionFlagNumber, flagListHoverText, regionFlagListCmd, RUN_COMMAND, LINK_COLOR);
                MutableText regionFlagsComp = region.getFlags().isEmpty() ? regionFlagNumber : regionFlagListLink;
                return Text.translatable("%s %s",
                        Text.translatableWithFallback("cli.msg.info.region.flag.region.link.text", "%s flag(s)", regionFlagsComp),
                        buildAddFlagLink(region));
            }
            default:
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        }
    }

    public static MutableText buildAddFlagLink(IProtectedRegion region) {
        return buildAddFlagLink(region, "");
    }

    public static MutableText buildAddFlagLink(IProtectedRegion region, String flag) {
        MutableText hoverText = Text.translatableWithFallback("cli.msg.info.region.flag.add.link.hover", "Add new flag to region %s", region.getName());
        MutableText linkText = Text.translatableWithFallback("cli.link.add", "+");
        switch (region.getRegionType()) {
            case GLOBAL: {
                String command = buildCommandStr(GLOBAL.toString(), ADD.toString(), FLAG.toString(), flag);
                return buildExecuteCmdComponent(linkText, hoverText, command, SUGGEST_COMMAND, ADD_CMD_COLOR);
            }
            case DIMENSION: {
                String command = buildCommandStr(DIM.toString(), region.getDim().getValue().toString(), ADD.toString(), FLAG.toString(), flag);
                return buildExecuteCmdComponent(linkText, hoverText, command, SUGGEST_COMMAND, ADD_CMD_COLOR);
            }
            case LOCAL: {
                String addCmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().getValue().toString(), region.getName(), ADD.toString(), FLAG.toString(), flag);
                return buildExecuteCmdComponent(linkText, hoverText, addCmd, SUGGEST_COMMAND, ADD_CMD_COLOR);
            }
            default:
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        }
    }

    public static MutableText buildRegionStateLink(IProtectedRegion region) {
        MutableText linkText = Text.translatableWithFallback("cli.msg.info.region.state.link.text", "State");
        MutableText hoverText = Text.translatableWithFallback("cli.msg.info.region.state.link.hover", "Show region state for %s", region.getName());
        switch (region.getRegionType()) {
            case GLOBAL: {
                String command = buildCommandStr(GLOBAL.toString(), STATE.toString());
                return buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, LINK_COLOR);
            }
            case DIMENSION: {
                String command = buildCommandStr(DIM.toString(), region.getDim().getValue().toString(), STATE.toString());
                return buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, LINK_COLOR);
            }
            case LOCAL: {
                String showStateCmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().getValue().toString(), region.getName(), STATE.toString());
                return buildExecuteCmdComponent(linkText, hoverText, showStateCmd, RUN_COMMAND, LINK_COLOR);
            }
            default:
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        }
    }

    public static MutableText buildInfoComponent(String subjectLangKey, String fallback, MutableText payload) {
        return Text.translatable("%s: %s", Text.translatable(subjectLangKey, fallback), payload);
    }


    public static String buildExecuteCommandString(RegistryKey<World> dim, String command) {
        return "/execute in " + dim.getValue() + " run " + command;
    }

    public static String buildTeleportCmd(RegistryKey<World> dim, String tpSource, BlockPos target) {
        return buildExecuteCommandString(dim, "tp " + tpSource + " " + buildBlockCoordinateStr(target));
    }

    /**
     * [X,Y,Z]
     */
    public static MutableText buildDimensionalBlockTpLink(RegistryKey<World> dim, BlockPos target) {
        String teleportCmd = buildTeleportCmd(dim, "@s", target);
        MutableText text = Text.translatableWithFallback("cli.msg.info.region.area.tp.block.link.text", "%s", buildBlockPosLinkText(target));
        MutableText hover = Text.translatableWithFallback("cli.msg.info.region.area.tp.block.link.hover", "Teleport to block");
        return buildExecuteCmdComponent(text, hover, teleportCmd, RUN_COMMAND, TP_COLOR);
    }

    /**
     * [region] @ [X,Y,Z]
     */
    public static MutableText buildRegionInfoAndTpLink(IMarkableRegion region) {
        return Text.translatable("%s @ %s", buildRegionInfoLink(region), buildRegionTeleportLink(region, null));
    }

    private static MutableText buildRegionInfoAndTpLink(IMarkableRegion region, MutableText regionInfoLinkWithIndicator) {
        return Text.translatable("%s @ %s", regionInfoLinkWithIndicator, buildRegionTeleportLink(region, null));
    }

    public static MutableText buildRegionTeleportLink(IMarkableRegion region, PlayerEntity player) {
        String regionTpCmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().getValue().toString(), region.getName(), AREA.toString(), TELEPORT.toString());
        if (player != null) {
            regionTpCmd = appendSubCommand(regionTpCmd, player.getEntityName());
        }
        MutableText text = Text.translatableWithFallback("cli.msg.info.region.area.tp.link.text", "%s", buildBlockPosLinkText(region.getTpTarget()));
        MutableText hover = Text.translatableWithFallback("cli.msg.info.region.area.tp.link.hover", "Teleport to region '%s' in dimension '%s", region.getName(), region.getDim().getValue().toString());
        return buildExecuteCmdComponent(text, hover, regionTpCmd, RUN_COMMAND, TP_COLOR);
    }

    public static MutableText buildRegionSetTpLink(IMarkableRegion region) {
        String setTpPosCmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().getValue().toString(), region.getName(), AREA.toString(), TELEPORT.toString(), SET.toString(), "");
        MutableText linkText = Text.translatableWithFallback("cli.msg.info.region.area.tp.set.link.text", "set TP");
        MutableText hoverText = Text.translatableWithFallback("cli.msg.info.region.area.tp.set.link.hover", "Set new teleport anchor for '%s'", region.getName());
        return buildExecuteCmdComponent(linkText, hoverText, setTpPosCmd, SUGGEST_COMMAND, LINK_COLOR);
    }

    public static MutableText buildDimSuggestRegionRemovalLink(IMarkableRegion region) {
        String cmd = buildCommandStr(DIM.toString(), region.getDim().getValue().toString(), DELETE.toString(), region.getName());
        MutableText hover = Text.translatableWithFallback("cli.msg.info.dim.region.remove.link.hover", "Remove region %s", region.getName());
        MutableText text = Text.translatableWithFallback("cli.link.remove", "x");
        return buildExecuteCmdComponent(text, hover, cmd, SUGGEST_COMMAND, REMOVE_CMD_COLOR);
    }

    /**
     * @param region    the region to build the link for
     * @param names     the names of the players or teams of the group
     * @param groupType the type of the group (player or team)
     * @param group     the name of the group
     * @return a list of links to remove the group from the region
     */
    public static List<MutableText> buildRemoveGroupMemberEntries(IProtectedRegion region, List<String> names, GroupType groupType, String group) {
        return names.stream().sorted().map(name -> buildRemoveGroupEntry(region, name, groupType, group)).collect(Collectors.toList());
    }

    public static MutableText buildRemoveGroupEntry(IProtectedRegion region, String name, GroupType groupType, String group) {
        MutableText linkText = Text.translatableWithFallback("cli.link.remove", "x");
        MutableText hoverText = Text.translatableWithFallback("cli.msg.info.region.group." + groupType.name + ".remove.link.hover", name, region.getName());
        MutableText regionRemoveLink;
        if (groupType == GroupType.PLAYER) {
            PlayerEntity player = RegionDataManager.serverInstance.getPlayerManager().getPlayer(name);
            boolean isOffline = player == null;
            if (isOffline) {
                MutableText offlinePlayerRemoveLink = buildRemoveLinkForOfflinePlayer(region, name, groupType, group, linkText, hoverText);
                return Text.translatable(" - %s %s", offlinePlayerRemoveLink, buildGroupInfo(region, name, groupType));
            }
        }
        regionRemoveLink = switch (region.getRegionType()) {
            case GLOBAL -> {
                String command = buildCommandStr(GLOBAL.toString(), REMOVE.toString(), groupType.name, group, name);
                yield buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, REMOVE_CMD_COLOR);
            }
            case DIMENSION -> {
                String command = buildCommandStr(DIM.toString(), region.getDim().getValue().toString(), REMOVE.toString(), groupType.name, group, name);
                yield buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, REMOVE_CMD_COLOR);
            }
            case LOCAL -> {
                String command = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().getValue().toString(), region.getName(), REMOVE.toString(), groupType.name, group, name);
                yield buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, REMOVE_CMD_COLOR);
            }
            default -> throw new IllegalArgumentException();
        };
        return Text.translatable(" - %s %s", regionRemoveLink, buildGroupInfo(region, name, groupType));
    }

    private static MutableText buildRemoveLinkForOfflinePlayer(IProtectedRegion region, String name, GroupType groupType, String group, MutableText linkText, MutableText hoverText) {
        return switch (region.getRegionType()) {
            case GLOBAL -> {
                String command = buildCommandStr(GLOBAL.toString(), REMOVE.toString(), groupType.name, group, BY_NAME.toString(), name);
                yield buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, REMOVE_CMD_COLOR);
            }
            case DIMENSION -> {
                String command = buildCommandStr(DIM.toString(), region.getDim().getValue().toString(), REMOVE.toString(), groupType.name, group, BY_NAME.toString(), name);
                yield buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, REMOVE_CMD_COLOR);
            }
            case LOCAL -> {
                String command = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().getValue().toString(), region.getName(), REMOVE.toString(), groupType.name, group, BY_NAME.toString(), name);
                yield buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, REMOVE_CMD_COLOR);
            }
            default -> throw new IllegalArgumentException();
        };
    }

    public static MutableText buildGroupInfo(IProtectedRegion region, String groupMemberName, GroupType groupType) {
        return switch (groupType) {
            case PLAYER -> {
                PlayerEntity player = RegionDataManager.serverInstance.getPlayerManager().getPlayer(groupMemberName);
                if (player == null) {
                    yield Text.translatable("%s %s",
                            Text.literal(groupMemberName).formatted(GRAY),
                            Text.translatableWithFallback("cli.msg.info.player.list.entry.offline", "(offline)"));
                } else {
                    yield buildPlayerHoverComponent(player);
                }

            }
            case TEAM -> {
                Team team = RegionDataManager.serverInstance.getScoreboard().getPlayerTeam(groupMemberName);
                yield team == null ? Text.literal(groupMemberName) : buildTeamHoverComponent(team);
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
    public static MutableText buildRegionRemoveChildLink(IProtectedRegion region, IProtectedRegion child) {
        String command = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().getValue().toString(), region.getName(), REMOVE.toString(), CHILD.toString(), child.getName());
        MutableText linkText = Text.translatableWithFallback("cli.link.remove", "x");
        MutableText linkHoverText = Text.translatableWithFallback("cli.msg.info.region.children.remove.link.hover", "Remove child '%s' from region %s", child.getName(), region.getName());
        return buildExecuteCmdComponent(linkText, linkHoverText, command, SUGGEST_COMMAND, REMOVE_CMD_COLOR);
    }

    public static MutableText buildRegionActionUndoLink(String cmd, CommandConstants toReplace, CommandConstants replacement) {
        String revertCmd = ArgumentUtil.revertCommand(cmd, toReplace, replacement);
        MutableText revertLinkText = Text.translatableWithFallback("cli.link.action.undo.text", "←");
        MutableText revertLinkHover = Text.translatableWithFallback("cli.link.action.undo.hover", "Undo action.");
        return buildExecuteCmdComponent(revertLinkText, revertLinkHover, revertCmd, RUN_COMMAND, DARK_RED);
    }

    public static MutableText buildRegionActionUndoLink(String cmd, String toReplace, String replacement) {
        String revertCmd = ArgumentUtil.revertCommand(cmd, toReplace, replacement);
        MutableText revertLinkText = Text.translatableWithFallback("cli.link.action.undo.text", "←");
        MutableText revertLinkHover = Text.translatableWithFallback("cli.link.action.undo.hover", "Undo action.");
        return buildExecuteCmdComponent(revertLinkText, revertLinkHover, revertCmd, RUN_COMMAND, DARK_RED);
    }

    public static MutableText buildRegionFlagInfoHeader(IProtectedRegion region, MutableText flagListLink) {
        return buildHeader(Text.translatableWithFallback("cli.msg.info.header.in", "== %s in %s ==", flagListLink, buildRegionInfoLink(region)));
    }

    public static MutableText buildFlagInfoHeader(IProtectedRegion region, IFlag flag) {
        return buildHeader(Text.translatableWithFallback("cli.msg.info.header.flag.in", "== Flag %s in %s ==", buildFlagInfoLink(region, flag), buildRegionInfoLink(region)));
    }
}
