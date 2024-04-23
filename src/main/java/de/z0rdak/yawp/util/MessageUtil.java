package de.z0rdak.yawp.util;

import de.z0rdak.yawp.commands.CommandConstants;
import de.z0rdak.yawp.commands.CommandUtil;
import de.z0rdak.yawp.commands.RegionCommands;
import de.z0rdak.yawp.commands.arguments.ArgumentUtil;
import de.z0rdak.yawp.config.server.CommandPermissionConfig;
import de.z0rdak.yawp.config.server.RegionConfig;
import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.area.IMarkableArea;
import de.z0rdak.yawp.core.area.SphereArea;
import de.z0rdak.yawp.core.flag.FlagContainer;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.group.GroupType;
import de.z0rdak.yawp.core.group.PlayerContainer;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.handler.flags.FlagCorrelation;
import de.z0rdak.yawp.handler.flags.HandlerUtil;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.entity.EntityType;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.scoreboard.Team;
import net.minecraft.util.Direction;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.text.*;
import net.minecraft.util.text.event.ClickEvent;
import net.minecraft.util.text.event.HoverEvent;
import net.minecraft.world.World;
import net.minecraft.world.server.ServerWorld;
import net.minecraftforge.fml.server.ServerLifecycleHooks;
import org.apache.commons.lang3.NotImplementedException;

import java.util.*;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.commands.arguments.ArgumentUtil.*;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.getFlagMapRecursive;
import static net.minecraft.util.text.TextFormatting.*;
import static net.minecraft.util.text.event.ClickEvent.Action.*;


public class MessageUtil {

    private MessageUtil() {
    }

    public final static TextFormatting SUGGEST_COLOR = BLUE;
    public final static TextFormatting TP_COLOR = GREEN;
    public final static TextFormatting LINK_COLOR = AQUA;
    public final static TextFormatting INACTIVE_LINK_COLOR = GRAY;
    public final static TextFormatting ADD_CMD_COLOR = DARK_GREEN;
    public final static TextFormatting REMOVE_CMD_COLOR = DARK_RED;
    public static int FIRST_PAGE_IDX = 0;

    public static IFormattableTextComponent buildHeader(String translationKey) {
        return buildHeader(new TranslationTextComponent(translationKey));
    }

    public static String buildTeleportCmd(String tpSource, BlockPos target) {
        return "tp " + tpSource + " " + buildBlockCoordinateStr(target);
    }

    public static String buildBlockCoordinateStr(BlockPos target) {
        return target.getX() + " " + target.getY() + " " + target.getZ();
    }

    public static String shortBlockPos(BlockPos target) {
        return "[X=" + target.getX() + ", Y=" + target.getY() + ", Z=" + target.getZ() + "]";
    }

    public static String buildBlockPosLinkText(BlockPos target) {
        return target.getX() + ", " + target.getY() + ", " + target.getZ();
    }

    public static String buildExecuteCommandString(RegistryKey<World> dim, String command) {
        return "/execute in " + dim.location() + " run " + command;
    }

    public static String buildDimTeleportCmd(RegistryKey<World> dim, String tpSource, BlockPos target) {
        return buildExecuteCommandString(dim, buildTeleportCmd(tpSource, target));
    }

    public static String buildRegionTpCmd(IMarkableRegion region, String target) {
        return buildDimTeleportCmd(region.getDim(), target, region.getTpTarget());
    }

    public static IFormattableTextComponent buildHeader(IFormattableTextComponent header) {
        return new TranslationTextComponent("%s %s %s", TextFormatting.BOLD, header, TextFormatting.BOLD);
    }

    public static void sendMessage(PlayerEntity player, IFormattableTextComponent textComponent) {
        player.sendMessage(textComponent, player.getUUID());
    }

    public static IFormattableTextComponent buildExecuteCmdComponent(String linkText, String hoverText, String command, ClickEvent.Action eventAction, TextFormatting color) {
        IFormattableTextComponent text = TextComponentUtils.wrapInSquareBrackets(new TranslationTextComponent(linkText));
        return text.setStyle(text.getStyle()
                .withColor(color)
                .withClickEvent(new ClickEvent(eventAction, command))
                .withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, new TranslationTextComponent(hoverText))));
    }

    public static IFormattableTextComponent buildExecuteCmdComponent(IFormattableTextComponent linkText, IFormattableTextComponent hoverText, String command, ClickEvent.Action eventAction, TextFormatting color) {
        IFormattableTextComponent text = TextComponentUtils.wrapInSquareBrackets(linkText);
        return text.setStyle(text.getStyle()
                .withColor(color)
                .withClickEvent(new ClickEvent(eventAction, command))
                .withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, hoverText)));
    }

    public static IFormattableTextComponent buildPlayerHoverComponent(PlayerEntity player) {
        HoverEvent.EntityHover entityTooltipInfo = new HoverEvent.EntityHover(EntityType.PLAYER, player.getUUID(), player.getName());
        IFormattableTextComponent playerName = new StringTextComponent(player.getScoreboardName());
        playerName.setStyle(playerName.getStyle()
                .withColor(LINK_COLOR)
                .withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_ENTITY, entityTooltipInfo))
                .withClickEvent(new ClickEvent(SUGGEST_COMMAND, "/tell " + playerName.getString() + " ")));
        return playerName;
    }

    public static IFormattableTextComponent buildTeamHoverComponent(Team team) {
        IFormattableTextComponent teamName = new StringTextComponent(team.getName());
        teamName.setStyle(teamName.getStyle()
                .withColor(LINK_COLOR)
                .withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, new TranslationTextComponent("cli.msg.info.region.group.link.hover")))
                .withClickEvent(new ClickEvent(RUN_COMMAND, "/team list " + team.getName())));
        return teamName;
    }

    /***
     * [X,Y,Z], ..., [X,Y,Z]
     */
    public static IFormattableTextComponent buildAreaMarkedBlocksTpLinks(IMarkableRegion region) {
        List<IFormattableTextComponent> tpLinks = region.getArea().getMarkedBlocks()
                .stream()
                .map(pos -> buildDimensionalBlockTpLink(region.getDim(), pos))
                .collect(Collectors.toList());
        IFormattableTextComponent blockPosTpLinkList = new StringTextComponent("");
        tpLinks.forEach(tpLink -> blockPosTpLinkList.append(tpLink).append(" "));
        return blockPosTpLinkList;
    }

    /**
     * [X,Y,Z]
     */
    public static IFormattableTextComponent buildDimensionalBlockTpLink(RegistryKey<World> dim, BlockPos target) {
        String teleportCmd = buildDimTeleportCmd(dim, "@s", target);
        TranslationTextComponent text = new TranslationTextComponent("cli.msg.info.region.area.tp.block.link.text", buildBlockPosLinkText(target));
        TranslationTextComponent hover = new TranslationTextComponent("cli.msg.info.region.area.tp.block.link.hover");
        return buildExecuteCmdComponent(text, hover, teleportCmd, RUN_COMMAND, TP_COLOR);
    }

    public static IFormattableTextComponent buildTeleportLink(IMarkableRegion region) {
        String executeCmdStr = buildDimTeleportCmd(region.getDim(), "@s", region.getTpTarget());
        TranslationTextComponent text = new TranslationTextComponent("cli.msg.info.region.area.tp.link.text", buildBlockPosLinkText(region.getTpTarget()));
        IFormattableTextComponent hover = new TranslationTextComponent("cli.msg.info.region.area.tp.link.hover", region.getName(), region.getDim().location().toString());
        return buildExecuteCmdComponent(text, hover, executeCmdStr, RUN_COMMAND, TP_COLOR);
    }

    /**
     * [region] @ [X,Y,Z]
     */
    public static IFormattableTextComponent buildDimensionTeleportLink(IMarkableRegion region) {
        String executeCmdStr = buildDimTeleportCmd(region.getDim(), "@s", region.getTpTarget());
        TranslationTextComponent text = new TranslationTextComponent("cli.msg.info.region.area.tp.link.text", buildBlockPosLinkText(region.getTpTarget()));
        IFormattableTextComponent hover = new TranslationTextComponent("cli.msg.info.region.area.tp.link.hover", region.getName(), region.getDim().location().toString());
        return new TranslationTextComponent("%s @ %s",
                buildRegionInfoLink(region),
                buildExecuteCmdComponent(text, hover, executeCmdStr, RUN_COMMAND, TP_COLOR));
    }

    public static IFormattableTextComponent buildRegionAreaDetailComponent(IMarkableRegion region) {
        IMarkableArea area = region.getArea();
        IFormattableTextComponent areaInfo = new StringTextComponent(area.getAreaType().areaType);
        switch (area.getAreaType()) {
            case CUBOID:
                return new TranslationTextComponent("%s, %s", areaInfo, buildCuboidAreaInfo((CuboidArea) area));
            case CYLINDER:
                throw new NotImplementedException("cylinder");
            case SPHERE:
                return new TranslationTextComponent("%s, %s", areaInfo, buildSphereAreaInfo((SphereArea) area));
            case POLYGON_3D:
                throw new NotImplementedException("polygon");
            case PRISM:
                throw new NotImplementedException("prism");
            default:
                throw new IllegalArgumentException("Invalid area type");
        }
    }

    /**
     *  Size: X=69, Y=10, Z=42
     */
    private static IFormattableTextComponent buildCuboidAreaInfo(CuboidArea cuboidArea) {
        return new TranslationTextComponent("cli.msg.info.region.area.area.size.text.cuboid",
                buildAreaAxisInfoComponent(cuboidArea, Direction.Axis.X),
                buildAreaAxisInfoComponent(cuboidArea, Direction.Axis.Y),
                buildAreaAxisInfoComponent(cuboidArea, Direction.Axis.Z));

    }

    /**
     * Center: [X,Y,Z], Radius: 5, Diameter: 11
     */
    private static IFormattableTextComponent buildSphereAreaInfo(SphereArea sphereArea) {
        int diameter = (sphereArea.getRadius() * 2) + 1;
        IFormattableTextComponent centerPos = new StringTextComponent(buildBlockPosLinkText(sphereArea.getCenterPos()));
        return new TranslationTextComponent("cli.msg.info.region.area.area.size.text.sphere",
                buildTextWithHoverMsg(centerPos, centerPos, WHITE), sphereArea.getRadius(), diameter);
    }

    /**
     * Builds component showing size of the area for the given axis with a hover text displaying the block range of the axis.
     * Axis=N, e.g. X=5
     */
    private static IFormattableTextComponent buildAreaAxisInfoComponent(CuboidArea cuboidArea, Direction.Axis axis) {
        int min = (int) Math.floor(cuboidArea.getArea().min(axis));
        int max = (int) Math.floor(cuboidArea.getArea().max(axis));
        String axisName = axis.getName().toUpperCase();
        return buildTextWithHoverMsg(
                new StringTextComponent(axisName + "=" + Math.abs(max - min)),
                new StringTextComponent(axisName + ": " + min + " - " + max), WHITE);
    }

    /**
     * [<=expand=>] [<=max=>]
     */
    public static IFormattableTextComponent buildRegionAreaExpandLink(IMarkableRegion region) {
        IFormattableTextComponent linkText = new TranslationTextComponent("cli.msg.info.region.area.area.expand.link.text");
        IFormattableTextComponent linkHover = new TranslationTextComponent("cli.msg.info.region.area.area.expand.link.hover", region.getName());
        String expandCmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), AREA.toString(), EXPAND.toString(), region.getArea().getAreaType().areaType);
        switch (region.getArea().getAreaType()) {
            case CUBOID: {
                CuboidArea cuboidArea = (CuboidArea) region.getArea();
                int areaLowerLimit = (int) Math.floor(cuboidArea.getArea().minZ);
                int areaUpperLimit = (int) Math.floor(cuboidArea.getArea().maxZ);
                // [<=expand=>]
                String expandCmdSuggestion = appendSubCommand(expandCmd, String.valueOf(areaLowerLimit), String.valueOf(areaUpperLimit));
                IFormattableTextComponent expandLink = buildExecuteCmdComponent(linkText, linkHover, expandCmdSuggestion, SUGGEST_COMMAND, LINK_COLOR);
                // [<=max=>]
                IFormattableTextComponent maxExpandLinkText = new TranslationTextComponent("cli.msg.info.region.area.area.expand-max.link.text");
                IFormattableTextComponent maxExpandLinkHover = new TranslationTextComponent("cli.msg.info.region.area.area.expand-max.link.hover");
                String maxExpandCmd = appendSubCommand(expandCmd, String.valueOf(RegionCommands.MIN_BUILD_LIMIT), String.valueOf(RegionCommands.MAX_BUILD_LIMIT));
                IFormattableTextComponent maxExpandLink = buildExecuteCmdComponent(maxExpandLinkText, maxExpandLinkHover, maxExpandCmd, RUN_COMMAND, LINK_COLOR);
                return new TranslationTextComponent("%s %s", expandLink, maxExpandLink);
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

    private static IFormattableTextComponent buildShowAreaToggleLink(IMarkableRegion region) {
        TranslationTextComponent showAreaLinkText = new TranslationTextComponent("cli.msg.info.region.area.area.show.link");
        TranslationTextComponent showAreaLinkHover = new TranslationTextComponent("cli.msg.info.region.area.area.show.hover", region.getName());
        String showAreaCmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), AREA.toString(), "show");
        return buildExecuteCmdComponent(showAreaLinkText, showAreaLinkHover, showAreaCmd, RUN_COMMAND, LINK_COLOR);
    }

    /**
     * [set area]
     */
    public static IFormattableTextComponent buildAreaUpdateLink(IMarkableRegion region) {
        TranslationTextComponent setAreaLinkText = new TranslationTextComponent("cli.msg.info.region.area.area.set.link");
        TranslationTextComponent setAreaLinkHover = new TranslationTextComponent("cli.msg.info.region.area.area.set.hover", region.getName());
        String blocks = String.join(" ", region.getArea().getMarkedBlocks().stream()
                .map(MessageUtil::buildBlockCoordinateStr)
                .collect(Collectors.toSet()));
        String setAreaCmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), AREA.toString(), SET.toString(), region.getArea().getAreaType().areaType, blocks);
        return buildExecuteCmdComponent(setAreaLinkText, setAreaLinkHover, setAreaCmd, SUGGEST_COMMAND, LINK_COLOR);
    }

    /**
     * [set area] [set TP] [show area] [<=expand=>] [<=max=>]
     */
    public static IFormattableTextComponent buildRegionAreaActionLinks(IMarkableRegion region) {
        return new TranslationTextComponent("%s %s %s", buildAreaUpdateLink(region), buildRegionSetTpLink(region), buildRegionAreaExpandLink(region));
        // buildShowAreaToggleLink(region)
    }

    public static IFormattableTextComponent buildTextWithHoverMsg(IFormattableTextComponent text, IFormattableTextComponent hoverText, TextFormatting color) {
        IFormattableTextComponent bracketedText = TextComponentUtils.wrapInSquareBrackets(text);
        bracketedText.setStyle(bracketedText.getStyle().withColor(color).withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, hoverText)));
        return bracketedText;
    }

    public static IFormattableTextComponent buildHelpStartComponent() {
        String command = ArgumentUtil.buildCommandStr(CommandConstants.GLOBAL.toString(), CommandConstants.INFO.toString());
        IFormattableTextComponent text = new TranslationTextComponent("help.hint.link.text");
        IFormattableTextComponent hover = new TranslationTextComponent("help.hint.link.hover", CommandPermissionConfig.BASE_CMD);
        return buildExecuteCmdComponent(text, hover, command, ClickEvent.Action.RUN_COMMAND, LINK_COLOR);
    }

    public static IFormattableTextComponent buildWikiLink() {
        IFormattableTextComponent wikiLinkHover = new TranslationTextComponent("help.tooltip.wiki.link.hover");
        IFormattableTextComponent wikiLink = new TranslationTextComponent("help.tooltip.wiki.link.text");
        return buildExecuteCmdComponent(wikiLink, wikiLinkHover, "https://github.com/Z0rdak/Yet-Another-World-Protector/wiki", OPEN_URL, AQUA);
    }

    public static IFormattableTextComponent buildHelpSuggestionLink(String translationKey, CommandConstants baseCmd, CommandConstants cmd) {
        String command = "/" + CommandPermissionConfig.BASE_CMD + " " + baseCmd + " " + cmd + " ";
        return new TranslationTextComponent(" %s %s",
                buildExecuteCmdComponent("=>", "chat.link.hover.command.copy", command, SUGGEST_COMMAND, LINK_COLOR),
                new TranslationTextComponent(translationKey));
    }

    public static IFormattableTextComponent buildRegionEnableComponent(IProtectedRegion region) {
        String linkTextKey = "cli.msg.info.region.state.enable." + region.isActive() + ".link.text";
        String hoverTextKey = "cli.msg.info.region.state.enable." + !region.isActive() + ".link.hover";
        TextFormatting color = region.isActive() ? ADD_CMD_COLOR : REMOVE_CMD_COLOR;
        switch (region.getRegionType()) {
            case GLOBAL: {
                String cmd = ArgumentUtil.buildCommandStr(GLOBAL.toString(), STATE.toString(), ENABLE.toString());
                return buildExecuteCmdComponent(linkTextKey, hoverTextKey, cmd, RUN_COMMAND, color);
            }
            case DIMENSION: {
                String cmd = ArgumentUtil.buildCommandStr(DIM.toString(), region.getDim().location().toString(), STATE.toString(), ENABLE.toString());
                return buildExecuteCmdComponent(linkTextKey, hoverTextKey, cmd, RUN_COMMAND, color);
            }
            case LOCAL: {
                String cmd = ArgumentUtil.buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), STATE.toString(), ENABLE.toString());
                return buildExecuteCmdComponent(linkTextKey, hoverTextKey, cmd, RUN_COMMAND, color);
            }
            default:
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        }
    }

    public static IFormattableTextComponent buildAllLocalEnableComponent(DimensionRegionCache dimCache) {
        String enableLinkTextKey = "cli.msg.info.region.state.alert.all.true.link.text";
        String enableHoverTextKey = "cli.msg.info.region.state.alert.all.true.link.hover";
        String disableLinkTextKey = "cli.msg.info.region.state.alert.all.false.link.text";
        String disableHoverTextKey = "cli.msg.info.region.state.alert.all.false.link.hover";
        String enableCmd = ArgumentUtil.buildCommandStr(DIM.toString(), dimCache.dimensionKey().location().toString(), STATE.toString(), ENABLE_LOCAL.toString(), Boolean.TRUE.toString());
        String disableCmd = ArgumentUtil.buildCommandStr(DIM.toString(), dimCache.dimensionKey().location().toString(), STATE.toString(), ENABLE_LOCAL.toString(), Boolean.FALSE.toString());
        IFormattableTextComponent activeAlertLink = buildExecuteCmdComponent(enableLinkTextKey, enableHoverTextKey, enableCmd, RUN_COMMAND, ADD_CMD_COLOR);
        IFormattableTextComponent disableAlertLink = buildExecuteCmdComponent(disableLinkTextKey, disableHoverTextKey, disableCmd, RUN_COMMAND, REMOVE_CMD_COLOR);
        return new TranslationTextComponent("%s %s", activeAlertLink, disableAlertLink);
    }

    public static IFormattableTextComponent buildAllLocalAlertToggleLink(DimensionRegionCache dimCache) {
        String enableLinkTextKey = "cli.msg.info.region.state.enable.all.true.link.text";
        String enableHoverTextKey = "cli.msg.info.region.state.enable.all.true.link.hover";
        String disableLinkTextKey = "cli.msg.info.region.state.enable.all.false.link.text";
        String disableHoverTextKey = "cli.msg.info.region.state.enable.all.false.link.hover";
        String enableCmd = ArgumentUtil.buildCommandStr(DIM.toString(), dimCache.dimensionKey().location().toString(), STATE.toString(), ALERT_LOCAL.toString(), Boolean.TRUE.toString());
        String disableCmd = ArgumentUtil.buildCommandStr(DIM.toString(), dimCache.dimensionKey().location().toString(), STATE.toString(), ALERT_LOCAL.toString(), Boolean.FALSE.toString());
        IFormattableTextComponent activeAlertLink = buildExecuteCmdComponent(enableLinkTextKey, enableHoverTextKey, enableCmd, RUN_COMMAND, ADD_CMD_COLOR);
        IFormattableTextComponent disableAlertLink = buildExecuteCmdComponent(disableLinkTextKey, disableHoverTextKey, disableCmd, RUN_COMMAND, REMOVE_CMD_COLOR);
        return new TranslationTextComponent("%s %s", activeAlertLink, disableAlertLink);
    }

    public static IFormattableTextComponent buildRegionPriorityComponent(IMarkableRegion region) {
        int defaultPriorityInc = RegionConfig.getDefaultPriorityInc();
        String incPriorityCmd = ArgumentUtil.buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), STATE.toString(), PRIORITY.toString(), INC.toString(), String.valueOf(defaultPriorityInc));
        IFormattableTextComponent incLinkText = new TranslationTextComponent("cli.msg.info.region.state.priority.increase.link.text", defaultPriorityInc);
        IFormattableTextComponent incHoverText = new TranslationTextComponent("cli.msg.info.region.state.priority.increase.link.hover", defaultPriorityInc);
        IFormattableTextComponent increaseLink = buildExecuteCmdComponent(incLinkText, incHoverText, incPriorityCmd, RUN_COMMAND, ADD_CMD_COLOR);
        String decPriorityCmd = ArgumentUtil.buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), STATE.toString(), PRIORITY.toString(), DEC.toString(), String.valueOf(defaultPriorityInc));
        IFormattableTextComponent decLinkText = new TranslationTextComponent("cli.msg.info.region.state.priority.decrease.link.text", defaultPriorityInc);
        IFormattableTextComponent decHoverText = new TranslationTextComponent("cli.msg.info.region.state.priority.decrease.link.hover", defaultPriorityInc);
        IFormattableTextComponent decreaseLink = buildExecuteCmdComponent(decLinkText, decHoverText, decPriorityCmd, RUN_COMMAND, REMOVE_CMD_COLOR);
        String setPriorityCmd = ArgumentUtil.buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), STATE.toString(), PRIORITY.toString(), "");
        IFormattableTextComponent setPriorityLinkText = new TranslationTextComponent("cli.msg.info.region.state.priority.set.link.text", region.getPriority());
        IFormattableTextComponent setPriorityHoverText = new TranslationTextComponent("cli.msg.info.region.state.priority.set.link.hover");
        IFormattableTextComponent setPriorityLink = buildExecuteCmdComponent(setPriorityLinkText, setPriorityHoverText, setPriorityCmd, SUGGEST_COMMAND, LINK_COLOR);
        return new TranslationTextComponent("%s %s %s", setPriorityLink, increaseLink, decreaseLink);
    }

    public static IFormattableTextComponent buildRegionAlertToggleLink(IProtectedRegion region) {
        String linkTextKey = "cli.msg.info.region.state.alert." + !region.isMuted() + ".link.text";
        String hoverTextKey = "cli.msg.info.region.state.alert." + region.isMuted() + ".link.hover";
        TextFormatting color = region.isMuted() ? REMOVE_CMD_COLOR : ADD_CMD_COLOR;
        switch (region.getRegionType()) {
            case GLOBAL: {
                String cmd = buildCommandStr(GLOBAL.toString(), STATE.toString(), ALERT.toString(), Boolean.toString(!region.isMuted()));
                return buildExecuteCmdComponent(linkTextKey, hoverTextKey, cmd, RUN_COMMAND, color);
            }
            case DIMENSION: {
                String cmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), STATE.toString(), ALERT.toString(), Boolean.toString(!region.isMuted()));
                return buildExecuteCmdComponent(linkTextKey, hoverTextKey, cmd, RUN_COMMAND, color);
            }
            case LOCAL: {
                String cmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), STATE.toString(), ALERT.toString(), Boolean.toString(!region.isMuted()));
                return buildExecuteCmdComponent(linkTextKey, hoverTextKey, cmd, RUN_COMMAND, color);
            }
            default:
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        }
    }

    public static IFormattableTextComponent buildRegionRenameLink(IProtectedRegion region) {
        String cmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), RENAME.toString(), "");
        IFormattableTextComponent text = new TranslationTextComponent("cli.msg.info.region.state.rename.link.text");
        IFormattableTextComponent hover = new TranslationTextComponent("cli.msg.info.region.state.rename.link.hover", region.getName());
        return buildExecuteCmdComponent(text, hover, cmd, SUGGEST_COMMAND, LINK_COLOR);
    }

    public static IFormattableTextComponent buildRegionInfoLink(IProtectedRegion region) {
        return buildRegionInfoLink(region, new TranslationTextComponent("cli.msg.info.region.link.hover", region.getName()));
    }

    public static IFormattableTextComponent buildRegionInfoLink(IProtectedRegion region, IFormattableTextComponent linkText, IFormattableTextComponent hoverText) {
        switch (region.getRegionType()) {
            case GLOBAL: {
                String command = buildCommandStr(GLOBAL.toString(), INFO.toString());
                return buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, LINK_COLOR);
            }
            case DIMENSION: {
                String command = buildCommandStr(DIM.toString(), region.getDim().location().toString(), INFO.toString());
                return buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, LINK_COLOR);
            }
            case LOCAL: {
                String cmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), INFO.toString());
                return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            default:
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        }
    }

    public static IFormattableTextComponent buildRegionInfoLink(IProtectedRegion region, IFormattableTextComponent hoverText) {
        IFormattableTextComponent linkText = new StringTextComponent(region.getName());
        return buildRegionInfoLink(region, linkText, hoverText);
    }

    public static IFormattableTextComponent buildRegionAreaLink(IMarkableRegion region) {
        String showSpatialPropLink = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), AREA.toString());
        IFormattableTextComponent spatialPropLinkText = new TranslationTextComponent("cli.msg.info.region.area.link.text");
        IFormattableTextComponent spatialPropHoverText = new TranslationTextComponent("cli.msg.info.region.area.link.hover", region.getName());
        return buildExecuteCmdComponent(spatialPropLinkText, spatialPropHoverText, showSpatialPropLink, RUN_COMMAND, LINK_COLOR);
    }

    public static IFormattableTextComponent buildRegionOverviewHeader(IProtectedRegion region) {
        switch (region.getRegionType()) {
            case GLOBAL: {
                IFormattableTextComponent clipBoardDumpLink = buildExecuteCmdComponent("cli.msg.global.overview.header.dump.link.text", "cli.msg.global.overview.header.dump.link.hover", region.serializeNBT().getPrettyDisplay().getString(), ClickEvent.Action.COPY_TO_CLIPBOARD, GOLD);
                return buildHeader(new TranslationTextComponent("cli.msg.info.header.for", clipBoardDumpLink, buildRegionInfoLink(region)));
            }
            case DIMENSION: {
                IFormattableTextComponent clipBoardDumpLink = buildExecuteCmdComponent("cli.msg.dim.overview.header.dump.link.text", "cli.msg.dim.overview.header.dump.link.hover", region.serializeNBT().getPrettyDisplay().getString(), ClickEvent.Action.COPY_TO_CLIPBOARD, GOLD);
                return buildHeader(new TranslationTextComponent("cli.msg.info.header.for", clipBoardDumpLink, buildRegionInfoLink(region)));
            }
            case LOCAL: {
                IFormattableTextComponent clipBoardDumpLink = buildExecuteCmdComponent("cli.msg.local.overview.header.dump.link.text", "cli.msg.local.overview.header.dump.link.hover", region.serializeNBT().getPrettyDisplay().getString(), ClickEvent.Action.COPY_TO_CLIPBOARD, GOLD);
                return buildHeader(new TranslationTextComponent("cli.msg.info.header.for", clipBoardDumpLink, buildRegionInfoLink(region)));
            }
            default:
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        }
    }

    /**
     * Players: [n player(s)] [+]
     */
    public static IFormattableTextComponent buildPlayerListLink(IProtectedRegion region, PlayerContainer players, String group) {
        IFormattableTextComponent hoverText = new TranslationTextComponent("cli.msg.info.region.group.player.list.link.hover", group, region.getName());
        IFormattableTextComponent linkText = new TranslationTextComponent("cli.msg.info.region.group.player.list.link.text", players.getPlayers().size());
        switch (region.getRegionType()) {
            case GLOBAL: {
                String cmd = buildCommandStr(GLOBAL.toString(), LIST.toString(), GROUP.toString(), group, PLAYER.toString());
                return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            case DIMENSION: {
                String cmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), LIST.toString(), GROUP.toString(), group, PLAYER.toString());
                return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            case LOCAL: {
                String cmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), GROUP.toString(), group, PLAYER.toString());
                return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            default:
                throw new IllegalArgumentException();
        }
    }

    /**
     * Teams: [n team(s)] [+]
     */
    public static IFormattableTextComponent buildTeamListLink(IProtectedRegion region, PlayerContainer teams, String group) {
        IFormattableTextComponent hoverText = new TranslationTextComponent("cli.msg.info.region.group.team.list.link.hover", group, region.getName());
        IFormattableTextComponent linkText = new TranslationTextComponent("cli.msg.info.region.group.team.list.link.text", teams.getTeams().size());
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
                String cmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), GROUP.toString(), group, TEAM.toString());
                return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            default:
                throw new IllegalArgumentException();
        }
    }

    public static IFormattableTextComponent buildAddToGroupLink(IProtectedRegion region, String group, GroupType groupType) {
        IFormattableTextComponent linkText = new TranslationTextComponent("cli.link.add");
        IFormattableTextComponent hoverText = new TranslationTextComponent("cli.msg.info.region.group." + groupType.name + ".add.link.hover", group, region.getName());
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

    public static IFormattableTextComponent buildGroupLinks(IProtectedRegion region) {
        return getGroupsForRegion(region).stream()
                .map(group -> buildGroupLink(region, group, getGroupSize(region, group)))
                .reduce(new StringTextComponent(""), (link1, link2) -> link1.append(" ").append(link2));
    }

    public static List<String> getGroupsForRegion(IProtectedRegion region) {
        return CommandUtil.GROUP_LIST;
    }

    private static int getGroupSize(IProtectedRegion region, String groupName) {
        PlayerContainer group = region.getGroup(groupName);
        return group.getPlayers().size() + group.getTeams().size();
    }

    public static IFormattableTextComponent buildGroupHeader(IProtectedRegion region, String group) {
        IFormattableTextComponent groupLink = buildGroupLink(region, group, getGroupSize(region, group));
        return buildHeader(new TranslationTextComponent("cli.msg.info.header.in", groupLink, buildRegionInfoLink(region)));
    }

    public static IFormattableTextComponent buildGroupHeader(IProtectedRegion region, String group, GroupType groupType) {
        return new TranslationTextComponent("cli.msg.info.region.group." + groupType.name + ".list", buildRegionInfoLink(region), group);
    }

    public static IFormattableTextComponent buildGroupLink(IProtectedRegion region, String group, int groupSie) {
        IFormattableTextComponent linkText = new TranslationTextComponent("cli.msg.info.region.group.list.link.text", groupSie, group);
        IFormattableTextComponent hoverText = new TranslationTextComponent("cli.msg.info.region.group.list.link.hover", group, region.getName());
        switch (region.getRegionType()) {
            case GLOBAL: {
                String cmd = buildCommandStr(GLOBAL.toString(), LIST.toString(), GROUP.toString(), group);
                return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            case LOCAL: {
                String cmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), GROUP.toString(), group);
                return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            case DIMENSION: {
                String cmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), LIST.toString(), GROUP.toString(), group);
                return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            default:
                throw new IllegalArgumentException();
        }
    }

    public static IFormattableTextComponent buildGroupTeamListLink(IProtectedRegion region, String group) {
        // Teams: [n team(s)] [+]
        PlayerContainer playerContainer = region.getGroup(group);
        IFormattableTextComponent teamAddLink = buildAddToGroupLink(region, group, GroupType.TEAM);
        IFormattableTextComponent teamListLink = playerContainer.hasTeams()
                ? buildTeamListLink(region, playerContainer, group)
                : new TranslationTextComponent("cli.msg.info.region.group.team.list.link.text", playerContainer.getTeams().size());
        return new TranslationTextComponent("%s: %s %s",
                new TranslationTextComponent("cli.msg.info.region.group.team"),
                teamListLink,
                teamAddLink);
    }

    public static IFormattableTextComponent buildGroupPlayerListLink(IProtectedRegion region, String group) {
        // Players: [n player(s)] [+]
        PlayerContainer playerContainer = region.getGroup(group);
        IFormattableTextComponent playersAddLink = buildAddToGroupLink(region, group, GroupType.PLAYER);
        IFormattableTextComponent playerListLink = playerContainer.hasPlayers()
                ? buildPlayerListLink(region, playerContainer, group)
                : new TranslationTextComponent("cli.msg.info.region.group.player.list.link.text", playerContainer.getPlayers().size());
        return new TranslationTextComponent("%s: %s %s",
                new TranslationTextComponent("cli.msg.info.region.group.player"),
                playerListLink,
                playersAddLink);
    }

    /**
     * Creates a TextComponent for flag removal, followed by the flag infos
     * @return - [x] [flagname] [<region-indicator] [] []
     */
    private static IFormattableTextComponent buildRemoveFlagEntry(IProtectedRegion region, IFlag flag, TextFormatting flagLinkColor, TextFormatting... formattings) {
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
                cmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), REMOVE.toString(), FLAG.toString(), flag.getName());
                break;
            }
            default:
                throw new IllegalArgumentException();
        }
        IFormattableTextComponent hoverText = new TranslationTextComponent("cli.msg.info.region.flag.remove.link.hover", flag.getName(), region.getName());
        IFormattableTextComponent linkText = new TranslationTextComponent("cli.link.remove");
        IFormattableTextComponent flagRemoveLink = buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, REMOVE_CMD_COLOR);
        IFormattableTextComponent flagQuickActionComponent = buildFlagQuickActionComponent(region, flag, flagLinkColor);
        flagQuickActionComponent.withStyle(formattings);
        return new TranslationTextComponent(" - %s %s", flagRemoveLink, flagQuickActionComponent);
    }

    /**
     * Creates a TextComponent with a Link for displaying the flag info. <br></br>
     * Text: [flagname] [+]|[#] [!] [$] <br></br>
     * Where <br></br>
     * - [!] is a quick link to toggle the flag active state, <br></br>
     * - [s] is a suggest link to change the flag state, <br></br>
     * - [m] is a quick link to toggle the flag override state, <br></br>
     * - [o] is a quick link to toggle the flag mute state, <br></br>
     * FIXME: Regiontype indicator is still D even though the flags are from G
     * @param region
     * @param flag
     * @return text component for quick flag actions [flagname] [regionTypeIdentifier] [s] [m] [o]
     */
    private static IFormattableTextComponent buildFlagQuickActionComponent(IProtectedRegion region, IFlag flag, TextFormatting flagLinkColor) {
        IFormattableTextComponent regionTypeIndicator = new StringTextComponent(region.getRegionType().type.substring(0, 1).toUpperCase());
        IFormattableTextComponent hoverText = new TranslationTextComponent("cli.flag.info.hover", flag.getName(), region.getName());
        IFormattableTextComponent flagInfoLink = buildFlagInfoLink(region, flag, flagLinkColor);
        return new TranslationTextComponent("%s %s %s %s %s",
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
    public static IFormattableTextComponent buildFlagInfoLink(IProtectedRegion region, IFlag flag, TextFormatting linkColor) {
        IFormattableTextComponent text = new StringTextComponent(flag.getName());
        IFormattableTextComponent hoverText = new TranslationTextComponent("cli.flag.info.hover", flag.getName(), region.getName());
        return buildFlagInfoLink(region, flag, text, hoverText, linkColor);
    }

    public static IFormattableTextComponent buildFlagInfoLink(IProtectedRegion region, IFlag flag, IFormattableTextComponent text, IFormattableTextComponent hoverText, TextFormatting linkColor) {
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

    public static IFormattableTextComponent buildFlagInfoLink(IProtectedRegion region, IFlag flag) {
        return buildFlagInfoLink(region, flag, LINK_COLOR);
    }


    /**
     * Creates a TextComponent for toggling the flag state between allow and deny <br></br>
     */
    public static IFormattableTextComponent buildFlagStateToggleLink(IProtectedRegion region, IFlag flag) {
        switch (region.getRegionType()) {
            case GLOBAL: {
                String cmd = buildCommandStr(FLAG.toString(), GLOBAL.toString(), flag.getName());
                return buildFlagToggleLink(cmd, "state", flag.isActive(), STATE.toString());
            }
            case DIMENSION: {
                String cmd = buildCommandStr(FLAG.toString(), DIM.toString(), region.getDim().location().toString(), flag.getName());
                return buildFlagToggleLink(cmd, "state", flag.isActive(), STATE.toString());
            }
            case LOCAL: {
                String cmd = buildCommandStr(FLAG.toString(), CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), flag.getName());
                return buildFlagToggleLink(cmd, "state", flag.isActive(), STATE.toString());
            }
            case TEMPLATE:
                throw new NotImplementedException("No supported yet");
            default:
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        }
    }

    public static IFormattableTextComponent buildFlagStateComponent(IProtectedRegion region, IFlag flag) {
        FlagState state = flag.getState();
        IFormattableTextComponent text = new StringTextComponent(state.name());
        IFormattableTextComponent hover = StringTextComponent.EMPTY.plainCopy();
        TextFormatting color = WHITE;
        switch (state) {
            case ALLOWED:
                color = GREEN;
                hover = new TranslationTextComponent("cli.flag.state.allowed.info.hover");
                break;
            case DENIED:
                color = RED;
                hover = new TranslationTextComponent("cli.flag.state.denied.info.hover");
                break;
            case DISABLED:
                color = GRAY;
                hover = new TranslationTextComponent("cli.flag.state.disabled.info.hover");
                break;
        }
        IFormattableTextComponent stateInfo = buildTextWithHoverMsg(text, hover, color);
        return new TranslationTextComponent("%s %s", stateInfo, buildFlagStateSuggestionLink(region, flag));
    }

    public static IFormattableTextComponent buildFlagStateSuggestionLink(IProtectedRegion region, IFlag flag) {
        IFormattableTextComponent hover = new TranslationTextComponent("cli.flag.state.set.link.hover", flag.getName(), region.getName());
        IFormattableTextComponent text = new TranslationTextComponent("cli.flag.state.set.link.text");
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

    public static IFormattableTextComponent buildFlagOverrideInfoComponent(IFlag flag) {
        IFormattableTextComponent linkText = new TranslationTextComponent("cli.flag.msg.text.link.text." + flag.doesOverride());
        IFormattableTextComponent hoverText = new TranslationTextComponent("cli.flag.msg.text.link.hover." + flag.doesOverride());
        TextFormatting color = flag.doesOverride() ? TextFormatting.GREEN : TextFormatting.GRAY;
        return buildTextWithHoverMsg(linkText, hoverText, color);
    }


    public static IFormattableTextComponent buildFlagOverrideToggleLink(IProtectedRegion region, IFlag flag, boolean shortLink) {
        IFormattableTextComponent linkText = new TranslationTextComponent("cli.flag.override.link.text." + flag.doesOverride());
        IFormattableTextComponent hoverText = new TranslationTextComponent("cli.flag.override.link.hover." + !flag.doesOverride(), flag.getName(), region.getName());
        if (shortLink) {
            linkText = new TranslationTextComponent("cli.flag.override.link.text.toggle");
        }
        TextFormatting color = flag.doesOverride() ? TextFormatting.GREEN : TextFormatting.GRAY;
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

    public static IFormattableTextComponent buildFlagMessageEditLink(IProtectedRegion region, IFlag flag) {
        IFormattableTextComponent hover = new TranslationTextComponent("cli.flag.msg.text.set.link.hover", flag.getName(), region.getName());
        IFormattableTextComponent text = new TranslationTextComponent("cli.flag.msg.text.set.link.text");
        switch (region.getRegionType()) {
            case GLOBAL: {
                String cmd = buildCommandStr(FLAG.toString(), GLOBAL.toString(), flag.getName(), MSG.toString(), SET.toString());
                return buildExecuteCmdComponent(text, hover, cmd, SUGGEST_COMMAND, LINK_COLOR);
            }
            case DIMENSION: {
                String cmd = buildCommandStr(FLAG.toString(), DIM.toString(), region.getDim().location().toString(), flag.getName(), MSG.toString(), SET.toString());
                return buildExecuteCmdComponent(text, hover, cmd, SUGGEST_COMMAND, LINK_COLOR);
            }
            case LOCAL: {
                String cmd = buildCommandStr(FLAG.toString(), CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), flag.getName(), MSG.toString(), SET.toString());
                return buildExecuteCmdComponent(text, hover, cmd, SUGGEST_COMMAND, LINK_COLOR);
            }
            default:
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        }
    }

    private static IFormattableTextComponent buildFlagMessageClearLink(IProtectedRegion region, IFlag flag) {
        IFormattableTextComponent hover = new TranslationTextComponent("cli.flag.msg.text.set.default", flag.getName(), region.getName());
        IFormattableTextComponent text = new TranslationTextComponent("cli.link.remove");
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

    public static IFormattableTextComponent buildFlagMuteToggleLink(IProtectedRegion region, IFlag flag, boolean shortLink) {
        IFormattableTextComponent hover = new TranslationTextComponent("cli.flag.msg.mute.set.link.hover", flag.getName(), region.getName());
        IFormattableTextComponent text = new TranslationTextComponent("cli.flag.msg.mute.set.link.text." + flag.getFlagMsg().isMuted());
        if (shortLink) {
            text = new TranslationTextComponent("cli.flag.msg.mute.set.link.text.toggle");
        }
        TextFormatting textFormatting = flag.getFlagMsg().isMuted() ? GREEN : GRAY;
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

    public static IFormattableTextComponent buildFlagMessageHoverText(IProtectedRegion region, IFlag flag) {
        String flagMsg = flag.getFlagMsg().getMsg();
        if (flag.getFlagMsg().getMsg().length() > 30) {
            flagMsg = flagMsg.substring(0, 30) + "...";
        }
        IFormattableTextComponent flagMsgText = new StringTextComponent(flagMsg);
        IFormattableTextComponent hoverText = new StringTextComponent(flag.getFlagMsg().getMsg());
        // if flag has default msg, use default msg
        if (flag.getFlagMsg().isDefault()) {
            switch (region.getRegionType()) {
                case GLOBAL: {
                    hoverText = new TranslationTextComponent("flag.player.msg.push.deny.global.default");
                    break;
                }
                case DIMENSION: {
                    hoverText = new TranslationTextComponent("flag.player.msg.push.deny.dim.default");
                    break;
                }
                case LOCAL: {
                    hoverText = new TranslationTextComponent("flag.player.msg.push.deny.local.default");
                    break;
                }
            }
        }
        return buildTextWithHoverMsg(flagMsgText, hoverText, WHITE);
    }

    /**
     * Message: [set] [x]: 'msg' <br></br>
     */
    public static IFormattableTextComponent buildFlagMessageComponent(IProtectedRegion region, IFlag flag) {
        IFormattableTextComponent editLink = buildFlagMessageEditLink(region, flag);
        IFormattableTextComponent clearLink = buildFlagMessageClearLink(region, flag);
        IFormattableTextComponent flagMsgTextWithHover = buildFlagMessageHoverText(region, flag);
        return new TranslationTextComponent("%s %s '%s'", editLink, clearLink, flagMsgTextWithHover);
    }

    private static IFormattableTextComponent buildFlagToggleLink(String cmd, String langKey, boolean toggleActiveState, String... subCmds) {
        String cmdStr = appendSubCommand(cmd, subCmds);
        IFormattableTextComponent linkText = new TranslationTextComponent("cli.flag." + langKey + ".link.text");
        IFormattableTextComponent hoverText = new TranslationTextComponent("cli.flag." + langKey + ".link.hover");
        TextFormatting color = toggleActiveState ? ADD_CMD_COLOR : REMOVE_CMD_COLOR;
        return buildExecuteCmdComponent(linkText, hoverText, cmdStr, RUN_COMMAND, color);
    }

    /**
     * Show flags for region, with a color of their state
     * But also show parent flags in italic
     * How do we want to handle flags defined in the region but also in the parent?
     * If the child flag is dominant, we display the child flag, and add a hint to the parent
     * If the flag is overriden, we format them with strikethrough and add a link to the parent which overrides it
     */
    public static List<IFormattableTextComponent> buildRemoveFlagEntries(IProtectedRegion region) {
        List<IFormattableTextComponent> flagEntries = new ArrayList<>();
        Map<String, FlagCorrelation> flagMapRecursive = getFlagMapRecursive(region, null);
        Map<FlagState, List<FlagCorrelation>> flagStateListMap = LocalRegions.sortFlagsByState(flagMapRecursive);
        flagEntries.addAll(buildFlagEntries(flagStateListMap, FlagState.ALLOWED));
        flagEntries.addAll(buildFlagEntries(flagStateListMap, FlagState.DENIED));
        flagEntries.addAll(buildFlagEntries(flagStateListMap, FlagState.DISABLED));
        return flagEntries;
    }

    private static List<IFormattableTextComponent> buildFlagEntries(Map<FlagState, List<FlagCorrelation>> flagStateListMap, FlagState state) {
        List<FlagCorrelation> allowedFlags = flagStateListMap.get(state);
        allowedFlags.sort(Comparator.comparing(flagCorrelation -> flagCorrelation.getFlag().getName()));
        return allowedFlags.stream()
                .map(flagCorrelation -> buildRemoveFlagEntry(flagCorrelation.getRegion(), flagCorrelation.getFlag(), colorForState(state)))
                .collect(Collectors.toList());
    }

    private static TextFormatting colorForState(FlagState state) {
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

    public static List<IFormattableTextComponent> buildRemoveRegionEntries(IProtectedRegion parent, List<IProtectedRegion> regions) {
        return regions.stream()
                .map(region -> buildRemoveRegionEntry(parent, region))
                .collect(Collectors.toList());
    }

    /**
     * Builds a TextComponent for the given flag and region. <br></br>
     * Currently not used in the CLI for obvious reasons. <br></br>
     */
    public static IFormattableTextComponent buildRemoveAllRegionsAttemptLink(DimensionRegionCache dimCache) {
        String cmd = buildCommandStr(DIM.toString(), dimCache.dimensionKey().location().toString(), DELETE_ALL.toString(), REGIONS.toString());
        IFormattableTextComponent hover = new TranslationTextComponent("cli.msg.info.dim.region.remove.all.link.hover", dimCache.getDimensionalRegion().getName());
        IFormattableTextComponent text = new TranslationTextComponent("cli.link.remove");
        return buildExecuteCmdComponent(text, hover, cmd, RUN_COMMAND, REMOVE_CMD_COLOR);
    }

    public static IFormattableTextComponent buildRemoveAllRegionsLink(DimensionRegionCache dimCache) {
        String cmd = buildCommandStr(DIM.toString(), dimCache.dimensionKey().location().toString(), DELETE_ALL.toString(), REGIONS.toString(), FOREVER.toString(), SERIOUSLY.toString());
        IFormattableTextComponent hover = new TranslationTextComponent("cli.msg.info.dim.region.remove.all.link.hover", dimCache.getDimensionalRegion().getName());
        IFormattableTextComponent text = new TranslationTextComponent("cli.link.remove");
        return buildExecuteCmdComponent(text, hover, cmd, RUN_COMMAND, REMOVE_CMD_COLOR);
    }


    public static IFormattableTextComponent buildRemoveRegionLink(IProtectedRegion region) {
        String cmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), DELETE.toString(), region.getName(), "-y");
        IFormattableTextComponent hover = new TranslationTextComponent("cli.msg.info.dim.region.remove.link.hover", region.getName());
        IFormattableTextComponent text = new TranslationTextComponent("cli.link.remove");
        return buildExecuteCmdComponent(text, hover, cmd, RUN_COMMAND, REMOVE_CMD_COLOR);
    }

    public static IFormattableTextComponent buildRemoveRegionEntry(IProtectedRegion parent, IProtectedRegion region) {
        IFormattableTextComponent regionRemoveLink;
        switch (parent.getRegionType()) {
            case GLOBAL: {
                regionRemoveLink = new TranslationTextComponent("%s %s", buildDimResetComponent((DimensionalRegion) region), buildRegionInfoLink(region));
                break;
            }
            case DIMENSION: {
                IFormattableTextComponent removeLink = new StringTextComponent("");
                IFormattableTextComponent childIndicator = buildTextWithHoverMsg(new StringTextComponent("*"), new TranslationTextComponent("cli.msg.info.dim.region.child.hover"), GOLD);
                if (parent.hasChild(region)) {
                    removeLink = new TranslationTextComponent("%s %s%s", buildDimSuggestRegionRemovalLink((IMarkableRegion) region), buildRegionInfoLink(region), childIndicator);
                } else {
                    removeLink = new TranslationTextComponent("%s %s", buildDimSuggestRegionRemovalLink((IMarkableRegion) region), buildRegionInfoLink(region));
                }
                regionRemoveLink = new TranslationTextComponent("%s @ %s", removeLink, buildRegionTeleportLink((IMarkableRegion) region));
                break;
            }
            case LOCAL: {
                regionRemoveLink = new TranslationTextComponent("%s %s", buildRegionRemoveChildLink(parent, region), buildRegionInfoLink(region));
                break;
            }
            default:
                throw new IllegalArgumentException();
        }
        return new TranslationTextComponent(" - %s", regionRemoveLink);
    }

    private static IFormattableTextComponent buildDimResetComponent(DimensionalRegion region) {
        String cmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), CommandConstants.RESET.toString(), DIM.toString());
        IFormattableTextComponent hover = new TranslationTextComponent("cli.dim.reset.dim.link.hover", region.getName());
        IFormattableTextComponent text = new TranslationTextComponent("cli.link.action.undo.text");
        return buildExecuteCmdComponent(text, hover, cmd, SUGGEST_COMMAND, REMOVE_CMD_COLOR);
    }

    private static String buildPageCommand(String cmd, int page) {
        return cmd + " " + page;
    }

    public static List<IFormattableTextComponent> buildPaginationComponents(IFormattableTextComponent description, String cmd, List<IFormattableTextComponent> entries, int pageNo, IFormattableTextComponent addEmptyLink) {
        List<IFormattableTextComponent> paginationComponents = new ArrayList<>();
        int numberOfPages = entries.size() / RegionConfig.getPaginationSize();
        if (numberOfPages == 0 || entries.size() % RegionConfig.getPaginationSize() != 0) {
            numberOfPages += 1;
        }
        if (pageNo < FIRST_PAGE_IDX || pageNo >= numberOfPages) {
            paginationComponents.add(new TranslationTextComponent("cli.msg.info.pagination.error.index", pageNo, numberOfPages - 1).withStyle(RED));
            return paginationComponents;
        }
        boolean hasMultiplePages = numberOfPages > 1;

        IFormattableTextComponent paginationControl = buildPaginationControl(
                buildFirstLinkArrow(cmd, pageNo, hasMultiplePages),
                buildPrevLinkArrow(cmd, pageNo, hasMultiplePages),
                pageNo, numberOfPages,
                buildNextLinkArrow(cmd, pageNo, numberOfPages, hasMultiplePages),
                buildLastLinkArrow(cmd, pageNo, numberOfPages, hasMultiplePages)
        );

        int from = pageNo * RegionConfig.getPaginationSize();
        int to = Math.min(RegionConfig.getPaginationSize() + (RegionConfig.getPaginationSize() * pageNo), entries.size());
        List<IFormattableTextComponent> entriesForPage = entries.subList(from, to);

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

    private static IFormattableTextComponent buildLastLinkArrow(String cmd, int pageNo, int numberOfPages, boolean hasMultiplePages) {
        return hasMultiplePages && pageNo < numberOfPages - 1
                ? buildExecuteCmdComponent(new TranslationTextComponent("cli.msg.info.pagination.last.text"), new TranslationTextComponent("cli.msg.info.pagination.last.hover"), buildPageCommand(cmd, numberOfPages - 1), RUN_COMMAND, LINK_COLOR)
                : TextComponentUtils.wrapInSquareBrackets(new TranslationTextComponent("cli.msg.info.pagination.last.text")).withStyle(INACTIVE_LINK_COLOR);
    }

    private static IFormattableTextComponent buildNextLinkArrow(String cmd, int pageNo, int numberOfPages, boolean hasMultiplePages) {
        return hasMultiplePages && pageNo < numberOfPages - 1
                ? buildExecuteCmdComponent(new TranslationTextComponent("cli.msg.info.pagination.next.text"), new TranslationTextComponent("cli.msg.info.pagination.next.hover"), buildPageCommand(cmd, Math.min(pageNo + 1, numberOfPages - 1)), RUN_COMMAND, LINK_COLOR)
                : TextComponentUtils.wrapInSquareBrackets(new TranslationTextComponent("cli.msg.info.pagination.next.text")).withStyle(INACTIVE_LINK_COLOR);
    }

    private static IFormattableTextComponent buildPrevLinkArrow(String cmd, int pageNo, boolean hasMultiplePages) {
        return hasMultiplePages && pageNo > FIRST_PAGE_IDX
                ? buildExecuteCmdComponent(new TranslationTextComponent("cli.msg.info.pagination.previous.text"), new TranslationTextComponent("cli.msg.info.pagination.previous.hover"), buildPageCommand(cmd, Math.max(pageNo - 1, FIRST_PAGE_IDX)), RUN_COMMAND, LINK_COLOR)
                : TextComponentUtils.wrapInSquareBrackets(new TranslationTextComponent("cli.msg.info.pagination.previous.text")).withStyle(INACTIVE_LINK_COLOR);
    }

    private static IFormattableTextComponent buildFirstLinkArrow(String cmd, int pageNo, boolean hasMultiplePages) {
        return hasMultiplePages && pageNo != FIRST_PAGE_IDX
                ? buildExecuteCmdComponent(new TranslationTextComponent("cli.msg.info.pagination.first.text"), new TranslationTextComponent("cli.msg.info.pagination.first.hover"), buildPageCommand(cmd, FIRST_PAGE_IDX), RUN_COMMAND, LINK_COLOR)
                : TextComponentUtils.wrapInSquareBrackets(new TranslationTextComponent("cli.msg.info.pagination.first.text")).withStyle(INACTIVE_LINK_COLOR);
    }

    private static IFormattableTextComponent buildPaginationControl(IFormattableTextComponent front, IFormattableTextComponent back, int pageNo, int maxPage, IFormattableTextComponent forward, IFormattableTextComponent last) {
        // [<<]  [<]  x/n  [>]  [>>]
        IFormattableTextComponent pageIndicator = new StringTextComponent((pageNo + 1) + "/" + (maxPage));
        return new TranslationTextComponent(" %s  %s  %s  %s  %s", front, back, pageIndicator, forward, last);
    }

    // [x]
    public static IFormattableTextComponent buildParentClearLink(IMarkableRegion region) {
        String clearRegionParentCmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), PARENT.toString(), REMOVE.toString());
        IFormattableTextComponent parentClearLinkText = new TranslationTextComponent("cli.link.remove");
        IFormattableTextComponent parentClearHoverText = new TranslationTextComponent("cli.msg.info.region.parent.clear.link.hover", region.getParent().getName());
        return buildExecuteCmdComponent(parentClearLinkText, parentClearHoverText, clearRegionParentCmd, RUN_COMMAND, REMOVE_CMD_COLOR);
    }

    // No parent set [+]
    private static IFormattableTextComponent createParentAddLink(IProtectedRegion region) {
        String setRegionParentCmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), PARENT.toString(), SET.toString(), "");
        IFormattableTextComponent setParentLinkText = new TranslationTextComponent("cli.link.add");
        IFormattableTextComponent setParentHoverText = new TranslationTextComponent("cli.msg.info.region.parent.set.link.hover", region.getName());
        return new TranslationTextComponent("%s %s",
                new TranslationTextComponent("cli.msg.info.region.parent.null"),
                buildExecuteCmdComponent(setParentLinkText, setParentHoverText, setRegionParentCmd, RUN_COMMAND, GREEN));
    }

    public static IFormattableTextComponent buildRegionListHeader(IProtectedRegion region) {
        return buildHeader(new TranslationTextComponent("cli.msg.info.header.in", buildRegionListChildrenLink(region), buildRegionInfoLink(region)));
    }

    // [n regions] [+]
    public static IFormattableTextComponent buildDimRegionsLink(DimensionRegionCache dimCache) {
        DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
        String command = buildCommandStr(DIM.toString(), dimRegion.getDim().location().toString(), LIST.toString(), CommandConstants.LOCAL.toString());
        IFormattableTextComponent text = new TranslationTextComponent("cli.msg.dim.info.region.list.link.text", dimCache.getRegions().size());
        IFormattableTextComponent hover = new TranslationTextComponent("cli.msg.dim.info.region.list.link.hover", dimRegion.getName());
        IFormattableTextComponent listLocalRegionsLink = buildExecuteCmdComponent(text, hover, command, RUN_COMMAND, LINK_COLOR);
        IFormattableTextComponent createRegionLink = buildDimCreateRegionLink(dimRegion);
        if (dimRegion.getChildren().isEmpty()) {
            return new TranslationTextComponent("%s %s", text, createRegionLink);
        }
        return new TranslationTextComponent("%s %s", listLocalRegionsLink, createRegionLink);
    }

    public static IFormattableTextComponent buildRegionListChildrenLink(IProtectedRegion region) {
        IFormattableTextComponent text = new TranslationTextComponent("cli.msg.info.region.children.list.link.text", region.getChildren().size());
        IFormattableTextComponent hover = new TranslationTextComponent("cli.msg.info.region.children.list.link.hover", region.getName());
        switch (region.getRegionType()) {
            case GLOBAL: {
                // [n dimensions(s)]
                Collection<String> dimensionList = RegionDataManager.get().getDimensionList();
                String command = buildCommandStr(GLOBAL.toString(), LIST.toString(), CHILDREN.toString());
                IFormattableTextComponent listDimRegionsLinkText = new TranslationTextComponent("cli.msg.global.info.region.list.link.text", dimensionList.size());
                IFormattableTextComponent listDimRegionsHoverText = new TranslationTextComponent("cli.msg.global.info.region.list.link.hover");
                return buildExecuteCmdComponent(listDimRegionsLinkText, listDimRegionsHoverText, command, RUN_COMMAND, LINK_COLOR);
            }
            case DIMENSION: {
                // [n children] [+]
                String command = buildCommandStr(DIM.toString(), region.getDim().location().toString(), LIST.toString(), CHILDREN.toString());
                IFormattableTextComponent listDimRegionsListLink = buildExecuteCmdComponent(text, hover, command, RUN_COMMAND, LINK_COLOR);
                if (region.getChildren().isEmpty()) {
                    return new TranslationTextComponent("%s %s", text, buildDimCreateRegionLink(region));
                }
                return new TranslationTextComponent("%s %s", listDimRegionsListLink, buildDimCreateRegionLink(region));
            }
            case LOCAL: {
                // [n children] [+]
                String regionChildrenListLink = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), CHILDREN.toString());
                IFormattableTextComponent regionChildrenLink = buildExecuteCmdComponent(text, hover, regionChildrenListLink, RUN_COMMAND, LINK_COLOR);
                IFormattableTextComponent addChildrenLink = buildRegionAddChildrenLink(region);
                if (region.getChildren().isEmpty()) {
                    return new TranslationTextComponent("%s %s", text, addChildrenLink);
                }
                return new TranslationTextComponent("%s %s", regionChildrenLink, addChildrenLink);
            }
            default:
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        }
    }

    public static IFormattableTextComponent buildRegionAddChildrenLink(IProtectedRegion region) {
        String addChildrenCmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), ADD.toString(), CHILD.toString(), "");
        IFormattableTextComponent addChildrenLinkText = new TranslationTextComponent("cli.link.add");
        IFormattableTextComponent addChildrenHoverText = new TranslationTextComponent("cli.msg.info.region.children.add.link.hover", region.getName());
        return buildExecuteCmdComponent(addChildrenLinkText, addChildrenHoverText, addChildrenCmd, SUGGEST_COMMAND, ADD_CMD_COLOR);
    }

    public static IFormattableTextComponent buildDimCreateRegionLink(IProtectedRegion region) {
        String dimCreateRegionCmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), CREATE.toString(), CommandConstants.LOCAL.toString(), "");
        IFormattableTextComponent createRegionLinkText = new TranslationTextComponent("cli.link.add");
        IFormattableTextComponent createRegionHoverText = new TranslationTextComponent("cli.msg.dim.info.region.create.link.hover", region.getName());
        return buildExecuteCmdComponent(createRegionLinkText, createRegionHoverText, dimCreateRegionCmd, SUGGEST_COMMAND, ADD_CMD_COLOR);
    }

    public static IFormattableTextComponent buildFlagListLink(IProtectedRegion region) {
        FlagContainer flagsInHierarchy = HandlerUtil.getFlagsRecursive(region, null);
        IFormattableTextComponent flagListLinkText = new TranslationTextComponent("cli.msg.info.region.flag.link.text",
                region.getFlags().size(), flagsInHierarchy.size() - region.getFlags().size());
        IFormattableTextComponent flagListHoverText = new TranslationTextComponent("cli.msg.info.region.flag.link.hover", region.getName());
        switch (region.getRegionType()) {
            case GLOBAL: {
                String flagListCmd = buildCommandStr(GLOBAL.toString(), LIST.toString(), FLAG.toString());
                IFormattableTextComponent flagListLink = buildExecuteCmdComponent(flagListLinkText, flagListHoverText, flagListCmd, RUN_COMMAND, LINK_COLOR);
                if (flagsInHierarchy.isEmpty()) {
                    flagListLink = flagListLinkText;
                }
                return new TranslationTextComponent("%s %s", flagListLink, buildAddFlagLink(region));
            }
            case DIMENSION: {
                String flagListCmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), LIST.toString(), FLAG.toString());
                IFormattableTextComponent flagListLink = buildExecuteCmdComponent(flagListLinkText, flagListHoverText, flagListCmd, RUN_COMMAND, LINK_COLOR);
                if (flagsInHierarchy.isEmpty()) {
                    flagListLink = flagListLinkText;
                }
                return new TranslationTextComponent("%s %s", flagListLink, buildAddFlagLink(region));
            }
            case LOCAL: {
                String listCmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), FLAG.toString());
                IFormattableTextComponent flagListLink = buildExecuteCmdComponent(flagListLinkText, flagListHoverText, listCmd, RUN_COMMAND, LINK_COLOR);
                if (flagsInHierarchy.isEmpty()) {
                    flagListLink = flagListLinkText;
                }
                return new TranslationTextComponent("%s %s", flagListLink, buildAddFlagLink(region));
            }
            default:
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        }
    }

    public static IFormattableTextComponent buildAddFlagLink(IProtectedRegion region) {
        IFormattableTextComponent hoverText = new TranslationTextComponent("cli.msg.info.region.flag.add.link.hover", region.getName());
        IFormattableTextComponent linkText = new TranslationTextComponent("cli.link.add");
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
                String addCmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), ADD.toString(), FLAG.toString(), "");
                return buildExecuteCmdComponent(linkText, hoverText, addCmd, SUGGEST_COMMAND, ADD_CMD_COLOR);
            }
            default:
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        }
    }

    public static IFormattableTextComponent buildRegionStateLink(IProtectedRegion region) {
        IFormattableTextComponent linkText = new TranslationTextComponent("cli.msg.info.region.state.link.text");
        IFormattableTextComponent hoverText = new TranslationTextComponent("cli.msg.info.region.state.link.hover", region.getName());
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
                String showStateCmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), STATE.toString());
                return buildExecuteCmdComponent(linkText, hoverText, showStateCmd, RUN_COMMAND, LINK_COLOR);
            }
            default:
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        }
    }

    public static IFormattableTextComponent buildDimEnableLink(IProtectedRegion region) {
        String command = ArgumentUtil.buildCommandStr(DIM.toString(), region.getDim().location().toString(), ENABLE.toString());
        String onClickAction = region.isActive() ? "deactivate" : "activate";
        String hoverText = "cli.msg.info.state." + onClickAction;
        String linkText = "cli.msg.info.state.link." + (region.isActive() ? "activate" : "deactivate");
        TextFormatting color = region.isActive() ? ADD_CMD_COLOR : REMOVE_CMD_COLOR;
        return new TranslationTextComponent("%s: %s",
                new TranslationTextComponent("cli.msg.info.state"),
                buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, color));
    }

    public static IFormattableTextComponent buildInfoComponent(String subjectLangKey, IFormattableTextComponent payload) {
        return new TranslationTextComponent("%s: %s", new TranslationTextComponent(subjectLangKey), payload);
    }

    public static IFormattableTextComponent buildRegionTeleportLink(IMarkableRegion region) {
        String teleportCmd = buildDimTeleportCmd(region.getDim(), "@s", region.getTpTarget());
        IFormattableTextComponent textInfo = new StringTextComponent(buildBlockPosLinkText(region.getTpTarget()));
        IFormattableTextComponent hoverInfo = new TranslationTextComponent("cli.msg.info.region.area.tp.link.hover", region.getName(), region.getDim().location().toString());
        return buildExecuteCmdComponent(textInfo, hoverInfo, teleportCmd, RUN_COMMAND, TP_COLOR);
    }

    public static IFormattableTextComponent buildRegionSetTpLink(IMarkableRegion region) {
        String setTpPosCmd = buildCommandStr(LOCAL.toString(), region.getDim().location().toString(), region.getName(), AREA.toString(), TELEPORT.toString(), SET.toString(), "");
        TranslationTextComponent linkText = new TranslationTextComponent("cli.msg.info.region.area.tp.set.link.text");
        TranslationTextComponent hoverText = new TranslationTextComponent("cli.msg.info.region.area.tp.set.link.hover", region.getName());
        return buildExecuteCmdComponent(linkText, hoverText, setTpPosCmd, SUGGEST_COMMAND, LINK_COLOR);
    }

    public static IFormattableTextComponent buildDimSuggestRegionRemovalLink(IMarkableRegion region) {
        String cmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), DELETE.toString(), region.getName());
        IFormattableTextComponent hover = new TranslationTextComponent("cli.msg.info.dim.region.remove.link.hover", region.getName());
        IFormattableTextComponent text = new TranslationTextComponent("cli.link.remove");
        return buildExecuteCmdComponent(text, hover, cmd, SUGGEST_COMMAND, REMOVE_CMD_COLOR);
    }

    /**
     * @param region    the region to build the link for
     * @param names     the names of the players or teams of the group
     * @param groupType the type of the group (player or team)
     * @param group     the name of the group
     * @return a list of links to remove the group from the region
     */
    public static List<IFormattableTextComponent> buildRemoveGroupMemberEntries(IProtectedRegion region, List<String> names, GroupType groupType, String group) {
        return names.stream().sorted().map(name -> buildRemoveGroupEntry(region, name, groupType, group)).collect(Collectors.toList());
    }

    public static IFormattableTextComponent buildRemoveGroupEntry(IProtectedRegion region, String name, GroupType groupType, String group) {
        IFormattableTextComponent linkText = new TranslationTextComponent("cli.link.remove");
        IFormattableTextComponent hoverText = new TranslationTextComponent("cli.msg.info.region.group." + groupType.name + ".remove.link.hover", name, region.getName());
        IFormattableTextComponent regionRemoveLink;
        if (groupType == GroupType.PLAYER) {
            PlayerEntity player = ServerLifecycleHooks.getCurrentServer().getPlayerList().getPlayerByName(name);
            boolean isOffline = player == null;
            if (isOffline) {
                IFormattableTextComponent offlinePlayerRemoveLink = buildRemoveLinkForOfflinePlayer(region, name, groupType, group, linkText, hoverText);
                return new TranslationTextComponent(" - %s %s", offlinePlayerRemoveLink, buildGroupInfo(region, name, groupType));
            }
        }
        switch (region.getRegionType()) {
            case GLOBAL: {
                String command = buildCommandStr(GLOBAL.toString(), REMOVE.toString(), groupType.name, group, name);
                regionRemoveLink = buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, REMOVE_CMD_COLOR);
                break;
            }
            case DIMENSION: {
                String command = buildCommandStr(DIM.toString(), region.getDim().location().toString(), REMOVE.toString(), groupType.name, group, name);
                regionRemoveLink = buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, REMOVE_CMD_COLOR);
                break;
            }
            case LOCAL: {
                String command = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), REMOVE.toString(), groupType.name, group, name);
                regionRemoveLink = buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, REMOVE_CMD_COLOR);
                break;
            }
            default:
                throw new IllegalArgumentException();
        }
        return new TranslationTextComponent(" - %s %s", regionRemoveLink, buildGroupInfo(region, name, groupType));
    }

    private static IFormattableTextComponent buildRemoveLinkForOfflinePlayer(IProtectedRegion region, String name, GroupType groupType, String group, IFormattableTextComponent linkText, IFormattableTextComponent hoverText) {
        IFormattableTextComponent regionRemoveLink;
        switch (region.getRegionType()) {
            case GLOBAL: {
                String command = buildCommandStr(GLOBAL.toString(), REMOVE.toString(), groupType.name, group, BY_NAME.toString(), name);
                return buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, REMOVE_CMD_COLOR);
            }
            case DIMENSION: {
                String command = buildCommandStr(DIM.toString(), region.getDim().location().toString(), REMOVE.toString(), groupType.name, group, BY_NAME.toString(), name);
                return buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, REMOVE_CMD_COLOR);
            }
            case LOCAL: {
                String command = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), REMOVE.toString(), groupType.name, group, BY_NAME.toString(), name);
                return buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, REMOVE_CMD_COLOR);
            }
            default:
                throw new IllegalArgumentException();
        }
    }

    public static IFormattableTextComponent buildGroupInfo(IProtectedRegion region, String groupMemberName, GroupType groupType) {
        IFormattableTextComponent res;
        switch (groupType) {
            case PLAYER: {
                PlayerEntity player = ServerLifecycleHooks.getCurrentServer().getPlayerList().getPlayerByName(groupMemberName);
                if (player == null) {
                    res = new TranslationTextComponent("%s %s",
                            new StringTextComponent(groupMemberName).withStyle(GRAY),
                            new TranslationTextComponent("cli.msg.info.player.list.entry.offline"));
                } else {
                    res = buildPlayerHoverComponent(player);
                }
                break;
            }
            case TEAM: {
                ServerWorld level = ServerLifecycleHooks.getCurrentServer().getLevel(region.getDim());
                if (level != null) {
                    Team team = level.getScoreboard().getPlayerTeam(groupMemberName);
                    res = team == null ? new StringTextComponent(groupMemberName) : buildTeamHoverComponent(team);
                } else {
                    res = new StringTextComponent(groupMemberName);
                }
                break;
            }
            default:
                throw new IllegalStateException("Unexpected value: " + groupType);
        }
        return res;
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
    public static IFormattableTextComponent buildRegionRemoveChildLink(IProtectedRegion region, IProtectedRegion child) {
        String command = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), REMOVE.toString(), CHILD.toString(), child.getName());
        IFormattableTextComponent linkText = new TranslationTextComponent("cli.link.remove");
        IFormattableTextComponent linkHoverText = new TranslationTextComponent("cli.msg.info.region.children.remove.link.hover", child.getName(), region.getName());
        return buildExecuteCmdComponent(linkText, linkHoverText, command, RUN_COMMAND, REMOVE_CMD_COLOR);
    }

    public static IFormattableTextComponent buildRegionActionUndoLink(String cmd, CommandConstants toReplace, CommandConstants replacement) {
        String revertCmd = ArgumentUtil.revertCommand(cmd, toReplace, replacement);
        IFormattableTextComponent revertLinkText = new TranslationTextComponent("cli.link.action.undo.text");
        IFormattableTextComponent revertLinkHover = new TranslationTextComponent("cli.link.action.undo.hover");
        return buildExecuteCmdComponent(revertLinkText, revertLinkHover, revertCmd, RUN_COMMAND, DARK_RED);
    }

    public static IFormattableTextComponent buildRegionActionUndoLink(String cmd, String toReplace, String replacement) {
        String revertCmd = ArgumentUtil.revertCommand(cmd, toReplace, replacement);
        IFormattableTextComponent revertLinkText = new TranslationTextComponent("cli.link.action.undo.text");
        IFormattableTextComponent revertLinkHover = new TranslationTextComponent("cli.link.action.undo.hover");
        return buildExecuteCmdComponent(revertLinkText, revertLinkHover, revertCmd, RUN_COMMAND, DARK_RED);
    }

    // The content of the branches is the same for now but we keep it in place, in case we want to change it later
    public static IFormattableTextComponent buildRegionFlagInfoHeader(IProtectedRegion region) {
        IFormattableTextComponent res;
        switch (region.getRegionType()) {
            case GLOBAL:
                res = buildHeader(new TranslationTextComponent("cli.msg.info.header.in", buildFlagListLink(region), buildRegionInfoLink(region)));
                break;
            case DIMENSION:
                res = buildHeader(new TranslationTextComponent("cli.msg.info.header.in", buildFlagListLink(region), buildRegionInfoLink(region)));
                break;
            case LOCAL:
                res = buildHeader(new TranslationTextComponent("cli.msg.info.header.in", buildFlagListLink(region), buildRegionInfoLink(region)));
                break;
            default:
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        }
        return res;
    }

    // The content of the branches is the same for now but we keep it in place, in case we want to change it later
    public static IFormattableTextComponent buildFlagInfoHeader(IProtectedRegion region, IFlag flag) {
        IFormattableTextComponent res;
        switch (region.getRegionType()) {
            case DIMENSION:
                res = buildHeader(new TranslationTextComponent("cli.msg.info.header.flag.in", buildFlagInfoLink(region, flag), buildRegionInfoLink(region)));
                break;
            case LOCAL:
                res = buildHeader(new TranslationTextComponent("cli.msg.info.header.flag.in", buildFlagInfoLink(region, flag), buildRegionInfoLink(region)));
                break;
            case GLOBAL:
                res = buildHeader(new TranslationTextComponent("cli.msg.info.header.flag.in", buildFlagInfoLink(region, flag), buildRegionInfoLink(region)));
                break;
            default:
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        }
        return res;
    }
}
