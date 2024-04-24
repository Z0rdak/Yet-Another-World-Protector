package de.z0rdak.yawp.util;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import com.mojang.math.Vector3d;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.commands.CommandConstants;
import de.z0rdak.yawp.commands.RegionCommands;
import de.z0rdak.yawp.commands.RegionCommands;
import de.z0rdak.yawp.commands.arguments.ArgumentUtil;
import de.z0rdak.yawp.config.server.CommandPermissionConfig;
import de.z0rdak.yawp.config.server.FlagConfig;
import de.z0rdak.yawp.config.server.RegionConfig;
import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.area.IMarkableArea;
import de.z0rdak.yawp.core.area.SphereArea;
import de.z0rdak.yawp.core.flag.BooleanFlag;
import de.z0rdak.yawp.core.flag.FlagContainer;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.group.GroupType;
import de.z0rdak.yawp.core.group.PlayerContainer;
import de.z0rdak.yawp.core.region.*;
import de.z0rdak.yawp.handler.flags.HandlerUtil;
import de.z0rdak.yawp.handler.flags.FlagCorrelation;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.ChatFormatting;
import net.minecraft.commands.CommandSourceStack;
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
import static net.minecraft.ChatFormatting.RESET;
import static net.minecraft.ChatFormatting.*;
import static net.minecraft.network.chat.ClickEvent.Action.*;


public class MessageUtil {

    private MessageUtil() {
    }

    public final static ChatFormatting SUGGEST_COLOR = BLUE;
    public final static ChatFormatting TP_COLOR = GREEN;
    public final static ChatFormatting LINK_COLOR = AQUA;
    public final static ChatFormatting INACTIVE_LINK_COLOR = GRAY;
    public final static ChatFormatting ADD_CMD_COLOR = DARK_GREEN;
    public final static ChatFormatting REMOVE_CMD_COLOR = DARK_RED;
    public static int FIRST_PAGE_IDX = 0;

    public static MutableComponent buildHeader(String translationKey) {
        return buildHeader(new TranslatableComponent(translationKey));
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
        sendCmdFeedback(src, new TranslatableComponent(langKey));
    }

    public static void sendMessage(Player player, String translationKey) {
        player.sendMessage(new TranslatableComponent(translationKey), player.getUUID());
    }

    public static void sendNotification(Player player, MutableComponent msg) {
        player.displayClientMessage(msg, true);
    }

    public static String buildTeleportCmd(String tpSource, BlockPos target) {
        return "tp " + tpSource + " " + buildBlockCoordinateStr(target);
    }

    public static String buildBlockCoordinateStr(BlockPos target) {
        return target.getX() + " " + target.getY() + " " + target.getZ();
    }

    public static String buildTeleportLinkText(ResourceKey<Level> dim, BlockPos target) {
        return buildTeleportLinkText(dim.location().toString(), target);
    }

    public static String buildTeleportLinkText(String regionName, BlockPos target) {
        return regionName + " @ [" + buildBlockPosLinkText(target) + "]";
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

    public static String buildExecuteCommandString(ResourceKey<Level> dim, String command) {
        return "/execute in " + dim.location() + " run " + command;
    }

    public static String buildDimTeleportCmd(ResourceKey<Level> dim, String tpSource, BlockPos target) {
        return buildExecuteCommandString(dim, buildTeleportCmd(tpSource, target));
    }

    public static String buildRegionTpCmd(IMarkableRegion region, String target) {
        return buildDimTeleportCmd(region.getDim(), target, region.getTpTarget());
    }

    public static MutableComponent buildHeader(MutableComponent header) {
        return new TranslatableComponent("%s %s %s", ChatFormatting.BOLD, header, ChatFormatting.BOLD);
    }

    public static void sendMessage(Player player, MutableComponent textComponent) {
        player.sendMessage(textComponent, player.getUUID());
    }

    public static MutableComponent buildExecuteCmdComponent(String linkText, String hoverText, String command, ClickEvent.Action eventAction, ChatFormatting color) {
        MutableComponent text = ComponentUtils.wrapInSquareBrackets(new TranslatableComponent(linkText));
        return text.setStyle(text.getStyle()
                .withColor(color)
                .withClickEvent(new ClickEvent(eventAction, command))
                .withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, new TranslatableComponent(hoverText))));
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
        MutableComponent playerName = new TextComponent(player.getScoreboardName());
        playerName.setStyle(playerName.getStyle()
                .withColor(LINK_COLOR)
                .withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_ENTITY, entityTooltipInfo))
                .withClickEvent(new ClickEvent(SUGGEST_COMMAND, "/tell " + playerName.getString() + " ")));
        return playerName;
    }

    public static MutableComponent buildTeamHoverComponent(Team team) {
        MutableComponent playerName = new TextComponent(team.getName());
        playerName.setStyle(playerName.getStyle()
                .withColor(LINK_COLOR)
                .withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, new TranslatableComponent("cli.msg.info.region.group.link.hover")))
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
        MutableComponent blockPosTpLinkList = new TextComponent("");
        tpLinks.forEach(tpLink -> blockPosTpLinkList.append(tpLink).append(" "));
        return blockPosTpLinkList;
    }

    /**
     * [X,Y,Z]
     */
    public static MutableComponent buildDimensionalBlockTpLink(ResourceKey<Level> dim, BlockPos target) {
        String teleportCmd = buildDimTeleportCmd(dim, "@s", target);
        TranslatableComponent text = new TranslatableComponent("cli.msg.info.region.area.tp.block.link.text", buildBlockPosLinkText(target));
        TranslatableComponent hover = new TranslatableComponent("cli.msg.info.region.area.tp.block.link.hover");
        return buildExecuteCmdComponent(text, hover, teleportCmd, RUN_COMMAND, TP_COLOR);
    }

    public static MutableComponent buildTeleportLink(IMarkableRegion region) {
        String executeCmdStr = buildDimTeleportCmd(region.getDim(), "@s", region.getTpTarget());
        TranslatableComponent text = new TranslatableComponent("cli.msg.info.region.area.tp.link.text", buildBlockPosLinkText(region.getTpTarget()));
        MutableComponent hover = new TranslatableComponent("cli.msg.info.region.area.tp.link.hover", region.getName(), region.getDim().location().toString());
        return buildExecuteCmdComponent(text, hover, executeCmdStr, RUN_COMMAND, TP_COLOR);
    }

    /**
     * [region] @ [X,Y,Z]
     */
    public static MutableComponent buildDimensionTeleportLink(IMarkableRegion region) {
        String executeCmdStr = buildDimTeleportCmd(region.getDim(), "@s", region.getTpTarget());
        MutableComponent text = new TranslatableComponent("cli.msg.info.region.area.tp.link.text", buildBlockPosLinkText(region.getTpTarget()));
        MutableComponent hover = new TranslatableComponent("cli.msg.info.region.area.tp.link.hover", region.getName(), region.getDim().location().toString());
        return new TranslatableComponent("%s @ %s",
                buildRegionInfoLink(region),
                buildExecuteCmdComponent(text, hover, executeCmdStr, RUN_COMMAND, TP_COLOR));
    }

    public static MutableComponent buildRegionAreaDetailComponent(IMarkableRegion region) {
        IMarkableArea area = region.getArea();
        MutableComponent areaInfo = new TextComponent(area.getAreaType().areaType);
        switch (area.getAreaType()) {
            case CUBOID:
                return new TranslatableComponent("%s, %s", areaInfo, buildCuboidAreaInfo((CuboidArea) area));
            case CYLINDER:
                throw new NotImplementedException("cylinder");
            case SPHERE:
                return new TranslatableComponent("%s, %s", areaInfo, buildSphereAreaInfo((SphereArea) area));
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
    private static MutableComponent buildCuboidAreaInfo(CuboidArea cuboidArea) {
        return new TranslatableComponent("cli.msg.info.region.area.area.size.text.cuboid",
                buildAreaAxisInfoComponent(cuboidArea, Direction.Axis.X),
                buildAreaAxisInfoComponent(cuboidArea, Direction.Axis.Y),
                buildAreaAxisInfoComponent(cuboidArea, Direction.Axis.Z));

    }

    /**
     * Center: [X,Y,Z], Radius: 5, Diameter: 11
     */
    private static MutableComponent buildSphereAreaInfo(SphereArea sphereArea) {
        int diameter = (sphereArea.getRadius() * 2) + 1;
        MutableComponent centerPos = new TextComponent(buildBlockPosLinkText(sphereArea.getCenterPos()));
        return new TranslatableComponent("cli.msg.info.region.area.area.size.text.sphere",
                buildTextWithHoverMsg(centerPos, centerPos, WHITE), sphereArea.getRadius(), diameter);
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
        return buildTextWithHoverMsg(
                new TextComponent(axisName + "=" + axisSize),
                new TextComponent(axisName + ": " + min + " - " + max), WHITE);
    }

    /**
     * [<=expand=>] [<=max=>]
     */
    public static MutableComponent buildRegionAreaExpandLink(IMarkableRegion region) {
        MutableComponent linkText = new TranslatableComponent("cli.msg.info.region.area.area.expand.link.text");
        MutableComponent linkHover = new TranslatableComponent("cli.msg.info.region.area.area.expand.link.hover", region.getName());
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
                MutableComponent maxExpandLinkText = new TranslatableComponent("cli.msg.info.region.area.area.expand-max.link.text");
                MutableComponent maxExpandLinkHover = new TranslatableComponent("cli.msg.info.region.area.area.expand-max.link.hover");
                String maxExpandCmd = appendSubCommand(expandCmd, String.valueOf(RegionCommands.MIN_BUILD_LIMIT), String.valueOf(RegionCommands.MAX_BUILD_LIMIT));
                MutableComponent maxExpandLink = buildExecuteCmdComponent(maxExpandLinkText, maxExpandLinkHover, maxExpandCmd, RUN_COMMAND, LINK_COLOR);
                return new TranslatableComponent("%s %s", expandLink, maxExpandLink);
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
        MutableComponent showAreaLinkText = new TranslatableComponent("cli.msg.info.region.area.area.show.link");
        MutableComponent showAreaLinkHover = new TranslatableComponent("cli.msg.info.region.area.area.show.hover", region.getName());
        String showAreaCmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), AREA.toString(), "show");
        return buildExecuteCmdComponent(showAreaLinkText, showAreaLinkHover, showAreaCmd, RUN_COMMAND, LINK_COLOR);
    }

    /**
     * [set area]
     */
    public static MutableComponent buildAreaUpdateLink(IMarkableRegion region) {
        TranslatableComponent setAreaLinkText = new TranslatableComponent("cli.msg.info.region.area.area.set.link");
        TranslatableComponent setAreaLinkHover = new TranslatableComponent("cli.msg.info.region.area.area.set.hover", region.getName());
        String blocks = String.join(" ", region.getArea().getMarkedBlocks().stream()
                .map(MessageUtil::buildBlockCoordinateStr)
                .collect(Collectors.toSet()));
        String setAreaCmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), AREA.toString(), SET.toString(), region.getArea().getAreaType().areaType, blocks);
        return buildExecuteCmdComponent(setAreaLinkText, setAreaLinkHover, setAreaCmd, SUGGEST_COMMAND, LINK_COLOR);
    }

    /**
     * [set area] [set TP] [show area] [<=expand=>] [<=max=>]
     */
    public static MutableComponent buildRegionAreaActionLinks(IMarkableRegion region) {
        return new TranslatableComponent("%s %s %s", buildAreaUpdateLink(region), buildRegionSetTpLink(region), buildRegionAreaExpandLink(region));
        // buildShowAreaToggleLink(region)
    }

    public static MutableComponent buildTextWithHoverMsg(MutableComponent text, MutableComponent hoverText, ChatFormatting color) {
        MutableComponent bracketedText = ComponentUtils.wrapInSquareBrackets(text);
        bracketedText.setStyle(bracketedText.getStyle().withColor(color).withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, hoverText)));
        return bracketedText;
    }

    public static MutableComponent buildHelpStartComponent() {
        String command = ArgumentUtil.buildCommandStr(CommandConstants.GLOBAL.toString(), CommandConstants.INFO.toString());
        MutableComponent text = new TranslatableComponent("help.hint.link.text");
        MutableComponent hover = new TranslatableComponent("help.hint.link.hover", CommandPermissionConfig.BASE_CMD);
        return buildExecuteCmdComponent(text, hover, command, ClickEvent.Action.RUN_COMMAND, LINK_COLOR);
    }

    public static MutableComponent buildWikiLink() {
        MutableComponent wikiLinkHover = new TranslatableComponent("help.tooltip.wiki.link.hover");
        MutableComponent wikiLink = new TranslatableComponent("help.tooltip.wiki.link.text");
        return buildExecuteCmdComponent(wikiLink, wikiLinkHover, "https://github.com/Z0rdak/Yet-Another-World-Protector/wiki", OPEN_URL, AQUA);
    }

    public static MutableComponent buildHelpSuggestionLink(String translationKey, CommandConstants baseCmd, CommandConstants cmd) {
        String command = "/" + CommandPermissionConfig.BASE_CMD + " " + baseCmd + " " + cmd + " ";
        return new TranslatableComponent(" %s %s",
                buildExecuteCmdComponent("=>", "chat.link.hover.command.copy", command, SUGGEST_COMMAND, LINK_COLOR),
                new TranslatableComponent(translationKey));
    }

    public static MutableComponent buildAllLocalEnableComponent(DimensionRegionCache dimCache) {
        MutableComponent enableLinkTextKey = new TranslatableComponent("cli.msg.info.region.state.alert.all.true.link.text");
        MutableComponent enableHoverTextKey = new TranslatableComponent("cli.msg.info.region.state.alert.all.true.link.hover", dimCache.dimensionKey().location().toString());
        MutableComponent disableLinkTextKey = new TranslatableComponent("cli.msg.info.region.state.alert.all.false.link.text");
        MutableComponent disableHoverTextKey = new TranslatableComponent("cli.msg.info.region.state.alert.all.false.link.hover", dimCache.dimensionKey().location().toString());
        String enableCmd = ArgumentUtil.buildCommandStr(DIM.toString(), dimCache.dimensionKey().location().toString(), STATE.toString(), ENABLE_LOCAL.toString(), Boolean.TRUE.toString());
        String disableCmd = ArgumentUtil.buildCommandStr(DIM.toString(), dimCache.dimensionKey().location().toString(), STATE.toString(), ENABLE_LOCAL.toString(), Boolean.FALSE.toString());
        MutableComponent activeAlertLink = buildExecuteCmdComponent(enableLinkTextKey, enableHoverTextKey, enableCmd, RUN_COMMAND, GREEN);
        MutableComponent disableAlertLink = buildExecuteCmdComponent(disableLinkTextKey, disableHoverTextKey, disableCmd, RUN_COMMAND, RED);
        return new TranslatableComponent("%s %s", activeAlertLink, disableAlertLink);
    }

    public static MutableComponent buildAllLocalAlertToggleLink(DimensionRegionCache dimCache) {
        MutableComponent enableLinkTextKey = new TranslatableComponent("cli.msg.info.region.state.enable.all.true.link.text");
        MutableComponent enableHoverTextKey = new TranslatableComponent("cli.msg.info.region.state.enable.all.true.link.hover", dimCache.dimensionKey().location().toString());
        MutableComponent disableLinkTextKey = new TranslatableComponent("cli.msg.info.region.state.enable.all.false.link.text");
        MutableComponent disableHoverTextKey = new TranslatableComponent("cli.msg.info.region.state.enable.all.false.link.hover", dimCache.dimensionKey().location().toString());
        String enableCmd = ArgumentUtil.buildCommandStr(DIM.toString(), dimCache.dimensionKey().location().toString(), STATE.toString(), ALERT_LOCAL.toString(), Boolean.TRUE.toString());
        String disableCmd = ArgumentUtil.buildCommandStr(DIM.toString(), dimCache.dimensionKey().location().toString(), STATE.toString(), ALERT_LOCAL.toString(), Boolean.FALSE.toString());
        MutableComponent activeAlertLink = buildExecuteCmdComponent(enableLinkTextKey, enableHoverTextKey, enableCmd, RUN_COMMAND, GREEN);
        MutableComponent disableAlertLink = buildExecuteCmdComponent(disableLinkTextKey, disableHoverTextKey, disableCmd, RUN_COMMAND, RED);
        return new TranslatableComponent("%s %s", activeAlertLink, disableAlertLink);
    }

    public static MutableComponent buildRegionEnableComponent(IProtectedRegion region) {
        String linkTextKey = "cli.msg.info.region.state.enable." + region.isActive() + ".link.text";
        String hoverTextKey = "cli.msg.info.region.state.enable." + !region.isActive() + ".link.hover";
        ChatFormatting color = region.isActive() ? GREEN : GRAY;
        return switch (region.getRegionType()) {
            case GLOBAL -> {
                String cmd = ArgumentUtil.buildCommandStr(GLOBAL.toString(), STATE.toString(), ENABLE.toString());
                yield buildExecuteCmdComponent(linkTextKey, hoverTextKey, cmd, ClickEvent.Action.RUN_COMMAND, color);
            }
            case DIMENSION -> {
                String cmd = ArgumentUtil.buildCommandStr(DIM.toString(), region.getDim().location().toString(), STATE.toString(), ENABLE.toString());
                yield buildExecuteCmdComponent(linkTextKey, hoverTextKey, cmd, RUN_COMMAND, color);
            }
            case LOCAL -> {
                String cmd = ArgumentUtil.buildCommandStr(LOCAL.toString(), region.getDim().location().toString(), region.getName(), STATE.toString(), ENABLE.toString());
                yield buildExecuteCmdComponent(linkTextKey, hoverTextKey, cmd, RUN_COMMAND, color);
            }
            default ->
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        };
    }

    public static MutableComponent buildRegionPriorityComponent(IMarkableRegion region) {
        int defaultPriorityInc = RegionConfig.getDefaultPriorityInc();
        String incPriorityCmd = ArgumentUtil.buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), STATE.toString(), PRIORITY.toString(), INC.toString(), String.valueOf(defaultPriorityInc));
        MutableComponent incLinkText = new TranslatableComponent("cli.msg.info.region.state.priority.increase.link.text", defaultPriorityInc);
        MutableComponent incHoverText = new TranslatableComponent("cli.msg.info.region.state.priority.increase.link.hover", defaultPriorityInc);
        MutableComponent increaseLink = buildExecuteCmdComponent(incLinkText, incHoverText, incPriorityCmd, RUN_COMMAND, ADD_CMD_COLOR);
        String decPriorityCmd = ArgumentUtil.buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), STATE.toString(), PRIORITY.toString(), DEC.toString(), String.valueOf(defaultPriorityInc));
        MutableComponent decLinkText = new TranslatableComponent("cli.msg.info.region.state.priority.decrease.link.text", defaultPriorityInc);
        MutableComponent decHoverText = new TranslatableComponent("cli.msg.info.region.state.priority.decrease.link.hover", defaultPriorityInc);
        MutableComponent decreaseLink = buildExecuteCmdComponent(decLinkText, decHoverText, decPriorityCmd, RUN_COMMAND, REMOVE_CMD_COLOR);
        String setPriorityCmd = ArgumentUtil.buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), STATE.toString(), PRIORITY.toString(), "");
        MutableComponent setPriorityLinkText = new TranslatableComponent("cli.msg.info.region.state.priority.set.link.text", region.getPriority());
        MutableComponent setPriorityHoverText = new TranslatableComponent("cli.msg.info.region.state.priority.set.link.hover");
        MutableComponent setPriorityLink = buildExecuteCmdComponent(setPriorityLinkText, setPriorityHoverText, setPriorityCmd, SUGGEST_COMMAND, LINK_COLOR);
        return new TranslatableComponent("%s %s %s", setPriorityLink, increaseLink, decreaseLink);
    }

    public static MutableComponent buildRegionAlertToggleLink(IProtectedRegion region) {
        String linkTextKey = "cli.msg.info.region.state.alert." + !region.isMuted() + ".link.text";
        String hoverTextKey = "cli.msg.info.region.state.alert." + region.isMuted() + ".link.hover";
        ChatFormatting color = region.isMuted() ? GRAY : GREEN;
        return switch (region.getRegionType()) {
            case GLOBAL -> {
                String cmd = buildCommandStr(GLOBAL.toString(), STATE.toString(), ALERT.toString());
                yield buildExecuteCmdComponent(linkTextKey, hoverTextKey, cmd, ClickEvent.Action.RUN_COMMAND, color);
            }
            case DIMENSION -> {
                String cmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), STATE.toString(), ALERT.toString());
                yield buildExecuteCmdComponent(linkTextKey, hoverTextKey, cmd, RUN_COMMAND, color);
            }
            case LOCAL -> {
                String cmd = buildCommandStr(LOCAL.toString(), region.getDim().location().toString(), region.getName(), STATE.toString(), ALERT.toString());
                yield buildExecuteCmdComponent(linkTextKey, hoverTextKey, cmd, RUN_COMMAND, color);
            }
            default ->
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        };
    }

    public static MutableComponent buildRegionRenameLink(IProtectedRegion region) {
        String cmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), RENAME.toString(), "");
        MutableComponent text = new TranslatableComponent("cli.msg.info.region.state.rename.link.text");
        MutableComponent hover = new TranslatableComponent("cli.msg.info.region.state.rename.link.hover", region.getName());
        return buildExecuteCmdComponent(text, hover, cmd, SUGGEST_COMMAND, LINK_COLOR);
    }

    public static MutableComponent buildRegionInfoLink(IProtectedRegion region) {
        return buildRegionInfoLink(region, new TranslatableComponent("cli.msg.info.region.link.hover", region.getName()));
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
            default ->
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        };
    }

    public static MutableComponent buildRegionInfoLink(IProtectedRegion region, MutableComponent hoverText) {
        MutableComponent linkText = new TextComponent(region.getName());
        return buildRegionInfoLink(region, linkText, hoverText);
    }

    public static MutableComponent buildRegionAreaLink(IMarkableRegion region) {
        String showSpatialPropLink = buildCommandStr(LOCAL.toString(), region.getDim().location().toString(), region.getName(), AREA.toString());
        MutableComponent spatialPropLinkText = new TranslatableComponent("cli.msg.info.region.area.link.text");
        MutableComponent spatialPropHoverText = new TranslatableComponent("cli.msg.info.region.area.link.hover", region.getName());
        return buildExecuteCmdComponent(spatialPropLinkText, spatialPropHoverText, showSpatialPropLink, RUN_COMMAND, LINK_COLOR);
    }

    public static MutableComponent buildRegionOverviewHeader(IProtectedRegion region) {
        switch (region.getRegionType()) {
            case GLOBAL: {
                MutableComponent clipBoardDumpLink = buildExecuteCmdComponent("cli.msg.global.overview.header.dump.link.text", "cli.msg.global.overview.header.dump.link.hover", NbtUtils.prettyPrint(region.serializeNBT()), ClickEvent.Action.COPY_TO_CLIPBOARD, GOLD);
                return buildHeader(new TranslatableComponent("cli.msg.info.header.for", clipBoardDumpLink, buildRegionInfoLink(region)));
            }
            case DIMENSION: {
                MutableComponent clipBoardDumpLink = buildExecuteCmdComponent("cli.msg.dim.overview.header.dump.link.text", "cli.msg.dim.overview.header.dump.link.hover", NbtUtils.prettyPrint(region.serializeNBT()), ClickEvent.Action.COPY_TO_CLIPBOARD, GOLD);
                return buildHeader(new TranslatableComponent("cli.msg.info.header.for", clipBoardDumpLink, buildRegionInfoLink(region)));
            }
            case LOCAL: {
                MutableComponent clipBoardDumpLink = buildExecuteCmdComponent("cli.msg.local.overview.header.dump.link.text", "cli.msg.local.overview.header.dump.link.hover", NbtUtils.prettyPrint(region.serializeNBT()), ClickEvent.Action.COPY_TO_CLIPBOARD, GOLD);
                return buildHeader(new TranslatableComponent("cli.msg.info.header.for", clipBoardDumpLink, buildRegionInfoLink(region)));
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
        MutableComponent hoverText = new TranslatableComponent("cli.msg.info.region.group.player.list.link.hover", group, region.getName());
        MutableComponent linkText = new TranslatableComponent("cli.msg.info.region.group.player.list.link.text", players.getPlayers().size());
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
            default ->
                throw new IllegalArgumentException();
        };
    }

    /**
     * Teams: [n team(s)] [+]
     * // TODO:
     */
    public static MutableComponent buildTeamListLink(IProtectedRegion region, PlayerContainer teams, String group) {
        MutableComponent hoverText = new TranslatableComponent("cli.msg.info.region.group.team.list.link.hover", group, region.getName());
        MutableComponent linkText = new TranslatableComponent("cli.msg.info.region.group.team.list.link.text", teams.getTeams().size());
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
        MutableComponent linkText = new TranslatableComponent("cli.link.add");
        MutableComponent hoverText = new TranslatableComponent("cli.msg.info.region.group." + groupType.name + ".add.link.hover", group, region.getName());
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
                .reduce(new TextComponent(""), (link1, link2) -> link1.append(" ").append(link2));
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
        return buildHeader(new TranslatableComponent("cli.msg.info.header.in", groupLink, buildRegionInfoLink(region)));
    }

    public static MutableComponent buildGroupHeader(IProtectedRegion region, String group, GroupType groupType) {
        return new TranslatableComponent("cli.msg.info.region.group." + groupType.name + ".list", buildRegionInfoLink(region), group);
    }

    public static MutableComponent buildGroupLink(IProtectedRegion region, String group, int groupSie) {
        MutableComponent linkText = new TranslatableComponent("cli.msg.info.region.group.list.link.text", groupSie, group);
        MutableComponent hoverText = new TranslatableComponent("cli.msg.info.region.group.list.link.hover", group, region.getName());
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
                : new TranslatableComponent("cli.msg.info.region.group.team.list.link.text", playerContainer.getTeams().size());
        return new TranslatableComponent("%s: %s %s",
                new TranslatableComponent("cli.msg.info.region.group.team"),
                teamListLink,
                teamAddLink);
    }

    public static MutableComponent buildGroupPlayerListLink(IProtectedRegion region, String group) {
        // Players: [n player(s)] [+]
        PlayerContainer playerContainer = region.getGroup(group);
        MutableComponent playersAddLink = buildAddToGroupLink(region, group, GroupType.PLAYER);
        MutableComponent playerListLink = playerContainer.hasPlayers()
                ? buildPlayerListLink(region, playerContainer, group)
                : new TranslatableComponent("cli.msg.info.region.group.player.list.link.text", playerContainer.getPlayers().size());
        return new TranslatableComponent("%s: %s %s",
                new TranslatableComponent("cli.msg.info.region.group.player"),
                playerListLink,
                playersAddLink);
    }

    /**
     * Creates a TextComponent for flag removal, followed by the flag infos
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
        MutableComponent hoverText = new TranslatableComponent("cli.msg.info.region.flag.remove.link.hover", flag.getName(), region.getName());
        MutableComponent linkText = new TranslatableComponent("cli.link.remove");
        MutableComponent flagRemoveLink = buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, REMOVE_CMD_COLOR);
        MutableComponent flagQuickActionComponent = buildFlagQuickActionComponent(region, flag, flagLinkColor);
        flagQuickActionComponent.withStyle(formattings);
        return new TranslatableComponent(" - %s %s", flagRemoveLink, flagQuickActionComponent);
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
    private static MutableComponent buildFlagQuickActionComponent(IProtectedRegion region, IFlag flag, ChatFormatting flagLinkColor) {
        MutableComponent regionTypeIndicator = new TextComponent(region.getRegionType().type.substring(0, 1).toUpperCase());
        MutableComponent hoverText = new TranslatableComponent("cli.flag.info.hover", flag.getName(), region.getName());
        MutableComponent flagInfoLink = buildFlagInfoLink(region, flag, flagLinkColor);
        return new TranslatableComponent("%s %s %s %s %s",
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
        MutableComponent text = new TextComponent(flag.getName());
        MutableComponent hoverText = new TranslatableComponent("cli.flag.info.hover", flag.getName(), region.getName());
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


    /**
     * Creates a TextComponent for toggling the flag state between allow and deny <br></br>
     */
    public static MutableComponent buildFlagStateToggleLink(IProtectedRegion region, IFlag flag) {
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

    public static MutableComponent buildFlagStateComponent(IProtectedRegion region, IFlag flag) {
        FlagState state = flag.getState();
        MutableComponent text = new TextComponent(state.name);
        MutableComponent hover = TextComponent.EMPTY.plainCopy();
        ChatFormatting color = WHITE;
        switch (state) {
            case ALLOWED:
                color = GREEN;
                hover = new TranslatableComponent("cli.flag.state.allowed.info.hover");
                break;
            case DENIED:
                color = RED;
                hover = new TranslatableComponent("cli.flag.state.denied.info.hover");
                break;
            case DISABLED:
                color = GRAY;
                hover = new TranslatableComponent("cli.flag.state.disabled.info.hover");
                break;
        }
        MutableComponent stateInfo = buildTextWithHoverMsg(text, hover, color);
        return new TranslatableComponent("%s %s", stateInfo, buildFlagStateSuggestionLink(region, flag));
    }

    public static MutableComponent buildFlagStateSuggestionLink(IProtectedRegion region, IFlag flag) {
        MutableComponent hover = new TranslatableComponent("cli.flag.state.set.link.hover", flag.getName(), region.getName());
        MutableComponent text = new TranslatableComponent("cli.flag.state.set.link.text");
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
        MutableComponent linkText = new TranslatableComponent("cli.flag.msg.text.link.text." + flag.doesOverride());
        MutableComponent hoverText = new TranslatableComponent("cli.flag.msg.text.link.hover." + flag.doesOverride());
        ChatFormatting color = flag.doesOverride() ? ChatFormatting.GREEN : ChatFormatting.GRAY;
        return buildTextWithHoverMsg(linkText, hoverText, color);
    }


    public static MutableComponent buildFlagOverrideToggleLink(IProtectedRegion region, IFlag flag, boolean shortLink) {
        MutableComponent linkText = new TranslatableComponent("cli.flag.override.link.text." + flag.doesOverride());
        MutableComponent hoverText = new TranslatableComponent("cli.flag.override.link.hover." + !flag.doesOverride(), flag.getName(), region.getName());
        if (shortLink) {
            linkText = new TranslatableComponent("cli.flag.override.link.text.toggle");
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
        MutableComponent hover = new TranslatableComponent("cli.flag.msg.text.set.link.hover", flag.getName(), region.getName());
        MutableComponent text = new TranslatableComponent("cli.flag.msg.text.set.link.text");
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
        MutableComponent hover = new TranslatableComponent("cli.flag.msg.text.set.default", flag.getName(), region.getName());
        MutableComponent text = new TranslatableComponent("cli.link.remove");
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
        MutableComponent hover = new TranslatableComponent("cli.flag.msg.mute.set.link.hover", flag.getName(), region.getName());
        MutableComponent text = new TranslatableComponent("cli.flag.msg.mute.set.link.text." + !flag.getFlagMsg().isMuted());
        if (shortLink) {
            text = new TranslatableComponent("cli.flag.msg.mute.set.link.text.toggle");
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
        MutableComponent flagMsgText = new TextComponent(flagMsg);
        MutableComponent hoverText = new TextComponent(flag.getFlagMsg().getMsg());
        // if flag has default msg, use default msg
        if (flag.getFlagMsg().isDefault()) {
            switch (region.getRegionType()) {
                case GLOBAL: {
                    hoverText = new TranslatableComponent("flag.player.msg.push.deny.global.default");
                    break;
                }
                case DIMENSION: {
                    hoverText = new TranslatableComponent("flag.player.msg.push.deny.dim.default");
                    break;
                }
                case LOCAL: {
                    hoverText = new TranslatableComponent("flag.player.msg.push.deny.local.default");
                    break;
                }
            }
        }
        return buildTextWithHoverMsg(flagMsgText, hoverText, WHITE);
    }

    /**
     * Message: [set] [x]: 'msg' <br></br>
     */
    public static MutableComponent buildFlagMessageComponent(IProtectedRegion region, IFlag flag) {
        MutableComponent editLink = buildFlagMessageEditLink(region, flag);
        MutableComponent clearLink = buildFlagMessageClearLink(region, flag);
        MutableComponent flagMsgTextWithHover = buildFlagMessageHoverText(region, flag);
        return new TranslatableComponent("%s %s '%s'", editLink, clearLink, flagMsgTextWithHover);
    }

    private static MutableComponent buildFlagToggleLink(String cmd, String langKey, boolean toggleActiveState, String... subCmds) {
        String cmdStr = appendSubCommand(cmd, subCmds);
        MutableComponent linkText = new TranslatableComponent("cli.flag." + langKey + ".link.text");
        MutableComponent hoverText = new TranslatableComponent("cli.flag." + langKey + ".link.hover");
        ChatFormatting color = toggleActiveState ? ADD_CMD_COLOR : REMOVE_CMD_COLOR;
        return buildExecuteCmdComponent(linkText, hoverText, cmdStr, RUN_COMMAND, color);
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
        String cmd = buildCommandStr(DIM.toString(), dimCache.dimensionKey().location().toString(), DELETE_ALL.toString(), REGIONS.toString());
        MutableComponent hover = new TranslatableComponent("cli.msg.info.dim.region.remove.all.link.hover", dimCache.getDimensionalRegion().getName());
        MutableComponent text = new TranslatableComponent("cli.link.remove");
        return buildExecuteCmdComponent(text, hover, cmd, RUN_COMMAND, REMOVE_CMD_COLOR);
    }

    public static MutableComponent buildRemoveAllRegionsLink(DimensionRegionCache dimCache) {
        String cmd = buildCommandStr(DIM.toString(), dimCache.dimensionKey().location().toString(), DELETE_ALL.toString(), REGIONS.toString(), FOREVER.toString(), SERIOUSLY.toString());
        MutableComponent hover = new TranslatableComponent("cli.msg.info.dim.region.remove.all.link.hover", dimCache.getDimensionalRegion().getName());
        MutableComponent text = new TranslatableComponent("cli.link.remove");
        return buildExecuteCmdComponent(text, hover, cmd, RUN_COMMAND, REMOVE_CMD_COLOR);
    }


    public static MutableComponent buildRemoveRegionLink(IProtectedRegion region) {
        String cmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), DELETE.toString(), region.getName(), "-y");
        MutableComponent hover = new TranslatableComponent("cli.msg.info.dim.region.remove.link.hover", region.getName());
        MutableComponent text = new TranslatableComponent("cli.link.remove");
        return buildExecuteCmdComponent(text, hover, cmd, RUN_COMMAND, REMOVE_CMD_COLOR);
    }

    public static MutableComponent buildRemoveRegionEntry(IProtectedRegion parent, IProtectedRegion region) {
        MutableComponent regionRemoveLink;
        switch (parent.getRegionType()) {
            case GLOBAL: {
                regionRemoveLink = new TranslatableComponent("%s %s", buildDimResetComponent((DimensionalRegion) region), buildRegionInfoLink(region));
                break;
            }
            case DIMENSION: {
                MutableComponent removeLink = new TextComponent("");
                MutableComponent childIndicator = buildTextWithHoverMsg(new TextComponent("*"), new TranslatableComponent("cli.msg.info.dim.region.child.hover"), GOLD);
                if (parent.hasChild(region)) {
                    removeLink = new TranslatableComponent("%s %s%s", buildDimSuggestRegionRemovalLink((IMarkableRegion) region), buildRegionInfoLink(region), childIndicator);
                } else {
                    removeLink = new TranslatableComponent("%s %s", buildDimSuggestRegionRemovalLink((IMarkableRegion) region), buildRegionInfoLink(region));
                }
                regionRemoveLink = new TranslatableComponent("%s @ %s", removeLink, buildRegionTeleportLink((IMarkableRegion) region));
                break;
            }
            case LOCAL: {
                regionRemoveLink = new TranslatableComponent("%s %s", buildRegionRemoveChildLink(parent, region), buildRegionInfoLink(region));
                break;
            }
            default:
                throw new IllegalArgumentException();
        }
        return new TranslatableComponent(" - %s", regionRemoveLink);
    }

    private static MutableComponent buildDimResetComponent(DimensionalRegion region) {
        String cmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), CommandConstants.RESET.toString(), DIM.toString());
        MutableComponent hover = new TranslatableComponent("cli.dim.reset.dim.link.hover", region.getName());
        MutableComponent text = new TranslatableComponent("cli.link.action.undo.text");
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
            paginationComponents.add(new TranslatableComponent("cli.msg.info.pagination.error.index", pageNo, numberOfPages - 1).withStyle(RED));
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
        return hasMultiplePages && pageNo < numberOfPages - 1
                ? buildExecuteCmdComponent(new TranslatableComponent("cli.msg.info.pagination.last.text"), new TranslatableComponent("cli.msg.info.pagination.last.hover"), buildPageCommand(cmd, numberOfPages - 1), RUN_COMMAND, LINK_COLOR)
                : ComponentUtils.wrapInSquareBrackets(new TranslatableComponent("cli.msg.info.pagination.last.text")).withStyle(INACTIVE_LINK_COLOR);
    }

    private static MutableComponent buildNextLinkArrow(String cmd, int pageNo, int numberOfPages, boolean hasMultiplePages) {
        return hasMultiplePages && pageNo < numberOfPages - 1
                ? buildExecuteCmdComponent(new TranslatableComponent("cli.msg.info.pagination.next.text"), new TranslatableComponent("cli.msg.info.pagination.next.hover"), buildPageCommand(cmd, Math.min(pageNo + 1, numberOfPages - 1)), RUN_COMMAND, LINK_COLOR)
                : ComponentUtils.wrapInSquareBrackets(new TranslatableComponent("cli.msg.info.pagination.next.text")).withStyle(INACTIVE_LINK_COLOR);
    }

    private static MutableComponent buildPrevLinkArrow(String cmd, int pageNo, boolean hasMultiplePages) {
        return hasMultiplePages && pageNo > FIRST_PAGE_IDX
                ? buildExecuteCmdComponent(new TranslatableComponent("cli.msg.info.pagination.previous.text"), new TranslatableComponent("cli.msg.info.pagination.previous.hover"), buildPageCommand(cmd, Math.max(pageNo - 1, FIRST_PAGE_IDX)), RUN_COMMAND, LINK_COLOR)
                : ComponentUtils.wrapInSquareBrackets(new TranslatableComponent("cli.msg.info.pagination.previous.text")).withStyle(INACTIVE_LINK_COLOR);
    }

    private static MutableComponent buildFirstLinkArrow(String cmd, int pageNo, boolean hasMultiplePages) {
        return hasMultiplePages && pageNo != FIRST_PAGE_IDX
                ? buildExecuteCmdComponent(new TranslatableComponent("cli.msg.info.pagination.first.text"), new TranslatableComponent("cli.msg.info.pagination.first.hover"), buildPageCommand(cmd, FIRST_PAGE_IDX), RUN_COMMAND, LINK_COLOR)
                : ComponentUtils.wrapInSquareBrackets(new TranslatableComponent("cli.msg.info.pagination.first.text")).withStyle(INACTIVE_LINK_COLOR);
    }

    private static MutableComponent buildPaginationControl(MutableComponent front, MutableComponent back, int pageNo, int maxPage, MutableComponent forward, MutableComponent last) {
        // [<<]  [<]  x/n  [>]  [>>]
        MutableComponent pageIndicator = new TextComponent((pageNo + 1) + "/" + (maxPage));
        return new TranslatableComponent(" %s  %s  %s  %s  %s", front, back, pageIndicator, forward, last);
    }

    // [x]
    public static MutableComponent buildParentClearLink(IMarkableRegion region) {
        String clearRegionParentCmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), PARENT.toString(), REMOVE.toString());
        MutableComponent parentClearLinkText = new TranslatableComponent("cli.link.remove");
        MutableComponent parentClearHoverText = new TranslatableComponent("cli.msg.info.region.parent.clear.link.hover", region.getParent().getName());
        return buildExecuteCmdComponent(parentClearLinkText, parentClearHoverText, clearRegionParentCmd, RUN_COMMAND, REMOVE_CMD_COLOR);
    }

    // No parent set [+]
    private static MutableComponent createParentAddLink(IProtectedRegion region) {
        String setRegionParentCmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), PARENT.toString(), SET.toString(), "");
        MutableComponent setParentLinkText = new TranslatableComponent("cli.link.add");
        MutableComponent setParentHoverText = new TranslatableComponent("cli.msg.info.region.parent.set.link.hover", region.getName());
        return new TranslatableComponent("%s %s",
                new TranslatableComponent("cli.msg.info.region.parent.null"),
                buildExecuteCmdComponent(setParentLinkText, setParentHoverText, setRegionParentCmd, RUN_COMMAND, GREEN));
    }


    public static MutableComponent buildRegionListHeader(IProtectedRegion region) {
        return buildHeader(new TranslatableComponent("cli.msg.info.header.in", buildRegionListChildrenLink(region), buildRegionInfoLink(region)));
    }


    // [n regions] [+]
    public static MutableComponent buildDimRegionsLink(DimensionRegionCache dimCache) {
        DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
        String command = buildCommandStr(DIM.toString(), dimRegion.getDim().location().toString(), LIST.toString(), CommandConstants.LOCAL.toString());
        MutableComponent text = new TranslatableComponent("cli.msg.dim.info.region.list.link.text", dimCache.getRegions().size());
        MutableComponent hover = new TranslatableComponent("cli.msg.dim.info.region.list.link.hover", dimRegion.getName());
        MutableComponent listLocalRegionsLink = buildExecuteCmdComponent(text, hover, command, RUN_COMMAND, LINK_COLOR);
        MutableComponent createRegionLink = buildDimCreateRegionLink(dimRegion);
        if (dimRegion.getChildren().isEmpty()) {
            return new TranslatableComponent("%s %s", text, createRegionLink);
        }
        return new TranslatableComponent("%s %s", listLocalRegionsLink, createRegionLink);
    }

    public static MutableComponent buildRegionListChildrenLink(IProtectedRegion region) {
        MutableComponent text = new TranslatableComponent("cli.msg.info.region.children.list.link.text", region.getChildren().size());
        MutableComponent hover = new TranslatableComponent("cli.msg.info.region.children.list.link.hover", region.getName());
        return switch (region.getRegionType()) {
            case GLOBAL -> {
                // [n dimensions(s)]
                Collection<String> dimensionList = RegionDataManager.get().getDimensionList();
                String command = buildCommandStr(GLOBAL.toString(), LIST.toString(), CHILDREN.toString());
                MutableComponent listDimRegionsLinkText = new TranslatableComponent("cli.msg.global.info.region.list.link.text", dimensionList.size());
                MutableComponent listDimRegionsHoverText = new TranslatableComponent("cli.msg.global.info.region.list.link.hover");
                yield buildExecuteCmdComponent(listDimRegionsLinkText, listDimRegionsHoverText, command, RUN_COMMAND, LINK_COLOR);
            }
            case DIMENSION -> {
                // [n children] [+]
                String command = buildCommandStr(DIM.toString(), region.getDim().location().toString(), LIST.toString(), CHILDREN.toString());
                MutableComponent listDimRegionsListLink = buildExecuteCmdComponent(text, hover, command, RUN_COMMAND, LINK_COLOR);
                if (region.getChildren().isEmpty()) {
                    yield new TranslatableComponent("%s %s", text, buildDimCreateRegionLink(region));
                }
                yield new TranslatableComponent("%s %s", listDimRegionsListLink, buildDimCreateRegionLink(region));
            }
            case LOCAL -> {
                // [n children] [+]
                String regionChildrenListLink = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), CHILDREN.toString());
                MutableComponent regionChildrenLink = buildExecuteCmdComponent(text, hover, regionChildrenListLink, RUN_COMMAND, LINK_COLOR);
                MutableComponent addChildrenLink = buildRegionAddChildrenLink(region);
                if (region.getChildren().isEmpty()) {
                    yield new TranslatableComponent("%s %s", text, addChildrenLink);
                }
                yield new TranslatableComponent("%s %s", regionChildrenLink, addChildrenLink);
            }
            default ->
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        };
    }

    public static MutableComponent buildRegionAddChildrenLink(IProtectedRegion region) {
        String addChildrenCmd = buildCommandStr(LOCAL.toString(), region.getDim().location().toString(), region.getName(), ADD.toString(), CHILD.toString(), "");
        MutableComponent addChildrenLinkText = new TranslatableComponent("cli.link.add");
        MutableComponent addChildrenHoverText = new TranslatableComponent("cli.msg.info.region.children.add.link.hover", region.getName());
        return buildExecuteCmdComponent(addChildrenLinkText, addChildrenHoverText, addChildrenCmd, SUGGEST_COMMAND, ADD_CMD_COLOR);
    }

    public static MutableComponent buildDimCreateRegionLink(IProtectedRegion region) {
        String dimCreateRegionCmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), CREATE.toString(), LOCAL.toString(), "");
        MutableComponent createRegionLinkText = new TranslatableComponent("cli.link.add");
        MutableComponent createRegionHoverText = new TranslatableComponent("cli.msg.dim.info.region.create.link.hover", region.getName());
        return buildExecuteCmdComponent(createRegionLinkText, createRegionHoverText, dimCreateRegionCmd, SUGGEST_COMMAND, ADD_CMD_COLOR);
    }

    public static MutableComponent buildFlagListLink(IProtectedRegion region) {
        Map<String, FlagCorrelation> flagsInHierarchy = getFlagMapRecursive(region, null);
        MutableComponent regionFlagNumber = buildTextWithHoverMsg(new TranslatableComponent("%s", region.getFlags().size()),
                new TranslatableComponent("Flags responsible in %s", region.getName()), LINK_COLOR);
        MutableComponent parentFlagsNumber = buildTextWithHoverMsg(new TranslatableComponent("%s", flagsInHierarchy.size() - region.getFlags().size()),
                new TranslatableComponent("Flags responsible in parent regions"), LINK_COLOR);
        MutableComponent flagListLinkText = new TranslatableComponent("cli.msg.info.region.flag.link.text",
                regionFlagNumber, parentFlagsNumber);
        MutableComponent flagListHoverText = new TranslatableComponent("cli.msg.info.region.flag.link.hover", region.getName());
        return switch (region.getRegionType()) {
            case GLOBAL -> {
                String flagListCmd = buildCommandStr(GLOBAL.toString(), LIST.toString(), FLAG.toString());
                MutableComponent flagListLink = buildExecuteCmdComponent(flagListLinkText, flagListHoverText, flagListCmd, RUN_COMMAND, LINK_COLOR);
                if (flagsInHierarchy.isEmpty()) {
                    flagListLink = flagListLinkText;
                }
                yield new TranslatableComponent("%s %s", flagListLink, buildAddFlagLink(region));
            }
            case DIMENSION -> {
                String flagListCmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), LIST.toString(), FLAG.toString());
                MutableComponent flagListLink = buildExecuteCmdComponent(flagListLinkText, flagListHoverText, flagListCmd, RUN_COMMAND, LINK_COLOR);
                if (flagsInHierarchy.isEmpty()) {
                    flagListLink = flagListLinkText;
                }
                yield new TranslatableComponent("%s %s", flagListLink, buildAddFlagLink(region));
            }
            case LOCAL -> {
                String listCmd = buildCommandStr(LOCAL.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), FLAG.toString());
                MutableComponent flagListLink = buildExecuteCmdComponent(flagListLinkText, flagListHoverText, listCmd, RUN_COMMAND, LINK_COLOR);
                if (flagsInHierarchy.isEmpty()) {
                    flagListLink = flagListLinkText;
                }
                yield new TranslatableComponent("%s %s", flagListLink, buildAddFlagLink(region));
            }
            default ->
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        };
    }

    public static MutableComponent buildAddFlagLink(IProtectedRegion region) {
        MutableComponent hoverText = new TranslatableComponent("cli.msg.info.region.flag.add.link.hover", region.getName());
        MutableComponent linkText = new TranslatableComponent("cli.link.add");
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
        MutableComponent linkText = new TranslatableComponent("cli.msg.info.region.state.link.text");
        MutableComponent hoverText = new TranslatableComponent("cli.msg.info.region.state.link.hover", region.getName());
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

    public static MutableComponent buildDimEnableLink(IProtectedRegion region) {
        String command = ArgumentUtil.buildCommandStr(DIM.toString(), region.getDim().location().toString(), ENABLE.toString());
        String onClickAction = region.isActive() ? "deactivate" : "activate";
        String hoverText = "cli.msg.info.state." + onClickAction;
        String linkText = "cli.msg.info.state.link." + (region.isActive() ? "activate" : "deactivate");
        ChatFormatting color = region.isActive() ? ADD_CMD_COLOR : REMOVE_CMD_COLOR;
        return new TranslatableComponent("%s: %s",
                new TranslatableComponent("cli.msg.info.state"),
                buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, color));
    }

    public static MutableComponent buildInfoComponent(String subjectLangKey, MutableComponent payload) {
        return new TranslatableComponent("%s: %s", new TranslatableComponent(subjectLangKey), payload);
    }

    public static MutableComponent buildRegionTeleportLink(IMarkableRegion region) {
        String teleportCmd = buildDimTeleportCmd(region.getDim(), "@s", region.getTpTarget());
        MutableComponent textInfo = new TextComponent(buildBlockPosLinkText(region.getTpTarget()));
        MutableComponent hoverInfo = new TranslatableComponent("cli.msg.info.region.area.tp.link.hover", region.getName(), region.getDim().location().toString());
        return buildExecuteCmdComponent(textInfo, hoverInfo, teleportCmd, RUN_COMMAND, TP_COLOR);
    }

    public static MutableComponent buildRegionSetTpLink(IMarkableRegion region) {
        String setTpPosCmd = buildCommandStr(LOCAL.toString(), region.getDim().location().toString(), region.getName(), AREA.toString(), TELEPORT.toString(), SET.toString(), "");
        MutableComponent linkText = new TranslatableComponent("cli.msg.info.region.area.tp.set.link.text");
        MutableComponent hoverText = new TranslatableComponent("cli.msg.info.region.area.tp.set.link.hover", region.getName());
        return buildExecuteCmdComponent(linkText, hoverText, setTpPosCmd, SUGGEST_COMMAND, LINK_COLOR);
    }

    public static MutableComponent buildDimSuggestRegionRemovalLink(IMarkableRegion region) {
        String cmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), DELETE.toString(), region.getName());
        MutableComponent hover = new TranslatableComponent("cli.msg.info.dim.region.remove.link.hover", region.getName());
        MutableComponent text = new TranslatableComponent("cli.link.remove");
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
        MutableComponent linkText = new TranslatableComponent("cli.link.remove");
        MutableComponent hoverText = new TranslatableComponent("cli.msg.info.region.group." + groupType.name + ".remove.link.hover", name, region.getName());
        MutableComponent regionRemoveLink;
        if (groupType == GroupType.PLAYER) {
            Player player = ServerLifecycleHooks.getCurrentServer().getPlayerList().getPlayerByName(name);
            boolean isOffline = player == null;
            if (isOffline) {
                MutableComponent offlinePlayerRemoveLink = buildRemoveLinkForOfflinePlayer(region, name, groupType, group, linkText, hoverText);
                return new TranslatableComponent(" - %s %s", offlinePlayerRemoveLink, buildGroupInfo(region, name, groupType));
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
            default ->
                throw new IllegalArgumentException();
        };
        return new TranslatableComponent(" - %s %s", regionRemoveLink, buildGroupInfo(region, name, groupType));
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
            default ->
                throw new IllegalArgumentException();
        };
    }

    public static MutableComponent buildGroupInfo(IProtectedRegion region, String groupMemberName, GroupType groupType) {
        return switch (groupType) {
            case PLAYER -> {
                Player player = ServerLifecycleHooks.getCurrentServer().getPlayerList().getPlayerByName(groupMemberName);
                if (player == null) {
                    yield new TranslatableComponent("%s %s",
                            new TextComponent(groupMemberName).withStyle(GRAY),
                            new TranslatableComponent("cli.msg.info.player.list.entry.offline"));
                } else {
                    yield buildPlayerHoverComponent(player);
                }

            }
            case TEAM -> {
                ServerLevel level = ServerLifecycleHooks.getCurrentServer().getLevel(region.getDim());
                if (level != null) {
                    Team team = level.getScoreboard().getPlayerTeam(groupMemberName);
                    yield team == null ? new TextComponent(groupMemberName) : buildTeamHoverComponent(team);
                } else {
                    yield new TextComponent(groupMemberName);
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
        MutableComponent linkText = new TranslatableComponent("cli.link.remove");
        MutableComponent linkHoverText = new TranslatableComponent("cli.msg.info.region.children.remove.link.hover", child.getName(), region.getName());
        return buildExecuteCmdComponent(linkText, linkHoverText, command, SUGGEST_COMMAND, REMOVE_CMD_COLOR);
    }

    public static MutableComponent buildRegionActionUndoLink(String cmd, CommandConstants toReplace, CommandConstants replacement) {
        String revertCmd = ArgumentUtil.revertCommand(cmd, toReplace, replacement);
        MutableComponent revertLinkText = new TranslatableComponent("cli.link.action.undo.text");
        MutableComponent revertLinkHover = new TranslatableComponent("cli.link.action.undo.hover");
        return buildExecuteCmdComponent(revertLinkText, revertLinkHover, revertCmd, RUN_COMMAND, DARK_RED);
    }

    public static MutableComponent buildRegionActionUndoLink(String cmd, String toReplace, String replacement) {
        String revertCmd = ArgumentUtil.revertCommand(cmd, toReplace, replacement);
        MutableComponent revertLinkText = new TranslatableComponent("cli.link.action.undo.text");
        MutableComponent revertLinkHover = new TranslatableComponent("cli.link.action.undo.hover");
        return buildExecuteCmdComponent(revertLinkText, revertLinkHover, revertCmd, RUN_COMMAND, DARK_RED);
    }

    public static MutableComponent buildRegionFlagInfoHeader(IProtectedRegion region) {
        MutableComponent res;
        switch (region.getRegionType()) {
            case GLOBAL:
                res = buildHeader(new TranslatableComponent("cli.msg.info.header.in", buildFlagListLink(region), buildRegionInfoLink(region)));
                break;
            case DIMENSION:
                res = buildHeader(new TranslatableComponent("cli.msg.info.header.in", buildFlagListLink(region), buildRegionInfoLink(region)));
                break;
            case LOCAL:
                res = buildHeader(new TranslatableComponent("cli.msg.info.header.in", buildFlagListLink(region), buildRegionInfoLink(region)));
                break;
            default:
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        }
        return res;
    }

    public static MutableComponent buildFlagInfoHeader(IProtectedRegion region, IFlag flag) {
        MutableComponent res;
        switch (region.getRegionType()) {
            case DIMENSION:
                res = buildHeader(new TranslatableComponent("cli.msg.info.header.flag.in", buildFlagInfoLink(region, flag), buildRegionInfoLink(region)));
                break;
            case LOCAL:
                res = buildHeader(new TranslatableComponent("cli.msg.info.header.flag.in", buildFlagInfoLink(region, flag), buildRegionInfoLink(region)));
                break;
            case GLOBAL:
                res = buildHeader(new TranslatableComponent("cli.msg.info.header.flag.in", buildFlagInfoLink(region, flag), buildRegionInfoLink(region)));
                break;
            default:
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        }
        return res;
    }
}
