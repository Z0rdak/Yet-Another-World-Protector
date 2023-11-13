package de.z0rdak.yawp.util;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.commands.CommandConstants;
import de.z0rdak.yawp.commands.RegionCommands;
import de.z0rdak.yawp.commands.arguments.ArgumentUtil;
import de.z0rdak.yawp.config.server.CommandPermissionConfig;
import de.z0rdak.yawp.config.server.FlagConfig;
import de.z0rdak.yawp.config.server.RegionConfig;
import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.area.IMarkableArea;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.group.GroupType;
import de.z0rdak.yawp.core.group.PlayerContainer;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.core.region.RegionType;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.command.CommandSource;
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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.commands.arguments.ArgumentUtil.*;
import static de.z0rdak.yawp.core.region.RegionType.LOCAL;
import static net.minecraft.util.text.TextFormatting.RESET;
import static net.minecraft.util.text.TextFormatting.*;
import static net.minecraft.util.text.event.ClickEvent.Action.RUN_COMMAND;
import static net.minecraft.util.text.event.ClickEvent.Action.SUGGEST_COMMAND;


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
        sendCmdFeedback(src, new TranslationTextComponent(langKey));
    }

    public static void sendMessage(PlayerEntity player, String translationKey) {
        player.sendMessage(new TranslationTextComponent(translationKey), player.getUUID());
    }

    public static void sendNotification(PlayerEntity player, IFormattableTextComponent msg) {
        player.displayClientMessage(msg, true);
    }

    public static String buildTeleportCmd(String tpSource, BlockPos target) {
        return "tp " + tpSource + " " + target.getX() + " " + target.getY() + " " + target.getZ();
    }

    public static String buildTeleportLinkText(RegistryKey<World> dim, BlockPos target) {
        return buildTeleportLinkText(dim.location().toString(), target);
    }

    public static String buildTeleportLinkText(String regionName, BlockPos target) {
        return regionName + " @ [" + buildBlockPosTeleportLinkText(target) + "]";
    }

    public static String shorterBlockPos(BlockPos target) {
        return "[" + buildBlockPosTeleportLinkText(target) + "]";
    }

    public static String shortBlockPos(BlockPos target) {
        return "[X=" + target.getX() + ", Y=" + target.getY() + ", Z=" + target.getZ() + "]";
    }

    public static String buildBlockPosTeleportLinkText(BlockPos target) {
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
        return new StringTextComponent(String.valueOf(BOLD))
                .append(header)
                .append(new StringTextComponent(String.valueOf(BOLD)));
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
        IFormattableTextComponent playerName = new StringTextComponent(team.getName());
        playerName.setStyle(playerName.getStyle()
                .withColor(LINK_COLOR)
                .withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, new TranslationTextComponent("cli.msg.info.region.affiliation.link.hover")))
                .withClickEvent(new ClickEvent(RUN_COMMAND, "/team list " + team.getName())));
        return playerName;
    }

    /***
     * [X,Y,Z], ..., [X,Y,Z]
     */
    public static IFormattableTextComponent buildBlockPosTpLinks(IMarkableRegion region) {
        List<IFormattableTextComponent> tpLinks = region.getArea().getMarkedBlocks()
                .stream()
                .map(pos -> buildDimensionalBlockTpLink(region.getDim(), pos))
                .collect(Collectors.toList());
        IFormattableTextComponent blockPosTpLinkList = new StringTextComponent("");
        tpLinks.forEach(tpLink -> blockPosTpLinkList.append(tpLink).append(" "));
        return blockPosTpLinkList;
    }

    /**
     * Location: [dimInfo] @ [tpCoordinates]
     */
    public static IFormattableTextComponent buildDimensionTeleportLink(IMarkableRegion region) {
        String executeCmdStr = buildDimTeleportCmd(region.getDim(), "@s", region.getTpTarget());
        IFormattableTextComponent cmdLinkText = new StringTextComponent(buildTeleportLinkText(region.getDim(), region.getTpTarget()));
        IFormattableTextComponent teleportCmdHoverText = new TranslationTextComponent("cli.msg.info.region.area.location.teleport", region.getName(), region.getDim().location().toString());
        return buildExecuteCmdComponent(cmdLinkText, teleportCmdHoverText, executeCmdStr, RUN_COMMAND, TP_COLOR);
    }

    /**
     * Area: Cuboid, Size: X=69, Y=10, Z=42 [<=expand=>] [<=max=>]
     */
    public static IFormattableTextComponent buildRegionAreaDetailComponent(IMarkableRegion region) {
        IMarkableArea area = region.getArea();
        IFormattableTextComponent areaInfo = new StringTextComponent("(" + area.getAreaType().areaType + ")");
        switch (area.getAreaType()) {
            case CUBOID: {
                CuboidArea cuboidArea = (CuboidArea) area;
                IFormattableTextComponent sizeInfo = buildAreaAxisInfoComponent(cuboidArea, Direction.Axis.X).append(", ")
                        .append(buildAreaAxisInfoComponent(cuboidArea, Direction.Axis.Y)).append(", ")
                        .append(buildAreaAxisInfoComponent(cuboidArea, Direction.Axis.Z)).append(", ");
                return areaInfo.append(" ").append(sizeInfo).append(buildRegionAreaExpandLink(region));
            }
            case CYLINDER:
                throw new NotImplementedException("cylinder");
            case SPHERE:
                throw new NotImplementedException("sphere");
            case POLYGON_3D:
                throw new NotImplementedException("polygon");
            case PRISM:
                throw new NotImplementedException("prism");
            default:
                throw new IllegalArgumentException("Invalid area type");
        }
    }

    /**
     * Axis=N
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
        int minBlockHeight = 0;
        int maxBlockHeight = 255;
        IFormattableTextComponent linkText = new TranslationTextComponent("cli.msg.info.region.area.area.expand.link.text");
        IFormattableTextComponent linkHover = new TranslationTextComponent("cli.msg.info.region.area.area.expand.link.hover");
        String expandCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), AREA.toString(), EXPAND.toString());
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
                String maxExpandCmd = appendSubCommand(expandCmd, String.valueOf(minBlockHeight), String.valueOf(maxBlockHeight));
                IFormattableTextComponent maxExpandLink = buildExecuteCmdComponent(maxExpandLinkText, maxExpandLinkHover, maxExpandCmd, RUN_COMMAND, LINK_COLOR);
                return expandLink.append(" ").append(maxExpandLink);
            }
            case CYLINDER:
                throw new NotImplementedException("cylinder");
            case SPHERE:
                throw new NotImplementedException("sphere");
            case POLYGON_3D:
                throw new NotImplementedException("polygon");
            case PRISM:
                throw new NotImplementedException("prism");
            default:
                throw new IllegalArgumentException("Invalid area type");
        }
    }

    /**
     * Marked Blocks: [X,Y,Z], ..., [X,Y,Z] [Set] [Show]
     */
    public static IFormattableTextComponent buildRegionAreaMarkingComponent(IMarkableRegion region) {
        switch (region.getArea().getAreaType()) {
            case CUBOID: {
                CuboidArea cuboidArea = (CuboidArea) region.getArea();
                String areaCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), AREA.toString());
                TranslationTextComponent setAreaLinkText = new TranslationTextComponent("cli.msg.info.region.area.area.set.link");
                TranslationTextComponent setAreaLinkHover = new TranslationTextComponent("cli.msg.info.region.area.area.set.hover", region.getName());
                String blocks = String.join(" ", cuboidArea.getMarkedBlocks().stream()
                        .map(MessageUtil::buildBlockCoordinateStr)
                        .collect(Collectors.toSet()));
                String setArea = appendSubCommand(areaCmd, SET.toString(), region.getArea().getAreaType().areaType, blocks);
                IFormattableTextComponent setAreaLink = buildExecuteCmdComponent(setAreaLinkText, setAreaLinkHover, setArea, SUGGEST_COMMAND, LINK_COLOR);

                TranslationTextComponent showAreaLinkText = new TranslationTextComponent("cli.msg.info.region.area.area.show.link");
                TranslationTextComponent showAreaLinkHover = new TranslationTextComponent("cli.msg.info.region.area.area.show.hover", region.getName());
                String showArea = appendSubCommand(areaCmd, "show");
                IFormattableTextComponent showAreaLink = buildExecuteCmdComponent(showAreaLinkText, showAreaLinkHover, showArea, RUN_COMMAND, LINK_COLOR);
                return buildBlockPosTpLinks(region).append(" ").append(setAreaLink).append(" ");//.append(showAreaLink);
            }
            case CYLINDER:
                throw new NotImplementedException("cylinder");
            case SPHERE:
                throw new NotImplementedException("sphere");
            case POLYGON_3D:
                throw new NotImplementedException("polygon");
            case PRISM:
                throw new NotImplementedException("prism");
            default:
                throw new IllegalArgumentException("Invalid area type");
        }
    }

    /**
     * TP-Anchor: [X,Y,Z] [Set]
     */
    public static IFormattableTextComponent buildRegionAreaTpComponent(IMarkableRegion region) {
        IFormattableTextComponent regionTpLink = buildRegionTeleportLink(region);
        IFormattableTextComponent setTpLink = buildRegionSetTpLink(region);
        return regionTpLink.append(" ").append(setTpLink);
    }

    public static IFormattableTextComponent buildTextWithHoverMsg(IFormattableTextComponent text, IFormattableTextComponent hoverText, TextFormatting color) {
        IFormattableTextComponent bracketedText = TextComponentUtils.wrapInSquareBrackets(text);
        bracketedText.setStyle(bracketedText.getStyle().withColor(color).withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, hoverText)));
        return bracketedText;
    }

    public static IFormattableTextComponent buildDimensionTeleportLink(IMarkableRegion region) {
        String cmdLinkText = buildTeleportLinkText(region.getDim(), region.getTpTarget());
        String executeCmdStr = buildDimTeleportCmd(region.getDim(), "@s", region.getTpTarget());
        IFormattableTextComponent teleportCmdHoverText = new TranslationTextComponent("cli.msg.info.region.spatial.location.teleport", region.getName(), region.getDim().location().toString());
        return buildExecuteCmdComponent(new StringTextComponent(cmdLinkText), teleportCmdHoverText, executeCmdStr, RUN_COMMAND, TP_COLOR);
    }

    public static IFormattableTextComponent buildHelpSuggestionLink(String translationKey, CommandConstants baseCmd, CommandConstants cmd) {
        String command = "/" + CommandPermissionConfig.BASE_CMD + " " + baseCmd + " " + cmd + " ";
        return new StringTextComponent(" ")
                .append(buildExecuteCmdComponent("=>", "chat.link.hover.command.copy", command, SUGGEST_COMMAND, SUGGEST_COLOR))
                .append(new StringTextComponent(" "))
                .append(new TranslationTextComponent(translationKey));
    }

    public static String buildDimCmdStr(IProtectedRegion region, CommandConstants constant) {
        return CommandUtil.buildCommandStr(DIM.toString(), region.getDim().location().toString(), constant.toString());
    }

    public static String buildRegionCmdStr(IProtectedRegion region, CommandConstants constant) {
        return CommandUtil.buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), constant.toString());
    }

    public static IFormattableTextComponent buildRegionEnableComponent(IMarkableRegion region) {
        String cmd = CommandUtil.buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), STATE.toString(), ENABLE.toString());
        String linkTextKey = "cli.msg.info.region.state.enable." + region.isActive() + ".link.text";
        String hoverTextKey = "cli.msg.info.region.state.enable." + !region.isActive() + ".link.hover";
        TextFormatting color = region.isActive() ? ADD_CMD_COLOR : REMOVE_CMD_COLOR;
        return buildExecuteCmdComponent(linkTextKey, hoverTextKey, cmd, RUN_COMMAND, color);
    }

    public static IFormattableTextComponent buildRegionPriorityComponent(IMarkableRegion region) {
        int defaultPriorityInc = RegionConfig.getDefaultPriorityInc();
        String incPriorityCmd = CommandUtil.buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), STATE.toString(), PRIORITY.toString(), INC.toString(), String.valueOf(defaultPriorityInc));
        IFormattableTextComponent incLinkText = new TranslationTextComponent("cli.msg.info.region.state.priority.increase.link.text", defaultPriorityInc);
        IFormattableTextComponent incHoverText = new TranslationTextComponent("cli.msg.info.region.state.priority.increase.link.hover", defaultPriorityInc);
        IFormattableTextComponent increaseLink = buildExecuteCmdComponent(incLinkText, incHoverText, incPriorityCmd, RUN_COMMAND, ADD_CMD_COLOR);
        String decPriorityCmd = CommandUtil.buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), STATE.toString(), PRIORITY.toString(), DEC.toString(), String.valueOf(defaultPriorityInc));
        IFormattableTextComponent decLinkText = new TranslationTextComponent("cli.msg.info.region.state.priority.decrease.link.text", defaultPriorityInc);
        IFormattableTextComponent decHoverText = new TranslationTextComponent("cli.msg.info.region.state.priority.decrease.link.hover", defaultPriorityInc);
        IFormattableTextComponent decreaseLink = buildExecuteCmdComponent(decLinkText, decHoverText, decPriorityCmd, RUN_COMMAND, REMOVE_CMD_COLOR);
        IFormattableTextComponent priorityValue = new StringTextComponent(String.valueOf(region.getPriority()));
        String setPriorityCmd = CommandUtil.buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), STATE.toString(), PRIORITY.toString(), "");
        IFormattableTextComponent setPriorityLinkText = new TranslationTextComponent("cli.msg.info.region.state.priority.set.link.text");
        IFormattableTextComponent setPriorityHoverText = new TranslationTextComponent("cli.msg.info.region.state.priority.set.link.hover");
        IFormattableTextComponent setPriorityLink = buildExecuteCmdComponent(setPriorityLinkText, setPriorityHoverText, setPriorityCmd, SUGGEST_COMMAND, SUGGEST_COLOR);
        return priorityValue.append(" ")
                .append(setPriorityLink).append(" ")
                .append(increaseLink).append(" ")
                .append(decreaseLink);
    }

    public static IFormattableTextComponent buildRegionAlertComponentLink(IMarkableRegion region) {
        String cmd = CommandUtil.buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), STATE.toString(), ALERT.toString());
        String linkTextKey = "cli.msg.info.region.state.alert." + !region.isMuted() + ".link.text";
        String hoverTextKey = "cli.msg.info.region.state.alert." + region.isMuted() + ".link.hover";
        TextFormatting color = region.isMuted() ? REMOVE_CMD_COLOR : ADD_CMD_COLOR;
        return buildExecuteCmdComponent(linkTextKey, hoverTextKey, cmd, RUN_COMMAND, color);
    }

    public static IFormattableTextComponent buildRegionInfoLink(IProtectedRegion region, RegionType type) {
        IFormattableTextComponent res;
        switch (type) {
            case GLOBAL: {
                String command = buildCommandStr(GLOBAL.toString(), INFO.toString());
                return buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, LINK_COLOR);
            }
            case DIMENSION: {
                String command = buildCommandStr(DIM.toString(), region.getDim().location().toString(), INFO.toString());
                IFormattableTextComponent hoverText = new TranslationTextComponent("cli.msg.dim.info");
                IFormattableTextComponent linkText = new StringTextComponent(region.getDim().location().toString());
                res = buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, LINK_COLOR);
                break;
            }
            case LOCAL: {
                String cmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), INFO.toString());
                IFormattableTextComponent regionInfoLinkText = new StringTextComponent(region.getName());
                IFormattableTextComponent regionInfoLinkHover = new TranslationTextComponent("cli.msg.info.region", region.getName());
                res = buildExecuteCmdComponent(regionInfoLinkText, regionInfoLinkHover, cmd, RUN_COMMAND, LINK_COLOR);
                break;
            }
            default:
                throw new IllegalStateException("Unexpected value: " + type);
        }
        return res;
    }

    public static IFormattableTextComponent buildRegionSpatialPropLink(IMarkableRegion region) {
        String showSpatialPropLink = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), SPATIAL.toString());
        IFormattableTextComponent spatialPropLinkText = new TranslationTextComponent("cli.msg.info.region.spatial.link.text");
        IFormattableTextComponent spatialPropHoverText = new TranslationTextComponent("cli.msg.info.region.spatial.link.hover", region.getName());
        return buildExecuteCmdComponent(spatialPropLinkText, spatialPropHoverText, showSpatialPropLink, RUN_COMMAND, LINK_COLOR);
    }

    public static IFormattableTextComponent buildRegionOverviewHeader(IProtectedRegion region, RegionType type) {
        IFormattableTextComponent res;
        switch (type) {
            case GLOBAL: {
                IFormattableTextComponent clipBoardDumpLink = buildExecuteCmdComponent("cli.msg.global.overview.header.dump.link.text", "cli.msg.global.overview.header.dump.link.hover", region.serializeNBT().getPrettyDisplay().getString(), ClickEvent.Action.COPY_TO_CLIPBOARD, GOLD);
                return buildHeader(new TranslationTextComponent("cli.msg.info.header.for", clipBoardDumpLink, buildRegionInfoLink(region, RegionType.GLOBAL)));
            }
            case DIMENSION: {
                IFormattableTextComponent clipBoardDumpLink = buildExecuteCmdComponent("cli.msg.dim.overview.header.dump.link.text", "cli.msg.dim.overview.header.dump.link.hover", region.serializeNBT().getPrettyDisplay().getString(), ClickEvent.Action.COPY_TO_CLIPBOARD, GOLD);
                res = buildHeader(new TranslationTextComponent("cli.msg.info.header.for", clipBoardDumpLink, buildRegionInfoLink(region, RegionType.DIMENSION)));
                break;
            }
            case LOCAL: {
                IFormattableTextComponent clipBoardDumpLink = buildExecuteCmdComponent("cli.msg.info.region.overview.dump.link.text", "cli.msg.info.region.overview.dump.link.hover", region.serializeNBT().getPrettyDisplay().getString(), ClickEvent.Action.COPY_TO_CLIPBOARD, GOLD);
                res = buildHeader(new TranslationTextComponent("cli.msg.info.header.for", clipBoardDumpLink, buildRegionInfoLink(region, LOCAL)));
                break;
            }
            default:
                throw new IllegalStateException("Unexpected value: " + type);
        }
        return res;
    }

    /**
     * Players: [n player(s)] [+]
     */
    public static IFormattableTextComponent buildPlayerListLink(IProtectedRegion region, PlayerContainer players, String affiliation, RegionType regionType) {
        IFormattableTextComponent hoverText = new TranslationTextComponent("cli.msg.info.region.affiliation.player.list.link.hover", affiliation, region.getName());
        IFormattableTextComponent linkText = new TranslationTextComponent("cli.msg.info.region.affiliation.player.list.link.text", players.getPlayers().size());
        IFormattableTextComponent res;
        switch (regionType) {
            case GLOBAL: {
                String cmd = buildCommandStr(GLOBAL.toString(), LIST.toString(), group, PLAYER.toString());
                return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            case DIMENSION: {
                String cmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), LIST.toString(), affiliation, PLAYER.toString());
                res = buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
                break;
            }
            case LOCAL: {
                String cmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), affiliation, PLAYER.toString());
                res = buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
                break;
            }
            default:
                throw new IllegalArgumentException();
        }
        return res;
    }

    /**
     * Teams: [n team(s)] [+]
     */
    public static IFormattableTextComponent buildTeamListLink(IProtectedRegion region, PlayerContainer teams, String affiliation, RegionType regionType) {
        IFormattableTextComponent hoverText = new TranslationTextComponent("cli.msg.info.region.affiliation.team.list.link.hover", affiliation, region.getName());
        IFormattableTextComponent linkText = new TranslationTextComponent("cli.msg.info.region.affiliation.team.list.link.text", teams.getTeams().size());
        IFormattableTextComponent res;
        switch (regionType) {
            case GLOBAL: {
                String cmd = buildCommandStr(GLOBAL.toString(), LIST.toString(), GROUP.toString(), group, TEAM.toString());
                return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            case DIMENSION: {
                String cmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), LIST.toString(), affiliation, TEAM.toString());
                res = buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
                break;
            }
            case LOCAL: {
                String cmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), affiliation, TEAM.toString());
                res = buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
                break;
            }
            default:
                throw new IllegalArgumentException();
        }
        return res;
    }

    public static IFormattableTextComponent buildAddAffiliateLink(IProtectedRegion region, String affiliation, AffiliationType affiliationType, RegionType regionType) {
        IFormattableTextComponent linkText = new TranslationTextComponent("cli.link.add");
        IFormattableTextComponent hoverText = new TranslationTextComponent("cli.msg.info.region.affiliation." + affiliationType.name + ".add.link.hover", affiliation, region.getName());
        String subCmd = " " + affiliationType.name + " " + affiliation + " ";
        IFormattableTextComponent res;
        switch (regionType) {
            case GLOBAL: {
                String cmd = buildCommandStr(GLOBAL.toString()) + " " + subCmd;
                return buildExecuteCmdComponent(linkText, hoverText, cmd, SUGGEST_COMMAND, ADD_CMD_COLOR);
            }
            case LOCAL: {
                String cmd = buildRegionCmdStr(region, ADD) + subCmd;
                res = buildExecuteCmdComponent(linkText, hoverText, cmd, SUGGEST_COMMAND, ADD_CMD_COLOR);
                break;
            }
            case DIMENSION: {
                String cmd = buildDimCmdStr(region, ADD) + subCmd;
                res = buildExecuteCmdComponent(linkText, hoverText, cmd, SUGGEST_COMMAND, ADD_CMD_COLOR);
                break;
            }
            default:
                throw new IllegalArgumentException();
        }
        return res;
    }

    public static IFormattableTextComponent buildAffiliationLinks(IProtectedRegion region, RegionType regionType) {
        IFormattableTextComponent affiliationLinks = new StringTextComponent("");
        List<String> affiliations = Arrays.asList("owner", "member");
        affiliations.forEach(affiliation -> {
            switch (affiliation) {
                case "owner": {
                    int affiliationSize = region.getOwners().getPlayers().size() + region.getOwners().getTeams().size();
                    affiliationLinks.append(buildAffiliationLink(region, affiliation, affiliationSize, regionType)).append(" ");
                }
                break;
                case "member": {
                    int affiliationSize = region.getMembers().getPlayers().size() + region.getMembers().getTeams().size();
                    affiliationLinks.append(buildAffiliationLink(region, affiliation, affiliationSize, regionType)).append(" ");
                }
                break;
            }
        });
        return affiliationLinks;
    }

    public static IFormattableTextComponent buildAffiliationHeader(IProtectedRegion region, String affiliation, RegionType regionType) {
        int amountOwners = (region.getOwners().getTeams().size() + region.getOwners().getPlayers().size());
        int amountMembers = (region.getMembers().getTeams().size() + region.getMembers().getPlayers().size());
        int affiliationSize = affiliation.equals("owner") ? amountOwners : amountMembers;
        IFormattableTextComponent affiliationLink = buildAffiliationLink(region, affiliation, affiliationSize, regionType);
        return buildHeader(new TranslationTextComponent("cli.msg.info.header.in", affiliationLink, buildRegionInfoLink(region, regionType)));
    }

    public static IFormattableTextComponent buildAffiliationHeader(IProtectedRegion region, String affiliation, AffiliationType affiliationType, RegionType regionType) {
        return new TranslationTextComponent("cli.msg.info.region.affiliation." + affiliationType.name + ".list", buildRegionInfoLink(region, regionType), affiliation);
    }

    public static IFormattableTextComponent buildAffiliationLink(IProtectedRegion region, String affiliation, int affiliationSize, RegionType regionType) {
        IFormattableTextComponent linkText = new TranslationTextComponent("cli.msg.info.region.affiliation.list.link.text", affiliationSize, affiliation);
        IFormattableTextComponent hoverText = new TranslationTextComponent("cli.msg.info.region.affiliation.list.link.hover", affiliation, region.getName());
        IFormattableTextComponent res;
        switch (regionType) {
            case GLOBAL: {
                String cmd = buildCommandStr(GLOBAL.toString(), LIST.toString(), group);
                return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            case LOCAL: {
                String cmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), affiliation);
                res = buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
                break;
            }
            case DIMENSION: {
                String cmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), LIST.toString(), affiliation);
                res = buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
                break;
            }
            default:
                throw new IllegalArgumentException();
        }
        return res;
    }

    // TODO: combine team and player list link
    public static IFormattableTextComponent buildAffiliationTeamListLink(IProtectedRegion region, String affiliation, RegionType regionType) {
        // Teams: [n team(s)] [+]
        PlayerContainer playerContainer = affiliation.equals("owner") ? region.getOwners() : region.getMembers();
        IFormattableTextComponent teams = new TranslationTextComponent("cli.msg.info.region.affiliation.team").append(": ");
        IFormattableTextComponent teamAddLink = buildAddAffiliateLink(region, affiliation, AffiliationType.TEAM, regionType);
        IFormattableTextComponent teamListLink = playerContainer.hasTeams()
                ? buildTeamListLink(region, playerContainer, affiliation, regionType)
                : new TranslationTextComponent("cli.msg.info.region.affiliation.team.list.link.text", playerContainer.getTeams().size());
        teams.append(teamListLink).append(teamAddLink);
        return teams;
    }

    // TODO: combine team and player list link
    public static IFormattableTextComponent buildAffiliationPlayerListLink(IProtectedRegion region, String affiliation, RegionType regionType) {
        // Players: [n player(s)] [+]
        PlayerContainer playerContainer = affiliation.equals("owner") ? region.getOwners() : region.getMembers();
        IFormattableTextComponent players = new TranslationTextComponent("cli.msg.info.region.affiliation.player").append(": ");
        IFormattableTextComponent playersAddLink = buildAddAffiliateLink(region, affiliation, AffiliationType.PLAYER, regionType);
        IFormattableTextComponent playerListLink = playerContainer.hasPlayers()
                ? buildPlayerListLink(region, playerContainer, affiliation, regionType)
                : new TranslationTextComponent("cli.msg.info.region.affiliation.player.list.link.text", playerContainer.getPlayers().size());
        players.append(playerListLink).append(playersAddLink);
        return players;
    }

    /**
     * Creates a TextComponent for flag removal, followed by the flag infos
     *
     * @param region
     * @param flag
     * @param regionType
     * @return - [x] [flagname] [] [] []
     */
    private static IFormattableTextComponent buildRemoveFlagEntry(IProtectedRegion region, IFlag flag, RegionType regionType) {
        String cmd;
        switch (regionType) {
            case GLOBAL: {
                cmd = buildCommandStr(GLOBAL.toString(), REMOVE.toString(), FLAG.toString(), flag.getName());
                break;
            }
            case DIMENSION: {
                cmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), REMOVE.toString(), FLAG.toString(), flag.getName());
                break;
            }
            case LOCAL: {
                cmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), REMOVE.toString(), FLAG.toString(), flag.getName());
                break;
            }
            default:
                throw new IllegalArgumentException();
        }
        IFormattableTextComponent hoverText = new TranslationTextComponent("cli.msg.info.region.flag.remove.link.hover", flag.getName(), region.getName());
        IFormattableTextComponent linkText = new TranslationTextComponent("cli.link.remove");
        IFormattableTextComponent flagRemoveLink = buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, REMOVE_CMD_COLOR);
        return new StringTextComponent(" - ")
                .append(flagRemoveLink)
                .append(" ")
                .append(buildFlagQuickActionComponent(region, flag, regionType, cmd));
    }

    /**
     * Creates a TextComponent with a Link for displaying the flag info. <br></br>
     * Text: [flagname] [+] [~] [!] <br></br>
     * Where <br></br>
     * - [+] is a quick link to toggle the flag active state, <br></br>
     * - [!] is a quick link to toggle the flag invert state, <br></br>
     * - [~] is a quick link to toggle the flag mute state, <br></br>
     *
     * @param region
     * @param flag
     * @param regionType
     * @param cmd
     * @return text component for quick flag actions [flagname] [+] [~] [!]
     */
    private static IFormattableTextComponent buildFlagQuickActionComponent(IProtectedRegion region, IFlag flag, RegionType regionType, String cmd) {
        return buildFlagInfoLink(region, flag, regionType)
                .append(" ")
                .append(buildFlagActiveToggleLink(region, regionType, flag))
                .append(" ")
                .append(buildFlagMuteToggleLink(region, regionType, flag))
                .append(" ")
                .append(buildFlagInvertToggleLink(region, regionType, flag));
    }

    /**
     * Creates a TextComponent for displaying the flag info  <br></br>
     * Text: [$flag-name] <br></br>
     * Link: /wp flag dim $dim $flag-name info  <br></br>
     *
     * @param region     involved in creating command link for
     * @param flag       involved in creating command link for
     * @param regionType region type for distinction
     * @return TextComponent [$flag-name] with a link for the flag info
     */
    public static IFormattableTextComponent buildFlagInfoLink(IProtectedRegion region, IFlag flag, RegionType regionType) {
        IFormattableTextComponent text = new StringTextComponent(flag.getName());
        IFormattableTextComponent hoverText = new TranslationTextComponent("cli.flag.info.hover", flag.getName(), region.getName());
        switch (regionType) {
            case GLOBAL: {
                String cmd = buildCommandStr(FLAG.toString(), GLOBAL.toString(), flag.getName(), INFO.toString());
                // return buildExecuteCmdComponent(text, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
                throw new NotImplementedException("Not implemented yet!");
            }
            case DIMENSION: {
                String cmd = buildCommandStr(FLAG.toString(), DIM.toString(), region.getDim().location().toString(), INFO.toString());
                return buildExecuteCmdComponent(text, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            case LOCAL: {
                String cmd = buildCommandStr(FLAG.toString(), CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), flag.getName(), INFO.toString());
                return buildExecuteCmdComponent(text, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            default:
                throw new IllegalStateException("Unexpected value: " + regionType);
        }
    }

    /**
     * Builds the flag info component for the given flag and region. <br></br>
     * == Flag info for [flagname] of [region] == <br></br>
     * Enabled: [yes] <br></br>
     * Inverted: [no] <br></br>
     * Muted: [no] <br></br>
     * Msg [set] [x]: 'msg' <br></br>
     *
     * @param region
     * @param flag
     * @param regionType
     * @return == Flag info for [flagname] of [region] ==
     */
    public static IFormattableTextComponent buildFlagInfoComponent(IProtectedRegion region, IFlag flag, RegionType regionType) {
        IFormattableTextComponent header = buildFlagInfoHeader(region, flag, regionType);
        switch (regionType) {
            case GLOBAL: {
                throw new NotImplementedException("Not implemented yet!");
            }
            case DIMENSION: {

                return header;
            }
            case LOCAL: {

                return header;
            }
            default:
                throw new IllegalStateException("Unexpected value: " + regionType);
        }
    }

    public static IFormattableTextComponent buildFlagActiveToggleLink(IProtectedRegion region, RegionType regionType, IFlag flag) {
        switch (regionType) {
            case GLOBAL:
                throw new NotImplementedException("No yet implemented");
            case DIMENSION: {
                String cmd = buildCommandStr(FLAG.toString(), DIM.toString(), region.getDim().location().toString(), flag.getName());
                return buildFlagToggleLink(cmd, "enable", flag.isActive(), ENABLE.toString());
            }
            case LOCAL: {
                String cmd = buildCommandStr(FLAG.toString(), CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), flag.getName());
                return buildFlagToggleLink(cmd, "enable", flag.isActive(), ENABLE.toString());
            }
            case TEMPLATE:
                throw new NotImplementedException("No supported yet");
            default:
                throw new IllegalStateException("Unexpected value: " + regionType);
        }
    }

    public static IFormattableTextComponent buildFlagInvertToggleLink(IProtectedRegion region, RegionType regionType, IFlag flag) {
        switch (regionType) {
            case GLOBAL:
                throw new NotImplementedException("No yet implemented");
            case DIMENSION: {
                String cmd = buildCommandStr(FLAG.toString(), DIM.toString(), region.getDim().location().toString(), flag.getName());
                return buildFlagToggleLink(cmd, "override", flag.doesOverride(), OVERRIDE.toString());
            }
            case LOCAL: {
                String cmd = buildCommandStr(FLAG.toString(), CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), flag.getName());
                return buildFlagToggleLink(cmd, "override", flag.doesOverride(), OVERRIDE.toString());
            }
            case TEMPLATE:
                throw new NotImplementedException("No supported yet");
            default:
                throw new IllegalStateException("Unexpected value: " + regionType);
        }
    }

    public static IFormattableTextComponent buildFlagMuteToggleLink(IProtectedRegion region, RegionType regionType, IFlag flag) {
        switch (regionType) {
            case GLOBAL:
                throw new NotImplementedException("No yet implemented");
            case DIMENSION: {
                String cmd = buildCommandStr(FLAG.toString(), DIM.toString(), region.getDim().location().toString(), flag.getName());
                return buildFlagToggleLink(cmd, "msg.mute", !flag.getFlagMsg().isMuted(), MSG.toString(), MUTE.toString());
            }
            case LOCAL: {
                String cmd = buildCommandStr(FLAG.toString(), CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), flag.getName());
                return buildFlagToggleLink(cmd, "msg.mute", !flag.getFlagMsg().isMuted(), MSG.toString(), MUTE.toString());
            }
            case TEMPLATE:
                throw new NotImplementedException("No supported yet");
            default:
                throw new IllegalStateException("Unexpected value: " + regionType);
        }
    }

    public static IFormattableTextComponent buildFlagMessageEditLink(IProtectedRegion region, RegionType regionType, IFlag flag) {
        // TODO: consider player flag specific messages
        // TODO: build clear link when not default and use it
        // TODO: use different text than quick action text
        boolean hasDefaultMsg = flag.getFlagMsg().isDefault();
        IFormattableTextComponent flagMsgText = new TranslationTextComponent("cli.info.flag.state.msg.text.link.text", flag.getFlagMsg().getMsg());
        IFormattableTextComponent hoverText = new TranslationTextComponent("cli.info.flag.state.msg.text.link.hover",
                buildFlagInfoLink(region, flag, regionType), buildRegionInfoLink(region, regionType));
        switch (regionType) {
            case GLOBAL: {
                String cmd = buildCommandStr(FLAG.toString(), GLOBAL.toString(), flag.getName(), flag.getFlagMsg().getMsg());
                if (hasDefaultMsg) {
                    flagMsgText = new StringTextComponent(FlagConfig.getRawGlobalFlagMsg());
                }
                return buildExecuteCmdComponent(flagMsgText, hoverText, cmd, SUGGEST_COMMAND, LINK_COLOR);
            }
            case DIMENSION: {
                String cmd = buildCommandStr(FLAG.toString(), DIM.toString(), region.getDim().location().toString(), flag.getName(), flag.getFlagMsg().getMsg());
                if (hasDefaultMsg) {
                    flagMsgText = new StringTextComponent(FlagConfig.getRawDimFlagMsg());
                }
                return buildExecuteCmdComponent(flagMsgText, hoverText, cmd, SUGGEST_COMMAND, LINK_COLOR);
            }
            case LOCAL: {
                String cmd = buildCommandStr(FLAG.toString(), LOCAL.toString(), region.getDim().location().toString(), region.getName(), flag.getName(), flag.getFlagMsg().getMsg());
                if (hasDefaultMsg) {
                    flagMsgText = new StringTextComponent(FlagConfig.getRawLocalFlagMsg());
                }
                return buildExecuteCmdComponent(flagMsgText, hoverText, cmd, SUGGEST_COMMAND, LINK_COLOR);
            }
            default:
                throw new IllegalStateException("Unexpected value: " + regionType);
        }
    }

    private static IFormattableTextComponent buildFlagToggleLink(String cmd, String langKey, boolean toggleActiveState, String... subCmds) {
        String cmdStr = appendSubCommand(cmd, subCmds);
        IFormattableTextComponent linkText = new TranslationTextComponent("cli.flag." + langKey + ".link.text");
        IFormattableTextComponent hoverText = new TranslationTextComponent("cli.flag." + langKey + ".link.hover");
        TextFormatting color = toggleActiveState ? ADD_CMD_COLOR : REMOVE_CMD_COLOR;
        return buildExecuteCmdComponent(linkText, hoverText, cmdStr, RUN_COMMAND, color);
    }

    public static List<IFormattableTextComponent> buildRemoveFlagEntries(IProtectedRegion region, List<IFlag> flags, RegionType regionType) {
        return flags.stream().map(flag -> buildRemoveFlagEntry(region, flag, regionType)).collect(Collectors.toList());
    }

    public static List<IFormattableTextComponent> buildResetDimensionalRegionEntries(IProtectedRegion parent, List<DimensionRegionCache> regions, RegionType parentType) {
        return regions.stream().map(region -> buildRemoveRegionEntry(parent, region.getDimensionalRegion(), parentType)).collect(Collectors.toList());
    }

    public static List<IFormattableTextComponent> buildRemoveRegionEntries(IProtectedRegion parent, List<IMarkableRegion> regions, RegionType parentType) {
        return regions.stream().map(region -> buildRemoveRegionEntry(parent, region, parentType)).collect(Collectors.toList());
    }

    public static IFormattableTextComponent buildRemoveRegionEntry(IProtectedRegion parent, IMarkableRegion region, RegionType parentType) {
        Style resetStyle = Style.EMPTY.withColor(WHITE).withHoverEvent(null).withClickEvent(null);
        IFormattableTextComponent separator = new StringTextComponent(" ").setStyle(resetStyle);
        IFormattableTextComponent regionRemoveLink;
        switch (parentType) {
            case GLOBAL: {
                YetAnotherWorldProtector.LOGGER.info("reseting global region - just kidding its not implemented yet");
                return new StringTextComponent("");
                //throw new NotImplementedException("todo");
            }
            case DIMENSION: {
                IFormattableTextComponent removeLink = buildDimSuggestRegionRemovalLink(region);
                removeLink.append(separator).append(buildRegionInfoLink(region, LOCAL));
                IFormattableTextComponent childIndicator = buildTextWithHoverMsg(new StringTextComponent("*"), new TranslationTextComponent("cli.msg.info.dim.region.child.hover"), GOLD);
                if (parent.hasChild(region)) {
                    removeLink.append(childIndicator.setStyle(childIndicator.getStyle().withInsertion("Test")));
                }
                removeLink.append(new StringTextComponent(" @ ").setStyle(resetStyle)).append(buildRegionTeleportLink(region));
                regionRemoveLink = removeLink;
                break;
            }
            case LOCAL: {
                regionRemoveLink = buildRegionRemoveChildLink(parent, region).append(separator).append(buildRegionInfoLink(region, LOCAL));
                break;
            }
            default:
                throw new IllegalArgumentException();
        }
        return new StringTextComponent(" - ").append(regionRemoveLink);
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

        IFormattableTextComponent first = hasMultiplePages && pageNo != FIRST_PAGE_IDX
                ? buildExecuteCmdComponent(new TranslationTextComponent("cli.msg.info.pagination.first.text"), new TranslationTextComponent("cli.msg.info.pagination.first.hover"), buildPageCommand(cmd, FIRST_PAGE_IDX), RUN_COMMAND, LINK_COLOR)
                : TextComponentUtils.wrapInSquareBrackets(new TranslationTextComponent("cli.msg.info.pagination.first.text")).withStyle(INACTIVE_LINK_COLOR);
        IFormattableTextComponent prev = hasMultiplePages && pageNo > FIRST_PAGE_IDX
                ? buildExecuteCmdComponent(new TranslationTextComponent("cli.msg.info.pagination.previous.text"), new TranslationTextComponent("cli.msg.info.pagination.previous.hover"), buildPageCommand(cmd, Math.max(pageNo - 1, FIRST_PAGE_IDX)), RUN_COMMAND, LINK_COLOR)
                : TextComponentUtils.wrapInSquareBrackets(new TranslationTextComponent("cli.msg.info.pagination.previous.text")).withStyle(INACTIVE_LINK_COLOR);
        IFormattableTextComponent next = hasMultiplePages && pageNo < numberOfPages - 1
                ? buildExecuteCmdComponent(new TranslationTextComponent("cli.msg.info.pagination.next.text"), new TranslationTextComponent("cli.msg.info.pagination.next.hover"), buildPageCommand(cmd, Math.min(pageNo + 1, numberOfPages - 1)), RUN_COMMAND, LINK_COLOR)
                : TextComponentUtils.wrapInSquareBrackets(new TranslationTextComponent("cli.msg.info.pagination.next.text")).withStyle(INACTIVE_LINK_COLOR);
        IFormattableTextComponent last = hasMultiplePages && pageNo < numberOfPages - 1
                ? buildExecuteCmdComponent(new TranslationTextComponent("cli.msg.info.pagination.last.text"), new TranslationTextComponent("cli.msg.info.pagination.last.hover"), buildPageCommand(cmd, numberOfPages - 1), RUN_COMMAND, LINK_COLOR)
                : TextComponentUtils.wrapInSquareBrackets(new TranslationTextComponent("cli.msg.info.pagination.last.text")).withStyle(INACTIVE_LINK_COLOR);

        IFormattableTextComponent paginationControl = buildPaginationControl(first, prev, pageNo, numberOfPages, next, last);
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

    public static IFormattableTextComponent buildPaginationControl(IFormattableTextComponent front, IFormattableTextComponent back, int pageNo, int maxPage, IFormattableTextComponent forward, IFormattableTextComponent last) {
        // [<<]  [<]  x/n  [>]  [>>]
        IFormattableTextComponent pageIndicator = new StringTextComponent((pageNo + 1) + "/" + (maxPage));
        pageIndicator.setStyle(pageIndicator.getStyle().withColor(RESET).withHoverEvent(null).withClickEvent(null));
        IFormattableTextComponent resetSpace = new StringTextComponent("  ");
        resetSpace.setStyle(resetSpace.getStyle().withColor(RESET).withHoverEvent(null).withClickEvent(null));
        return new StringTextComponent(" ")
                .append(front).append(resetSpace)
                .append(back).append(resetSpace)
                .append(pageIndicator).append(resetSpace)
                .append(forward).append(resetSpace)
                .append(last).append(resetSpace);
    }

    public static IFormattableTextComponent buildRegionChildrenHeader(IProtectedRegion region, RegionType type) {
        return buildHeader(new TranslationTextComponent("cli.msg.info.header.in", buildRegionChildrenLink(region, type), buildRegionInfoLink(region, type)));
    }

    public static IFormattableTextComponent buildRegionParentLink(IMarkableRegion region) {
        IFormattableTextComponent parentLink = null;
        if (region.getParent() != null) { // FIXME: should not happen. it is either a dim or local region as parent
            String regionParentInfoCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getParent().getName(), INFO.toString());
            IFormattableTextComponent parentLinkText = new TranslationTextComponent("cli.msg.info.region.parent.link.text", region.getParent().getName());
            IFormattableTextComponent parentHoverText = new TranslationTextComponent("cli.msg.info.region.parent.link.hover", region.getParent().getName());
            if (region.getParent() instanceof DimensionalRegion) {
                return buildRegionInfoLink(region.getParent(), RegionType.DIMENSION);
            }
            if (region.getParent() instanceof IMarkableRegion) {
                String clearRegionParentCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), PARENT.toString(), CLEAR.toString(), region.getParent().getName());
                IFormattableTextComponent parentClearLinkText = new TranslationTextComponent("cli.msg.info.region.parent.clear.link.text");
                IFormattableTextComponent parentClearHoverText = new TranslationTextComponent("cli.msg.info.region.parent.clear.link.hover", region.getParent().getName());
                parentLink = buildExecuteCmdComponent(parentLinkText, parentHoverText, regionParentInfoCmd, RUN_COMMAND, LINK_COLOR)
                        .append(buildExecuteCmdComponent(parentClearLinkText, parentClearHoverText, clearRegionParentCmd, RUN_COMMAND, REMOVE_CMD_COLOR));
                return parentLink;
            }
            if (region.getParent() instanceof GlobalRegion) { // FIXME: Not needed here
                // TODO: Hierarchy Info for dimensional Regions
            }

        } else {
            String setRegionParentCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), PARENT.toString(), SET.toString(), "");
            IFormattableTextComponent setParentLinkText = new TranslationTextComponent("cli.link.add");
            IFormattableTextComponent setParentHoverText = new TranslationTextComponent("cli.msg.info.region.parent.set.link.hover", region.getName());
            parentLink = new TranslationTextComponent("cli.msg.info.region.parent.null")
                    .append(" ")
                    .append(buildExecuteCmdComponent(setParentLinkText, setParentHoverText, setRegionParentCmd, RUN_COMMAND, GREEN));
        }
        return parentLink;
    }

    public static IFormattableTextComponent buildDimRegionListHeader(DimensionalRegion dimRegion) {
        return buildHeader(new TranslationTextComponent("cli.msg.info.header.in",
                buildRegionChildrenLink(dimRegion, RegionType.DIMENSION), buildRegionInfoLink(dimRegion, RegionType.DIMENSION)));
    }


    // [n regions][+]
    public static IFormattableTextComponent buildDimRegionsLink(DimensionRegionCache dimCache) {
        DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
        String command = buildCommandStr(DIM.toString(), dimRegion.getDim().location().toString(), LIST.toString(), REGION.toString());
        IFormattableTextComponent listDimRegionsLinkText = new TranslationTextComponent("cli.msg.dim.info.region.list.link.text", dimCache.getRegions().size());
        IFormattableTextComponent listDimRegionsHoverText = new TranslationTextComponent("cli.msg.dim.info.region.list.link.hover", dimRegion.getName());
        IFormattableTextComponent listDimRegionsListLink = buildExecuteCmdComponent(listDimRegionsLinkText, listDimRegionsHoverText, command, RUN_COMMAND, LINK_COLOR);
        IFormattableTextComponent createRegionLink = buildDimCreateRegionLink(dimRegion);
        return (dimRegion.getChildren().size() == 0) ? listDimRegionsLinkText.append(createRegionLink) : listDimRegionsListLink.append(createRegionLink);
    }

    // [n children][+]
    public static IFormattableTextComponent buildRegionChildrenLink(IProtectedRegion region, RegionType type) {
        IFormattableTextComponent res;
        switch (type) {
            case GLOBAL: {
                Collection<String> dimensionList = RegionDataManager.get().getDimensionList();
                String command = buildCommandStr(GLOBAL.toString(), LIST.toString(), DIM.toString());
                IFormattableTextComponent listDimRegionsLinkText = new TranslationTextComponent("cli.msg.global.info.region.list.link.text", dimensionList.size());
                IFormattableTextComponent listDimRegionsHoverText = new TranslationTextComponent("cli.msg.global.info.region.list.link.hover", region.getName());
                return buildExecuteCmdComponent(listDimRegionsLinkText, listDimRegionsHoverText, command, RUN_COMMAND, LINK_COLOR);
            }
            case DIMENSION: {
                // TODO: children not regions
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(region.getDim());
                String command = buildCommandStr(DIM.toString(), region.getDim().location().toString(), LIST.toString(), REGION.toString());
                IFormattableTextComponent listDimRegionsLinkText = new TranslationTextComponent("cli.msg.dim.info.region.list.link.text", dimCache.getRegions().size());
                IFormattableTextComponent listDimRegionsHoverText = new TranslationTextComponent("cli.msg.dim.info.region.list.link.hover", region.getName());
                IFormattableTextComponent listDimRegionsListLink = buildExecuteCmdComponent(listDimRegionsLinkText, listDimRegionsHoverText, command, RUN_COMMAND, LINK_COLOR);
                IFormattableTextComponent createRegionLink = buildDimCreateRegionLink(region);
                res = (region.getChildren().size() == 0) ? listDimRegionsLinkText.append(createRegionLink) : listDimRegionsListLink.append(createRegionLink);
                break;
            }
            // [n children][+]
            case LOCAL: {
                String regionChildrenListLink = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), CHILDREN.toString());
                IFormattableTextComponent childrenLinkText = new TranslationTextComponent("cli.msg.info.region.children.link.text", region.getChildren().size());
                IFormattableTextComponent childrenHoverText = new TranslationTextComponent("cli.msg.info.region.children.link.hover", region.getName());
                IFormattableTextComponent regionChildrenLink = buildExecuteCmdComponent(childrenLinkText, childrenHoverText, regionChildrenListLink, RUN_COMMAND, LINK_COLOR);
                IFormattableTextComponent addChildrenLink = buildRegionAddChildrenLink(region);
                res = (region.getChildren().size() == 0) ? childrenLinkText.append(addChildrenLink) : regionChildrenLink.append(addChildrenLink);
                break;
            }
            default:
                throw new IllegalStateException("Unexpected value: " + type);
        }
        return res;
    }

    public static IFormattableTextComponent buildRegionAddChildrenLink(IProtectedRegion region) {
        String addChildrenCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), ADD.toString(), CHILD.toString(), "");
        IFormattableTextComponent addChildrenLinkText = new TranslationTextComponent("cli.link.add");
        IFormattableTextComponent addChildrenHoverText = new TranslationTextComponent("cli.msg.info.region.children.add.link.hover", region.getName());
        return buildExecuteCmdComponent(addChildrenLinkText, addChildrenHoverText, addChildrenCmd, SUGGEST_COMMAND, ADD_CMD_COLOR);
    }

    public static IFormattableTextComponent buildDimCreateRegionLink(IProtectedRegion region) {
        String dimCreateRegionCmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), CREATE.toString(), REGION.toString(), "");
        IFormattableTextComponent createRegionLinkText = new TranslationTextComponent("cli.link.add");
        IFormattableTextComponent createRegionHoverText = new TranslationTextComponent("cli.msg.dim.info.region.create.link.hover", region.getName());
        return buildExecuteCmdComponent(createRegionLinkText, createRegionHoverText, dimCreateRegionCmd, SUGGEST_COMMAND, ADD_CMD_COLOR);
    }

    public static IFormattableTextComponent buildFlagListLink(IProtectedRegion region, RegionType type) {
        IFormattableTextComponent flagLink;
        switch (type) {
            case GLOBAL: {
                String flagListCmd = buildCommandStr(GLOBAL.toString(), LIST.toString(), FLAG.toString());
                IFormattableTextComponent flagListLink = buildExecuteCmdComponent(flagListLinkText, flagListHoverText, flagListCmd, RUN_COMMAND, LINK_COLOR);
                if (region.getFlags().isEmpty()) {
                    flagListLink = flagListLinkText;
                }
                return flagListLink.append(" ").append(buildAddFlagLink(region, type));
            }
            case DIMENSION: {
                String flagListCmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), LIST.toString(), FLAG.toString());
                IFormattableTextComponent flagListLinkText = new TranslationTextComponent("cli.msg.info.region.flag.link.text", region.getFlags().size());
                IFormattableTextComponent flagListHoverText = new TranslationTextComponent("cli.msg.dim.flag.list.link.hover", region.getName());
                IFormattableTextComponent dimFlagListLink = buildExecuteCmdComponent(flagListLinkText, flagListHoverText, flagListCmd, RUN_COMMAND, LINK_COLOR);
                flagLink = dimFlagListLink.append(buildDimAddFlagLink(region));
                break;
            }
            case LOCAL: {
                String listCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), FLAG.toString());
                IFormattableTextComponent flagListLinkText = new TranslationTextComponent("cli.msg.info.region.flag.link.text", region.getFlags().size());
                IFormattableTextComponent flagListHoverText = new TranslationTextComponent("cli.msg.info.region.flag.link.hover", region.getName());
                IFormattableTextComponent flagListLink = buildExecuteCmdComponent(flagListLinkText, flagListHoverText, listCmd, RUN_COMMAND, LINK_COLOR);
                flagLink = flagListLink.append(buildRegionAddFlagLink(region));
                break;
            }
            default:
                throw new IllegalStateException("Unexpected value: " + type);
        }
        return flagLink;
    }

    public static IFormattableTextComponent buildDimAddFlagLink(IProtectedRegion dimRegion) {
        String command = buildCommandStr(DIM.toString(), dimRegion.getDim().location().toString(), ADD.toString(), FLAG.toString(), "");
        IFormattableTextComponent hoverText = new TranslationTextComponent("cli.msg.dim.flag.add.link.hover", dimRegion.getDim().location().toString());
        IFormattableTextComponent linkText = new TranslationTextComponent("cli.link.add");
        return buildExecuteCmdComponent(linkText, hoverText, command, SUGGEST_COMMAND, ADD_CMD_COLOR);
    }

    public static IFormattableTextComponent buildRegionAddFlagLink(IProtectedRegion region) {
        String addCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), ADD.toString(), FLAG.toString(), "");
        IFormattableTextComponent flagAddHoverText = new TranslationTextComponent("cli.msg.info.region.flag.add.link.hover", region.getName());
        IFormattableTextComponent flagAddLinkText = new TranslationTextComponent("cli.link.add");
        IFormattableTextComponent addFlag = buildExecuteCmdComponent(flagAddLinkText, flagAddHoverText, addCmd, SUGGEST_COMMAND, ADD_CMD_COLOR);
        return addFlag;
    }

    public static IFormattableTextComponent buildRegionStateLink(IMarkableRegion region) {
        String showStateCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), STATE.toString());
        IFormattableTextComponent stateLinkText = new TranslationTextComponent("cli.msg.info.region.state.link.text");
        IFormattableTextComponent stateHoverText = new TranslationTextComponent("cli.msg.info.region.state.link.hover", region.getName());
        return buildExecuteCmdComponent(stateLinkText, stateHoverText, showStateCmd, RUN_COMMAND, LINK_COLOR);
    }

    public static IFormattableTextComponent buildStateLink(IProtectedRegion region) {
        String command = CommandUtil.buildCommandStr(DIM.toString(), region.getDim().location().toString(), ENABLE.toString());
        String onClickAction = region.isActive() ? "deactivate" : "activate";
        String hoverText = "cli.msg.info.state." + onClickAction;
        String linkText = "cli.msg.info.state.link." + (region.isActive() ? "activate" : "deactivate");
        TextFormatting color = region.isActive() ? ADD_CMD_COLOR : REMOVE_CMD_COLOR;
        IFormattableTextComponent stateLink = buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, color);
        return new TranslationTextComponent("cli.msg.info.state")
                .append(new StringTextComponent(": "))
                .append(stateLink);
    }

    public static IFormattableTextComponent buildInfoComponent(String subjectLangKey, IFormattableTextComponent payload) {
        return new TranslationTextComponent(subjectLangKey).append(": ").append(payload);
    }

    public static IFormattableTextComponent buildRegionTeleportLink(IMarkableRegion region) {
        String teleportCmd = buildDimTeleportCmd(region.getDim(), "@s", region.getTpTarget());
        return buildExecuteCmdComponent(buildBlockPosTeleportLinkText(region.getTpTarget()),
                "cli.msg.region.info.tp.link.hover", teleportCmd, RUN_COMMAND, TP_COLOR);
    }

    public static IFormattableTextComponent buildDimensionalBlockTpLink(RegistryKey<World> dim, BlockPos target) {
        String teleportCmd = buildDimTeleportCmd(dim, "@s", target);
        return buildExecuteCmdComponent(buildBlockPosTeleportLinkText(target),
                "cli.msg.info.region.spatial.location.teleport.link.hover", teleportCmd, RUN_COMMAND, TP_COLOR);
    }

    public static IFormattableTextComponent buildDimSuggestRegionRemovalLink(IMarkableRegion region) {
        String cmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), DELETE.toString(), region.getName());
        IFormattableTextComponent hover = new TranslationTextComponent("cli.msg.info.dim.region.remove.link.hover", region.getName());
        IFormattableTextComponent text = new TranslationTextComponent("cli.link.remove");
        return buildExecuteCmdComponent(text, hover, cmd, SUGGEST_COMMAND, REMOVE_CMD_COLOR);
    }

    /* TODO: extract method for n component(s) [+] */
    public static IFormattableTextComponent buildDimFlagListLink(IProtectedRegion region) {
        String command = buildCommandStr(DIM.toString(), region.getDim().location().toString(), LIST.toString(), FLAG.toString());
        IFormattableTextComponent hoverLink = new TranslationTextComponent("cli.msg.dim.flag.list.link.hover", region.getDim().location().toString());
        IFormattableTextComponent linkText = new TranslationTextComponent("cli.msg.flag.list.link.text", region.getFlags().size());
        return region.getFlags().isEmpty()
                ? new TranslationTextComponent("cli.msg.info.region.flag.link.text", region.getFlags().size())
                : buildExecuteCmdComponent(linkText, hoverLink, command, RUN_COMMAND, LINK_COLOR);
    }

    public static List<IFormattableTextComponent> buildRemoveAffiliationEntries(IProtectedRegion region, List<String> affiliationNames, AffiliationType affiliationType, String affiliation, RegionType parentType) {
        return affiliationNames.stream().map(affiliate -> buildRemoveAffiliateEntry(region, affiliate, affiliationType, affiliation, parentType)).collect(Collectors.toList());
    }

    public static IFormattableTextComponent buildRemoveAffiliateEntry(IProtectedRegion region, String affiliateName, AffiliationType affiliationType, String affiliation, RegionType regionType) {
        IFormattableTextComponent linkText = new TranslationTextComponent("cli.link.remove");
        IFormattableTextComponent hoverText = new TranslationTextComponent("cli.msg.info.region.affiliation." + affiliationType.name + ".remove.link.hover", affiliateName, region.getName());
        IFormattableTextComponent regionRemoveLink;
        switch (regionType) {
            case GLOBAL: {
                String command = buildCommandStr(GLOBAL.toString(), REMOVE.toString(), groupType.name, affiliation, affiliateName);
                regionRemoveLink = buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, REMOVE_CMD_COLOR);
                break;
            }
            case DIMENSION: {
                String command = buildCommandStr(DIM.toString(), region.getDim().location().toString(), REMOVE.toString(), affiliationType.name, affiliation, affiliateName);
                regionRemoveLink = buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, REMOVE_CMD_COLOR);
                break;
            }
            case LOCAL: {
                String command = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), REMOVE.toString(), affiliationType.name, affiliation, affiliateName);
                regionRemoveLink = buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, REMOVE_CMD_COLOR);
                break;
            }
            default:
                throw new IllegalArgumentException();
        }
        return new StringTextComponent(" - ")
                .append(regionRemoveLink).append(" ")
                .append(buildAffiliateInfo(region, affiliateName, affiliationType));
    }

    public static IFormattableTextComponent buildAffiliateInfo(IProtectedRegion region, String affiliateName, AffiliationType affiliationType) {
        IFormattableTextComponent res;
        switch (affiliationType) {
            case PLAYER: {
                PlayerEntity player = ServerLifecycleHooks.getCurrentServer().getPlayerList().getPlayerByName(affiliateName);
                res = player == null
                        ? new StringTextComponent(affiliateName).withStyle(GRAY).append(" ").append(new TranslationTextComponent("cli.msg.info.player.list.entry.offline"))
                        : buildPlayerHoverComponent(player);
                break;
            }
            case TEAM: {
                ServerWorld level = ServerLifecycleHooks.getCurrentServer().getLevel(region.getDim());
                if (level != null) {
                    Team team = level.getScoreboard().getPlayerTeam(affiliateName);
                    res = team == null ? new StringTextComponent(affiliateName) : buildTeamHoverComponent(team);
                } else {
                    res = new StringTextComponent(affiliateName);
                }
                break;
            }
            default:
                throw new IllegalStateException("Unexpected value: " + affiliationType);
        }
        return res;
    }

    public static List<String> getAffiliateList(IProtectedRegion region, String affiliation, AffiliationType affiliationType) {
        List<String> associateNames = new ArrayList<>();
        switch (affiliation) {
            case "owner":
                switch (affiliationType) {
                    case PLAYER:
                        associateNames = region.getOwners().getPlayers().values().stream().sorted().collect(Collectors.toList());
                        break;
                    case TEAM:
                        associateNames = region.getOwners().getTeams().stream().sorted().collect(Collectors.toList());
                        break;
                }
                break;
            case "member":
                switch (affiliationType) {
                    case PLAYER:
                        associateNames = region.getMembers().getPlayers().values().stream().sorted().collect(Collectors.toList());
                        break;
                    case TEAM:
                        associateNames = region.getMembers().getTeams().stream().sorted().collect(Collectors.toList());
                        break;
                }
                break;
            default:
                break;
        }
        return associateNames;
    }


    public static IFormattableTextComponent buildRegionRemoveChildLink(IProtectedRegion region, IProtectedRegion child) {
        String command = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), REMOVE.toString(), CHILD.toString(), child.getName());
        IFormattableTextComponent linkText = new TranslationTextComponent("cli.link.remove");
        IFormattableTextComponent linkHoverText = new TranslationTextComponent("cli.msg.info.region.children.remove.link.hover", child.getName(), region.getName());
        return buildExecuteCmdComponent(linkText, linkHoverText, command, RUN_COMMAND, REMOVE_CMD_COLOR);
    }

    public static IFormattableTextComponent buildRegionActionUndoLink(String cmd, CommandConstants toReplace, CommandConstants replacement) {
        String revertCmd = CommandUtil.revertCommand(cmd, toReplace, replacement);
        IFormattableTextComponent revertLinkText = new TranslationTextComponent("cli.link.action.undo.text");
        IFormattableTextComponent revertLinkHover = new TranslationTextComponent("cli.link.action.undo.hover");
        return buildExecuteCmdComponent(revertLinkText, revertLinkHover, revertCmd, RUN_COMMAND, DARK_RED);
    }

    public static IFormattableTextComponent buildRegionActionUndoLink(String cmd, String toReplace, String replacement) {
        String revertCmd = CommandUtil.revertCommand(cmd, toReplace, replacement);
        IFormattableTextComponent revertLinkText = new TranslationTextComponent("cli.link.action.undo.text");
        IFormattableTextComponent revertLinkHover = new TranslationTextComponent("cli.link.action.undo.hover");
        return buildExecuteCmdComponent(revertLinkText, revertLinkHover, revertCmd, RUN_COMMAND, DARK_RED);
    }

    public static IFormattableTextComponent buildRegionFlagInfoHeader(IProtectedRegion region, RegionType regionType) {
        IFormattableTextComponent res;
        switch (regionType) {
            case GLOBAL:
                res = buildHeader(new TranslationTextComponent("cli.msg.info.header.in", buildFlagListLink(region, regionType), buildRegionInfoLink(region, regionType)));
                break;
            case DIMENSION:
                res = buildHeader(new TranslationTextComponent("cli.msg.info.header.in", buildFlagListLink(region, regionType), buildRegionInfoLink(region, regionType)));
                break;
            case LOCAL:
                res = buildHeader(new TranslationTextComponent("cli.msg.info.header.in", buildFlagListLink(region, regionType), buildRegionInfoLink(region, regionType)));
                break;
            default:
                throw new IllegalStateException("Unexpected value: " + regionType);
        }
        return res;
    }

    public static IFormattableTextComponent buildFlagInfoHeader(IProtectedRegion region, IFlag flag, RegionType regionType) {
        IFormattableTextComponent res;
        switch (regionType) {
            case DIMENSION:
                res = buildHeader(new TranslationTextComponent("cli.msg.info.header.flag.in", buildFlagInfoLink(region, flag, regionType), buildRegionInfoLink(region, regionType)));
                break;
            case LOCAL:
                res = buildHeader(new TranslationTextComponent("cli.msg.info.header.flag.in", buildFlagInfoLink(region, flag, regionType), buildRegionInfoLink(region, regionType)));
                break;
            case GLOBAL:
                res = buildHeader(new TranslationTextComponent("cli.msg.info.header.flag.in", buildFlagInfoLink(region, flag, regionType), buildRegionInfoLink(region, regionType)));
                break;
            default:
                throw new IllegalStateException("Unexpected value: " + regionType);
        }
        return res;
    }
}
