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
import de.z0rdak.yawp.core.flag.BooleanFlag;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.group.GroupType;
import de.z0rdak.yawp.core.group.PlayerContainer;
import de.z0rdak.yawp.core.region.*;
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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.commands.arguments.ArgumentUtil.*;
import static de.z0rdak.yawp.core.region.RegionType.LOCAL;
import static net.minecraft.ChatFormatting.RESET;
import static net.minecraft.ChatFormatting.*;
import static net.minecraft.network.chat.ClickEvent.Action.RUN_COMMAND;
import static net.minecraft.network.chat.ClickEvent.Action.SUGGEST_COMMAND;


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
        return new TextComponent(String.valueOf(BOLD))
                .append(header)
                .append(new TextComponent(String.valueOf(BOLD)));
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
    public static MutableComponent buildBlockPosTpLinks(IMarkableRegion region) {
        List<MutableComponent> tpLinks = region.getArea().getMarkedBlocks()
                .stream()
                .map(pos -> buildDimensionalBlockTpLink(region.getDim(), pos))
                .collect(Collectors.toList());
        MutableComponent blockPosTpLinkList = new TextComponent("");
        tpLinks.forEach(tpLink -> blockPosTpLinkList.append(tpLink).append(" "));
        return blockPosTpLinkList;
    }

    /**
     * Location: [dimInfo] @ [tpCoordinates]
     */
    public static MutableComponent buildDimensionTeleportLink(IMarkableRegion region) {
        String executeCmdStr = buildDimTeleportCmd(region.getDim(), "@s", region.getTpTarget());
        MutableComponent cmdLinkText = new TextComponent(buildTeleportLinkText(region.getDim(), region.getTpTarget()));
        MutableComponent teleportCmdHoverText = new TranslatableComponent("cli.msg.info.region.area.location.teleport", region.getName(), region.getDim().location().toString());
        return buildExecuteCmdComponent(cmdLinkText, teleportCmdHoverText, executeCmdStr, RUN_COMMAND, TP_COLOR);
    }

    /**
     * Area: Cuboid, Size: X=69, Y=10, Z=42 [<=expand=>] [<=max=>]
     */
    public static MutableComponent buildRegionAreaDetailComponent(IMarkableRegion region) {
        IMarkableArea area = region.getArea();
        MutableComponent areaInfo = new TextComponent("(" + area.getAreaType().areaType + ")");
        switch (area.getAreaType()) {
            case CUBOID: {
                CuboidArea cuboidArea = (CuboidArea) area;
                MutableComponent sizeInfo = buildAreaAxisInfoComponent(cuboidArea, Direction.Axis.X).append(", ")
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
    private static MutableComponent buildAreaAxisInfoComponent(CuboidArea cuboidArea, Direction.Axis axis) {
        int min = (int) Math.floor(cuboidArea.getArea().min(axis));
        int max = (int) Math.floor(cuboidArea.getArea().max(axis));
        String axisName = axis.getName().toUpperCase();
        return buildTextWithHoverMsg(
                new TextComponent(axisName + "=" + Math.abs(max - min)),
                new TextComponent(axisName + ": " + min + " - " + max), WHITE);
    }

    /**
     * [<=expand=>] [<=max=>]
     */
    public static MutableComponent buildRegionAreaExpandLink(IMarkableRegion region) {
        int minBlockHeight = 0;
        int maxBlockHeight = 255;
        MutableComponent linkText = new TranslatableComponent("cli.msg.info.region.area.area.expand.link.text");
        MutableComponent linkHover = new TranslatableComponent("cli.msg.info.region.area.area.expand.link.hover");
        String expandCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), AREA.toString(), EXPAND.toString());
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
                String maxExpandCmd = appendSubCommand(expandCmd, String.valueOf(minBlockHeight), String.valueOf(maxBlockHeight));
                MutableComponent maxExpandLink = buildExecuteCmdComponent(maxExpandLinkText, maxExpandLinkHover, maxExpandCmd, RUN_COMMAND, LINK_COLOR);
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
    public static MutableComponent buildRegionAreaMarkingComponent(IMarkableRegion region) {
        switch (region.getArea().getAreaType()) {
            case CUBOID: {
                CuboidArea cuboidArea = (CuboidArea) region.getArea();
                String areaCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), AREA.toString());
                MutableComponent setAreaLinkText = new TranslatableComponent("cli.msg.info.region.area.area.set.link");
                MutableComponent setAreaLinkHover = new TranslatableComponent("cli.msg.info.region.area.area.set.hover", region.getName());
                String blocks = String.join(" ", cuboidArea.getMarkedBlocks().stream()
                        .map(MessageUtil::buildBlockCoordinateStr)
                        .collect(Collectors.toSet()));
                String setArea = appendSubCommand(areaCmd, SET.toString(), region.getArea().getAreaType().areaType, blocks);
                MutableComponent setAreaLink = buildExecuteCmdComponent(setAreaLinkText, setAreaLinkHover, setArea, SUGGEST_COMMAND, LINK_COLOR);

                MutableComponent showAreaLinkText = new TranslatableComponent("cli.msg.info.region.area.area.show.link");
                MutableComponent showAreaLinkHover = new TranslatableComponent("cli.msg.info.region.area.area.show.hover", region.getName());
                String showArea = appendSubCommand(areaCmd, "show");
                MutableComponent showAreaLink = buildExecuteCmdComponent(showAreaLinkText, showAreaLinkHover, showArea, RUN_COMMAND, LINK_COLOR);
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
    public static MutableComponent buildRegionAreaTpComponent(IMarkableRegion region) {
        MutableComponent regionTpLink = buildRegionTeleportLink(region);
        MutableComponent setTpLink = buildRegionSetTpLink(region);
        return regionTpLink.append(" ").append(setTpLink);
    }

    public static MutableComponent buildTextWithHoverMsg(MutableComponent text, MutableComponent hoverText, ChatFormatting color) {
        MutableComponent bracketedText = ComponentUtils.wrapInSquareBrackets(text);
        bracketedText.setStyle(bracketedText.getStyle().withColor(color).withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, hoverText)));
        return bracketedText;
    }

    public static MutableComponent buildHelpSuggestionLink(String translationKey, CommandConstants baseCmd, CommandConstants cmd) {
        String command = "/" + CommandPermissionConfig.BASE_CMD + " " + baseCmd + " " + cmd + " ";
        return new TextComponent(" ")
                .append(buildExecuteCmdComponent("=>", "chat.link.hover.command.copy", command, SUGGEST_COMMAND, SUGGEST_COLOR))
                .append(new TextComponent(" "))
                .append(new TranslatableComponent(translationKey));
    }

    public static MutableComponent buildRegionEnableComponent(IProtectedRegion region, RegionType type) {
        String linkTextKey = "cli.msg.info.region.state.enable." + region.isActive() + ".link.text";
        String hoverTextKey = "cli.msg.info.region.state.enable." + !region.isActive() + ".link.hover";
        ChatFormatting color = region.isActive() ? ADD_CMD_COLOR : REMOVE_CMD_COLOR;
        return switch (type) {
            case GLOBAL -> {
                String cmd = ArgumentUtil.buildCommandStr(GLOBAL.toString(), STATE.toString(), ENABLE.toString());
                yield buildExecuteCmdComponent(linkTextKey, hoverTextKey, cmd, ClickEvent.Action.RUN_COMMAND, color);
            }
            case DIMENSION -> {
                String cmd = ArgumentUtil.buildCommandStr(DIM.toString(), region.getDim().location().toString(), STATE.toString(), ENABLE.toString());
                yield buildExecuteCmdComponent(linkTextKey, hoverTextKey, cmd, RUN_COMMAND, color);
            }
            case LOCAL -> {
                String cmd = ArgumentUtil.buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), STATE.toString(), ENABLE.toString());
                yield buildExecuteCmdComponent(linkTextKey, hoverTextKey, cmd, RUN_COMMAND, color);
            }
            default ->
                throw new IllegalStateException("Unexpected value: " + type);
        };
    }

    public static MutableComponent buildRegionPriorityComponent(IMarkableRegion region) {
        int defaultPriorityInc = RegionConfig.getDefaultPriorityInc();
        String incPriorityCmd = ArgumentUtil.buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), STATE.toString(), PRIORITY.toString(), INC.toString(), String.valueOf(defaultPriorityInc));
        MutableComponent incLinkText = new TranslatableComponent("cli.msg.info.region.state.priority.increase.link.text", defaultPriorityInc);
        MutableComponent incHoverText = new TranslatableComponent("cli.msg.info.region.state.priority.increase.link.hover", defaultPriorityInc);
        MutableComponent increaseLink = buildExecuteCmdComponent(incLinkText, incHoverText, incPriorityCmd, ClickEvent.Action.RUN_COMMAND, ADD_CMD_COLOR);
        String decPriorityCmd = ArgumentUtil.buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), STATE.toString(), PRIORITY.toString(), DEC.toString(), String.valueOf(defaultPriorityInc));
        MutableComponent decLinkText = new TranslatableComponent("cli.msg.info.region.state.priority.decrease.link.text", defaultPriorityInc);
        MutableComponent decHoverText = new TranslatableComponent("cli.msg.info.region.state.priority.decrease.link.hover", defaultPriorityInc);
        MutableComponent decreaseLink = buildExecuteCmdComponent(decLinkText, decHoverText, decPriorityCmd, ClickEvent.Action.RUN_COMMAND, REMOVE_CMD_COLOR);
        MutableComponent priorityValue = new TextComponent(String.valueOf(region.getPriority()));
        String setPriorityCmd = ArgumentUtil.buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), STATE.toString(), PRIORITY.toString(), "");
        MutableComponent setPriorityLinkText = new TranslatableComponent("cli.msg.info.region.state.priority.set.link.text");
        MutableComponent setPriorityHoverText = new TranslatableComponent("cli.msg.info.region.state.priority.set.link.hover");
        MutableComponent setPriorityLink = buildExecuteCmdComponent(setPriorityLinkText, setPriorityHoverText, setPriorityCmd, SUGGEST_COMMAND, SUGGEST_COLOR);
        return priorityValue.append(" ")
                .append(setPriorityLink).append(" ")
                .append(increaseLink).append(" ")
                .append(decreaseLink);
    }

    public static MutableComponent buildRegionAlertToggleLink(IProtectedRegion region, RegionType type) {
        String linkTextKey = "cli.msg.info.region.state.alert." + !region.isMuted() + ".link.text";
        String hoverTextKey = "cli.msg.info.region.state.alert." + region.isMuted() + ".link.hover";
        ChatFormatting color = region.isMuted() ? REMOVE_CMD_COLOR : ADD_CMD_COLOR;
        return switch (type) {
            case GLOBAL -> {
                String cmd = ArgumentUtil.buildCommandStr(GLOBAL.toString(), MSG.toString(), MUTE.toString());
                yield buildExecuteCmdComponent(linkTextKey, hoverTextKey, cmd, ClickEvent.Action.RUN_COMMAND, color);
            }
            case DIMENSION -> {
                String cmd = ArgumentUtil.buildCommandStr(DIM.toString(), region.getDim().location().toString(), STATE.toString(), MSG.toString(), MUTE.toString());
                yield buildExecuteCmdComponent(linkTextKey, hoverTextKey, cmd, RUN_COMMAND, color);
            }
            case LOCAL -> {
                String cmd = ArgumentUtil.buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), STATE.toString(), MSG.toString(), MUTE.toString());
                yield buildExecuteCmdComponent(linkTextKey, hoverTextKey, cmd, RUN_COMMAND, color);
            }
            default ->
                throw new IllegalStateException("Unexpected value: " + type);
        };
    }

    public static MutableComponent buildRegionInfoLink(IProtectedRegion region, RegionType type) {
        return buildRegionInfoLink(region, type, new TranslatableComponent("cli.msg.info.region.link.hover", region.getName()));
    }

    public static MutableComponent buildRegionInfoLink(IProtectedRegion region, RegionType type, MutableComponent hoverText) {
        MutableComponent linkText = new TextComponent(region.getName());
        switch (type) {
            case GLOBAL -> {
                String command = buildCommandStr(GLOBAL.toString(), INFO.toString());
                return buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, LINK_COLOR);
            }
            case DIMENSION -> {
                String command = buildCommandStr(DIM.toString(), region.getDim().location().toString(), INFO.toString());
                return buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, LINK_COLOR);
            }
            case LOCAL -> {
                String cmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), INFO.toString());
                return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            default:
                throw new IllegalStateException("Unexpected value: " + type);
        }
    }

    public static MutableComponent buildRegionAreaLink(IMarkableRegion region) {
        String showSpatialPropLink = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), AREA.toString());
        MutableComponent spatialPropLinkText = new TranslatableComponent("cli.msg.info.region.area.link.text");
        MutableComponent spatialPropHoverText = new TranslatableComponent("cli.msg.info.region.area.link.hover", region.getName());
        return buildExecuteCmdComponent(spatialPropLinkText, spatialPropHoverText, showSpatialPropLink, RUN_COMMAND, LINK_COLOR);
    }

    public static MutableComponent buildRegionOverviewHeader(IProtectedRegion region, RegionType type) {
        switch (type) {
            case GLOBAL: {
                MutableComponent clipBoardDumpLink = buildExecuteCmdComponent("cli.msg.global.overview.header.dump.link.text", "cli.msg.global.overview.header.dump.link.hover", region.serializeNBT().getPrettyDisplay().getString(), ClickEvent.Action.COPY_TO_CLIPBOARD, GOLD);
                return buildHeader(new TranslatableComponent("cli.msg.info.header.for", clipBoardDumpLink, buildRegionInfoLink(region, RegionType.GLOBAL)));
            }
            case DIMENSION: {
                MutableComponent clipBoardDumpLink = buildExecuteCmdComponent("cli.msg.dim.overview.header.dump.link.text", "cli.msg.dim.overview.header.dump.link.hover", region.serializeNBT().getPrettyDisplay().getString(), ClickEvent.Action.COPY_TO_CLIPBOARD, GOLD);
                return buildHeader(new TranslatableComponent("cli.msg.info.header.for", clipBoardDumpLink, buildRegionInfoLink(region, RegionType.DIMENSION)));
            }
            case LOCAL: {
                MutableComponent clipBoardDumpLink = buildExecuteCmdComponent("cli.msg.local.overview.header.dump.link.text", "cli.msg.local.overview.header.dump.link.hover", region.serializeNBT().getPrettyDisplay().getString(), ClickEvent.Action.COPY_TO_CLIPBOARD, GOLD);
                return buildHeader(new TranslatableComponent("cli.msg.info.header.for", clipBoardDumpLink, buildRegionInfoLink(region, LOCAL)));
            }
            default:
                throw new IllegalStateException("Unexpected value: " + type);
        }
    }

    /**
     * Players: [n player(s)] [+]
     * // TODO:
     */
    public static MutableComponent buildPlayerListLink(IProtectedRegion region, PlayerContainer players, String group, RegionType regionType) {
        MutableComponent hoverText = new TranslatableComponent("cli.msg.info.region.group.player.list.link.hover", group, region.getName());
        MutableComponent linkText = new TranslatableComponent("cli.msg.info.region.group.player.list.link.text", players.getPlayers().size());
        return switch (regionType) {
            case GLOBAL -> {
                String cmd = buildCommandStr(GLOBAL.toString(), LIST.toString(), group, PLAYER.toString());
                yield buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            case DIMENSION -> {
                String cmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), LIST.toString(), group, PLAYER.toString());
                yield buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            case LOCAL -> {
                String cmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), group, PLAYER.toString());
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
    // TODO: Link building could be generalized if the command structure would be always the same except the region specific parts
    public static MutableComponent buildTeamListLink(IProtectedRegion region, PlayerContainer teams, String group, RegionType regionType) {
        MutableComponent hoverText = new TranslatableComponent("cli.msg.info.region.group.team.list.link.hover", group, region.getName());
        MutableComponent linkText = new TranslatableComponent("cli.msg.info.region.group.team.list.link.text", teams.getTeams().size());
        switch (regionType) {
            case GLOBAL: {
                String cmd = buildCommandStr(GLOBAL.toString(), LIST.toString(), GROUP.toString(), group, TEAM.toString());
                return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            case DIMENSION: {
                String cmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), LIST.toString(), GROUP.toString(), group, TEAM.toString());
                return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            case LOCAL: {
                String cmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), GROUP.toString(), group, TEAM.toString());
                return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            default:
                throw new IllegalArgumentException();
        }
    }

    public static MutableComponent buildAddToGroupLink(IProtectedRegion region, String group, GroupType groupType, RegionType regionType) {
        MutableComponent linkText = new TranslatableComponent("cli.link.add");
        MutableComponent hoverText = new TranslatableComponent("cli.msg.info.region.group." + groupType.name + ".add.link.hover", group, region.getName());
        String subCmd = buildSubCmdStr(ADD.toString(), GROUP.toString(), groupType.name, group, "");
        ;
        switch (regionType) {
            case GLOBAL: {
                String cmd = buildCommandStr(GLOBAL.toString()) + " " + subCmd;
                return buildExecuteCmdComponent(linkText, hoverText, cmd, SUGGEST_COMMAND, ADD_CMD_COLOR);
            }
            case LOCAL: {
                String cmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName()) + " " + subCmd;
                return buildExecuteCmdComponent(linkText, hoverText, cmd, SUGGEST_COMMAND, ADD_CMD_COLOR);
            }
            case DIMENSION: {
                String cmd = buildCommandStr(DIM.toString(), region.getDim().location().toString()) + " " + subCmd;
                return buildExecuteCmdComponent(linkText, hoverText, cmd, SUGGEST_COMMAND, ADD_CMD_COLOR);
            }
            default:
                throw new IllegalArgumentException();
        }
    }

    public static MutableComponent buildGroupLinks(IProtectedRegion region, RegionType regionType) {
        return getGroupsForRegion(region, regionType).stream()
                .map(group -> buildGroupLink(region, group, getGroupSize(region, group), regionType))
                .reduce(new TextComponent(""), (link1, link2) -> link1.append(" ").append(link2));
    }

    public static List<String> getGroupsForRegion(IProtectedRegion region, RegionType regionType) {
        return RegionCommands.GROUP_LIST;
    }

    private static int getGroupSize(IProtectedRegion region, String groupName) {
        PlayerContainer group = region.getGroup(groupName);
        return group.getPlayers().size() + group.getTeams().size();
    }

    public static MutableComponent buildGroupHeader(IProtectedRegion region, String group, RegionType regionType) {
        MutableComponent groupLink = buildGroupLink(region, group, getGroupSize(region, group), regionType);
        return buildHeader(new TranslatableComponent("cli.msg.info.header.in", groupLink, buildRegionInfoLink(region, regionType)));
    }

    public static MutableComponent buildGroupHeader(IProtectedRegion region, String affiliation, GroupType groupType, RegionType regionType) {
        return new TranslatableComponent("cli.msg.info.region.group." + groupType.name + ".list", buildRegionInfoLink(region, regionType), affiliation);
    }

    public static MutableComponent buildGroupLink(IProtectedRegion region, String group, int groupSie, RegionType regionType) {
        MutableComponent linkText = new TranslatableComponent("cli.msg.info.region.group.list.link.text", groupSie, group);
        MutableComponent hoverText = new TranslatableComponent("cli.msg.info.region.group.list.link.hover", group, region.getName());
        switch (regionType) {
            case GLOBAL: {
                String cmd = buildCommandStr(GLOBAL.toString(), LIST.toString(), group);
                return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            case LOCAL: {
                String cmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), group);
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

    public static MutableComponent buildGroupTeamListLink(IProtectedRegion region, String group, RegionType regionType) {
        // Teams: [n team(s)] [+]
        PlayerContainer playerContainer = region.getGroup(group);
        MutableComponent teams = new TranslatableComponent("cli.msg.info.region.group.team").append(": ");
        MutableComponent teamAddLink = buildAddToGroupLink(region, group, GroupType.TEAM, regionType);
        MutableComponent teamListLink = playerContainer.hasTeams()
                ? buildTeamListLink(region, playerContainer, group, regionType)
                : new TranslatableComponent("cli.msg.info.region.group.team.list.link.text", playerContainer.getTeams().size());
        teams.append(teamListLink).append(teamAddLink);
        return teams;
    }

    public static MutableComponent buildGroupPlayerListLink(IProtectedRegion region, String group, RegionType regionType) {
        // Players: [n player(s)] [+]
        PlayerContainer playerContainer = region.getGroup(group);
        MutableComponent players = new TranslatableComponent("cli.msg.info.region.group.player").append(": ");
        MutableComponent playersAddLink = buildAddToGroupLink(region, group, GroupType.PLAYER, regionType);
        MutableComponent playerListLink = playerContainer.hasPlayers()
                ? buildPlayerListLink(region, playerContainer, group, regionType)
                : new TranslatableComponent("cli.msg.info.region.group.player.list.link.text", playerContainer.getPlayers().size());
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
    private static MutableComponent buildRemoveFlagEntry(IProtectedRegion region, IFlag flag, RegionType regionType) {
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
        MutableComponent hoverText = new TranslatableComponent("cli.msg.info.region.flag.remove.link.hover", flag.getName(), region.getName());
        MutableComponent linkText = new TranslatableComponent("cli.link.remove");
        MutableComponent flagRemoveLink = buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, REMOVE_CMD_COLOR);
        return new TextComponent(" - ")
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
    private static MutableComponent buildFlagQuickActionComponent(IProtectedRegion region, IFlag flag, RegionType regionType, String cmd) {
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
    public static MutableComponent buildFlagInfoLink(IProtectedRegion region, IFlag flag, RegionType regionType) {
        MutableComponent text = new TextComponent(flag.getName());
        MutableComponent hoverText = new TranslatableComponent("cli.flag.info.hover", flag.getName(), region.getName());
        switch (regionType) {
            case GLOBAL: {
                String cmd = buildCommandStr(FLAG.toString(), GLOBAL.toString(), flag.getName(), INFO.toString());
                return buildExecuteCmdComponent(text, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
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
    public static MutableComponent buildFlagInfoComponent(IProtectedRegion region, IFlag flag, RegionType regionType) {
        MutableComponent header = buildFlagInfoHeader(region, flag, regionType);
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

    public static MutableComponent buildFlagActiveToggleLink(IProtectedRegion region, RegionType regionType, IFlag flag) {
        switch (regionType) {
            case GLOBAL: {
                String cmd = buildCommandStr(FLAG.toString(), GLOBAL.toString(), flag.getName());
                return buildFlagToggleLink(cmd, "enable", flag.isActive(), ENABLE.toString());
            }
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

    public static MutableComponent buildFlagInvertToggleLink(IProtectedRegion region, RegionType regionType, IFlag flag) {
        switch (regionType) {
            case GLOBAL: {
                String cmd = buildCommandStr(FLAG.toString(), GLOBAL.toString(), flag.getName());
                return buildFlagToggleLink(cmd, "override", flag.doesOverride(), OVERRIDE.toString());
            }
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

    public static MutableComponent buildFlagMuteToggleLink(IProtectedRegion region, RegionType regionType, IFlag flag) {
        switch (regionType) {
            case GLOBAL: {
                String cmd = buildCommandStr(FLAG.toString(), GLOBAL.toString(), flag.getName());
                return buildFlagToggleLink(cmd, "msg.mute", !flag.getFlagMsg().isMuted(), MSG.toString(), MUTE.toString());
            }
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

    public static MutableComponent buildFlagMessageEditLink(IProtectedRegion region, RegionType regionType, IFlag flag) {
        // TODO: consider player flag specific messages
        // TODO: build clear link when not default and use it
        // TODO: use different text than quick action text
        boolean hasDefaultMsg = flag.getFlagMsg().isDefault();
        MutableComponent flagMsgText = new TranslatableComponent("cli.info.flag.state.msg.text.link.text", flag.getFlagMsg().getMsg());
        MutableComponent hoverText = new TranslatableComponent("cli.info.flag.state.msg.text.link.hover",
                buildFlagInfoLink(region, flag, regionType), buildRegionInfoLink(region, regionType));
        switch (regionType) {
            case GLOBAL: {
                String cmd = buildCommandStr(FLAG.toString(), GLOBAL.toString(), flag.getName(), flag.getFlagMsg().getMsg());
                if (hasDefaultMsg) {
                    flagMsgText = new TextComponent(FlagConfig.getRawGlobalFlagMsg());
                }
                return buildExecuteCmdComponent(flagMsgText, hoverText, cmd, SUGGEST_COMMAND, LINK_COLOR);
            }
            case DIMENSION: {
                String cmd = buildCommandStr(FLAG.toString(), DIM.toString(), region.getDim().location().toString(), flag.getName(), flag.getFlagMsg().getMsg());
                if (hasDefaultMsg) {
                    flagMsgText = new TextComponent(FlagConfig.getRawDimFlagMsg());
                }
                return buildExecuteCmdComponent(flagMsgText, hoverText, cmd, SUGGEST_COMMAND, LINK_COLOR);
            }
            case LOCAL: {
                String cmd = buildCommandStr(FLAG.toString(), LOCAL.toString(), region.getDim().location().toString(), region.getName(), flag.getName(), flag.getFlagMsg().getMsg());
                if (hasDefaultMsg) {
                    flagMsgText = new TextComponent(FlagConfig.getRawLocalFlagMsg());
                }
                return buildExecuteCmdComponent(flagMsgText, hoverText, cmd, SUGGEST_COMMAND, LINK_COLOR);
            }
            default:
                throw new IllegalStateException("Unexpected value: " + regionType);
        }
    }

    private static MutableComponent buildFlagToggleLink(String cmd, String langKey, boolean toggleActiveState, String... subCmds) {
        String cmdStr = appendSubCommand(cmd, subCmds);
        MutableComponent linkText = new TranslatableComponent("cli.flag." + langKey + ".link.text");
        MutableComponent hoverText = new TranslatableComponent("cli.flag." + langKey + ".link.hover");
        ChatFormatting color = toggleActiveState ? ADD_CMD_COLOR : REMOVE_CMD_COLOR;
        return buildExecuteCmdComponent(linkText, hoverText, cmdStr, RUN_COMMAND, color);
    }

    public static List<MutableComponent> buildRemoveFlagEntries(IProtectedRegion region, List<IFlag> flags, RegionType regionType) {
        return flags.stream().map(flag -> buildRemoveFlagEntry(region, flag, regionType)).collect(Collectors.toList());
    }

    public static List<MutableComponent> buildResetDimensionalRegionEntries(IProtectedRegion parent, List<DimensionRegionCache> regions, RegionType parentType) {
        return regions.stream().map(region -> buildRemoveRegionEntry(parent, region.getDimensionalRegion(), parentType)).collect(Collectors.toList());
    }

    public static List<MutableComponent> buildRemoveRegionEntries(IProtectedRegion parent, List<IMarkableRegion> regions, RegionType parentType) {
        return regions.stream().map(region -> buildRemoveRegionEntry(parent, region, parentType)).collect(Collectors.toList());
    }

    public static MutableComponent buildRemoveRegionEntry(IProtectedRegion parent, IProtectedRegion region, RegionType parentType) {
        Style resetStyle = Style.EMPTY.withColor(WHITE).withHoverEvent(null).withClickEvent(null);
        MutableComponent separator = new TextComponent(" ").setStyle(resetStyle);
        MutableComponent regionRemoveLink = switch (parentType) {
            case GLOBAL -> {
                YetAnotherWorldProtector.LOGGER.info("reseting global region - just kidding its not implemented yet");
                yield new TextComponent("");
                //throw new NotImplementedException("todo");
            }
            case DIMENSION -> {
                MutableComponent removeLink = buildDimSuggestRegionRemovalLink((IMarkableRegion) region);
                removeLink.append(separator).append(buildRegionInfoLink(region, LOCAL));
                MutableComponent childIndicator = buildTextWithHoverMsg(new TextComponent("*"), new TranslatableComponent("cli.msg.info.dim.region.child.hover"), GOLD);
                if (parent.hasChild(region)) {
                    removeLink.append(childIndicator.setStyle(childIndicator.getStyle().withInsertion("Test")));
                }
                removeLink.append(new TextComponent(" @ ").setStyle(resetStyle)).append(buildRegionTeleportLink((IMarkableRegion) region));
                yield removeLink;
            }
            case LOCAL ->
                    buildRegionRemoveChildLink(parent, region).append(separator).append(buildRegionInfoLink(region, LOCAL));
            default -> throw new IllegalArgumentException();
        };
        return new TextComponent(" - ").append(regionRemoveLink);
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
        pageIndicator.setStyle(pageIndicator.getStyle().withColor(RESET).withHoverEvent(null).withClickEvent(null));
        MutableComponent resetSpace = new TextComponent("  ");
        resetSpace.setStyle(resetSpace.getStyle().withColor(RESET).withHoverEvent(null).withClickEvent(null));
        return new TextComponent(" ")
                .append(front).append(resetSpace)
                .append(back).append(resetSpace)
                .append(pageIndicator).append(resetSpace)
                .append(forward).append(resetSpace)
                .append(last).append(resetSpace);
    }

    public static MutableComponent buildRegionChildrenHeader(IProtectedRegion region, RegionType type) {
        return buildHeader(new TranslatableComponent("cli.msg.info.header.in", buildRegionChildrenLink(region, type), buildRegionInfoLink(region, type)));
    }

    public static MutableComponent buildRegionParentLink(IMarkableRegion region) {
        MutableComponent parentLink = null;
        if (region.getParent() != null) { // FIXME: should not happen. it is either a dim or local region as parent
            String regionParentInfoCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getParent().getName(), INFO.toString());
            MutableComponent parentLinkText = new TranslatableComponent("cli.msg.info.region.parent.link.text", region.getParent().getName());
            MutableComponent parentHoverText = new TranslatableComponent("cli.msg.info.region.parent.link.hover", region.getParent().getName());
            if (region.getParent() instanceof DimensionalRegion) {
                return buildRegionInfoLink(region.getParent(), RegionType.DIMENSION);
            }
            if (region.getParent() instanceof IMarkableRegion) {
                String clearRegionParentCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), PARENT.toString(), CLEAR.toString(), region.getParent().getName());
                MutableComponent parentClearLinkText = new TranslatableComponent("cli.msg.info.region.parent.clear.link.text");
                MutableComponent parentClearHoverText = new TranslatableComponent("cli.msg.info.region.parent.clear.link.hover", region.getParent().getName());
                parentLink = buildExecuteCmdComponent(parentLinkText, parentHoverText, regionParentInfoCmd, RUN_COMMAND, LINK_COLOR)
                        .append(buildExecuteCmdComponent(parentClearLinkText, parentClearHoverText, clearRegionParentCmd, RUN_COMMAND, REMOVE_CMD_COLOR));
                return parentLink;
            }
            if (region.getParent() instanceof GlobalRegion) { // FIXME: Not needed here
                // TODO: Hierarchy Info for dimensional Regions
            }

        } else {
            String setRegionParentCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), PARENT.toString(), SET.toString(), "");
            MutableComponent setParentLinkText = new TranslatableComponent("cli.link.add");
            MutableComponent setParentHoverText = new TranslatableComponent("cli.msg.info.region.parent.set.link.hover", region.getName());
            parentLink = new TranslatableComponent("cli.msg.info.region.parent.null")
                    .append(" ")
                    .append(buildExecuteCmdComponent(setParentLinkText, setParentHoverText, setRegionParentCmd, RUN_COMMAND, GREEN));
        }
        return parentLink;
    }


    public static IFormattableTextComponent buildRegionListHeader(IProtectedRegion region, RegionType type) {
        return buildHeader(new TranslatableComponent("cli.msg.info.header.in",
                buildRegionChildrenLink(region, type), buildRegionInfoLink(region, type)));
    }


    // [n regions][+]
    public static MutableComponent buildDimRegionsLink(DimensionRegionCache dimCache) {
        DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
        String command = buildCommandStr(DIM.toString(), dimRegion.getDim().location().toString(), LIST.toString(), REGION.toString());
        MutableComponent listDimRegionsLinkText = new TranslatableComponent("cli.msg.dim.info.region.list.link.text", dimCache.getRegions().size());
        MutableComponent listDimRegionsHoverText = new TranslatableComponent("cli.msg.dim.info.region.list.link.hover", dimRegion.getName());
        MutableComponent listDimRegionsListLink = buildExecuteCmdComponent(listDimRegionsLinkText, listDimRegionsHoverText, command, RUN_COMMAND, LINK_COLOR);
        MutableComponent createRegionLink = buildDimCreateRegionLink(dimRegion);
        return (dimRegion.getChildren().size() == 0) ? listDimRegionsLinkText.append(createRegionLink) : listDimRegionsListLink.append(createRegionLink);
    }

    // [n children][+]
    public static MutableComponent buildRegionChildrenLink(IProtectedRegion region, RegionType type) {
        return switch (type) {
            case GLOBAL -> {
                Collection<String> dimensionList = RegionDataManager.get().getDimensionList();
                String command = buildCommandStr(GLOBAL.toString(), LIST.toString(), DIM.toString());
                MutableComponent listDimRegionsLinkText = new TranslatableComponent("cli.msg.global.info.region.list.link.text", dimensionList.size());
                MutableComponent listDimRegionsHoverText = new TranslatableComponent("cli.msg.global.info.region.list.link.hover", region.getName());
                yield buildExecuteCmdComponent(listDimRegionsLinkText, listDimRegionsHoverText, command, RUN_COMMAND, LINK_COLOR);
            }
            case DIMENSION -> {
                // TODO: children not regions
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(region.getDim());
                String command = buildCommandStr(DIM.toString(), region.getDim().location().toString(), LIST.toString(), REGION.toString());
                MutableComponent listDimRegionsLinkText = new TranslatableComponent("cli.msg.dim.info.region.list.link.text", dimCache.getRegions().size());
                MutableComponent listDimRegionsHoverText = new TranslatableComponent("cli.msg.dim.info.region.list.link.hover", region.getName());
                MutableComponent listDimRegionsListLink = buildExecuteCmdComponent(listDimRegionsLinkText, listDimRegionsHoverText, command, RUN_COMMAND, LINK_COLOR);
                yield  (region.getChildren().size() == 0) ? listDimRegionsLinkText : listDimRegionsListLink;
            }
            // [n children][+]
            case LOCAL -> {
                String regionChildrenListLink = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), CHILDREN.toString());
                MutableComponent childrenLinkText = new TranslatableComponent("cli.msg.info.region.children.link.text", region.getChildren().size());
                MutableComponent childrenHoverText = new TranslatableComponent("cli.msg.info.region.children.link.hover", region.getName());
                MutableComponent regionChildrenLink = buildExecuteCmdComponent(childrenLinkText, childrenHoverText, regionChildrenListLink, RUN_COMMAND, LINK_COLOR);
                MutableComponent addChildrenLink = buildRegionAddChildrenLink(region);
                yield  (region.getChildren().size() == 0) ? childrenLinkText.append(addChildrenLink) : regionChildrenLink.append(addChildrenLink);
            }
            default ->
                throw new IllegalStateException("Unexpected value: " + type);
        };
    }

    public static MutableComponent buildRegionAddChildrenLink(IProtectedRegion region) {
        String addChildrenCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), ADD.toString(), CHILD.toString(), "");
        MutableComponent addChildrenLinkText = new TranslatableComponent("cli.link.add");
        MutableComponent addChildrenHoverText = new TranslatableComponent("cli.msg.info.region.children.add.link.hover", region.getName());
        return buildExecuteCmdComponent(addChildrenLinkText, addChildrenHoverText, addChildrenCmd, SUGGEST_COMMAND, ADD_CMD_COLOR);
    }

    public static MutableComponent buildDimCreateRegionLink(IProtectedRegion region) {
        String dimCreateRegionCmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), CREATE.toString(), REGION.toString(), "");
        MutableComponent createRegionLinkText = new TranslatableComponent("cli.link.add");
        MutableComponent createRegionHoverText = new TranslatableComponent("cli.msg.dim.info.region.create.link.hover", region.getName());
        return buildExecuteCmdComponent(createRegionLinkText, createRegionHoverText, dimCreateRegionCmd, SUGGEST_COMMAND, ADD_CMD_COLOR);
    }

    public static MutableComponent buildFlagListLink(IProtectedRegion region, RegionType type) {
        return switch (type) {
            case GLOBAL -> {
                String flagListCmd = buildCommandStr(GLOBAL.toString(), LIST.toString(), FLAG.toString());
                MutableComponent flagListLink = buildExecuteCmdComponent(flagListLinkText, flagListHoverText, flagListCmd, RUN_COMMAND, LINK_COLOR);
                if (region.getFlags().isEmpty()) {
                    flagListLink = flagListLinkText;
                }
                yield flagListLink.append(" ").append(buildAddFlagLink(region, type));
            }
            case DIMENSION -> {
                String flagListCmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), LIST.toString(), FLAG.toString());
                MutableComponent flagListLinkText = new TranslatableComponent("cli.msg.info.region.flag.link.text", region.getFlags().size());
                MutableComponent flagListHoverText = new TranslatableComponent("cli.msg.dim.flag.list.link.hover", region.getName());
                MutableComponent dimFlagListLink = buildExecuteCmdComponent(flagListLinkText, flagListHoverText, flagListCmd, RUN_COMMAND, LINK_COLOR);
                yield dimFlagListLink.append(buildDimAddFlagLink(region));
            }
            case LOCAL -> {
                String listCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), FLAG.toString());
                MutableComponent flagListLinkText = new TranslatableComponent("cli.msg.info.region.flag.link.text", region.getFlags().size());
                MutableComponent flagListHoverText = new TranslatableComponent("cli.msg.info.region.flag.link.hover", region.getName());
                MutableComponent flagListLink = buildExecuteCmdComponent(flagListLinkText, flagListHoverText, listCmd, RUN_COMMAND, LINK_COLOR);
                yield flagListLink.append(buildRegionAddFlagLink(region));
            }
            default -> throw new IllegalStateException("Unexpected value: " + type);
        };
    }

    public static MutableComponent buildAddFlagLink(IProtectedRegion region, RegionType regionType) {
        MutableComponent hoverText = new TranslatableComponent("cli.msg.info.region.flag.add.link.hover", region.getName());
        MutableComponent linkText = new TranslatableComponent("cli.link.add");
        switch (regionType) {
            case GLOBAL: {
                String command = buildCommandStr(GLOBAL.toString(), ADD.toString(), FLAG.toString(), "");
                return buildExecuteCmdComponent(linkText, hoverText, command, SUGGEST_COMMAND, ADD_CMD_COLOR);
            }
            case DIMENSION: {
                String command = buildCommandStr(DIM.toString(), region.getDim().location().toString(), ADD.toString(), FLAG.toString(), "");
                return buildExecuteCmdComponent(linkText, hoverText, command, SUGGEST_COMMAND, ADD_CMD_COLOR);
            }
            case LOCAL: {
                String addCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), ADD.toString(), FLAG.toString(), "");
                return buildExecuteCmdComponent(linkText, hoverText, addCmd, SUGGEST_COMMAND, ADD_CMD_COLOR);
            }
            default:
                throw new IllegalStateException("Unexpected value: " + regionType);
        }
    }

    public static MutableComponent buildRegionStateLink(IProtectedRegion region, RegionType regionType) {
        MutableComponent linkText = new TranslatableComponent("cli.msg.info.region.state.link.text");
        MutableComponent hoverText = new TranslatableComponent("cli.msg.info.region.state.link.hover", region.getName());
        switch (regionType) {
            case GLOBAL: {
                String command = buildCommandStr(GLOBAL.toString(), STATE.toString());
                return buildExecuteCmdComponent(linkText, hoverText, command, SUGGEST_COMMAND, ADD_CMD_COLOR);
            }
            case DIMENSION: {
                String command = buildCommandStr(DIM.toString(), region.getDim().location().toString(), STATE.toString());
                return buildExecuteCmdComponent(linkText, hoverText, command, SUGGEST_COMMAND, ADD_CMD_COLOR);
            }
            case LOCAL: {
                String showStateCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), STATE.toString());
                return buildExecuteCmdComponent(linkText, hoverText, showStateCmd, RUN_COMMAND, LINK_COLOR);
            }
            default:
                throw new IllegalStateException("Unexpected value: " + regionType);
        }
    }

    public static MutableComponent buildDimEnableLink(IProtectedRegion region) {
        String command = ArgumentUtil.buildCommandStr(DIM.toString(), region.getDim().location().toString(), ENABLE.toString());
        String onClickAction = region.isActive() ? "deactivate" : "activate";
        String hoverText = "cli.msg.info.state." + onClickAction;
        String linkText = "cli.msg.info.state.link." + (region.isActive() ? "activate" : "deactivate");
        ChatFormatting color = region.isActive() ? ADD_CMD_COLOR : REMOVE_CMD_COLOR;
        MutableComponent stateLink = buildExecuteCmdComponent(linkText, hoverText, command, ClickEvent.Action.RUN_COMMAND, color);
        return new TranslatableComponent("cli.msg.info.state")
                .append(new TextComponent(": "))
                .append(stateLink);
    }

    public static MutableComponent buildInfoComponent(String subjectLangKey, MutableComponent payload) {
        return new TranslatableComponent(subjectLangKey).append(": ").append(payload);
    }

    public static MutableComponent buildRegionTeleportLink(IMarkableRegion region) {
        String teleportCmd = buildDimTeleportCmd(region.getDim(), "@s", region.getTpTarget());
        return buildExecuteCmdComponent(buildBlockPosTeleportLinkText(region.getTpTarget()),
                "cli.msg.region.info.tp.link.hover", teleportCmd, RUN_COMMAND, TP_COLOR);
    }

    public static MutableComponent buildDimensionalBlockTpLink(ResourceKey<Level> dim, BlockPos target) {
        String teleportCmd = buildDimTeleportCmd(dim, "@s", target);
        return buildExecuteCmdComponent(buildBlockPosLinkText(target),
                "cli.msg.info.region.area.location.teleport.link.hover", teleportCmd, RUN_COMMAND, TP_COLOR);
    }

    public static MutableComponent buildDimSuggestRegionRemovalLink(IMarkableRegion region) {
        String cmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), DELETE.toString(), region.getName());
        MutableComponent hover = new TranslatableComponent("cli.msg.info.dim.region.remove.link.hover", region.getName());
        MutableComponent text = new TranslatableComponent("cli.link.remove");
        return buildExecuteCmdComponent(text, hover, cmd, SUGGEST_COMMAND, REMOVE_CMD_COLOR);
    }

    /* TODO: extract method for n component(s) [+] */
    public static MutableComponent buildDimFlagListLink(IProtectedRegion region) {
        String command = buildCommandStr(DIM.toString(), region.getDim().location().toString(), LIST.toString(), FLAG.toString());
        MutableComponent hoverLink = new TranslatableComponent("cli.msg.dim.flag.list.link.hover", region.getDim().location().toString());
        MutableComponent linkText = new TranslatableComponent("cli.msg.flag.list.link.text", region.getFlags().size());
        return region.getFlags().isEmpty()
                ? new TranslatableComponent("cli.msg.info.region.flag.link.text", region.getFlags().size())
                : buildExecuteCmdComponent(linkText, hoverLink, command, RUN_COMMAND, LINK_COLOR);
    }

    public static List<MutableComponent> buildRemoveGroupEntries(IProtectedRegion region, List<String> affiliationNames, GroupType groupType, String affiliation, RegionType parentType) {
        return affiliationNames.stream().map(group -> buildRemoveGroupEntry(region, group, groupType, affiliation, parentType)).collect(Collectors.toList());
    }

    public static MutableComponent buildRemoveGroupEntry(IProtectedRegion region, String groupName, GroupType groupType, String affiliation, RegionType regionType) {
        MutableComponent linkText = new TranslatableComponent("cli.link.remove");
        MutableComponent hoverText = new TranslatableComponent("cli.msg.info.region.group." + groupType.name + ".remove.link.hover", groupName, region.getName());
        MutableComponent regionRemoveLink = switch (regionType) {
            case GLOBAL -> {
                String command = buildCommandStr(GLOBAL.toString(), REMOVE.toString(), groupType.name, affiliation, groupName);
                yield buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, REMOVE_CMD_COLOR);
            }
            case DIMENSION -> {
                String command = buildCommandStr(DIM.toString(), region.getDim().location().toString(), REMOVE.toString(), groupType.name, affiliation, groupName);
                yield buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, REMOVE_CMD_COLOR);
            }
            case LOCAL -> {
                String command = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), REMOVE.toString(), groupType.name, affiliation, groupName);
                yield buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, REMOVE_CMD_COLOR);
            }
            default -> throw new IllegalArgumentException();
        };
        return new TextComponent(" - ")
                .append(regionRemoveLink).append(" ")
                .append(buildGroupInfo(region, groupName, groupType));
    }

    public static MutableComponent buildGroupInfo(IProtectedRegion region, String groupName, GroupType groupType) {
        return switch (groupType) {
            case PLAYER -> {
                Player player = ServerLifecycleHooks.getCurrentServer().getPlayerList().getPlayerByName(groupName);
                yield player == null
                        ? new TextComponent(groupName).withStyle(GRAY).append(" ").append(new TranslatableComponent("cli.msg.info.player.list.entry.offline"))
                        : buildPlayerHoverComponent(player);
            }
            case TEAM -> {
                ServerLevel level = ServerLifecycleHooks.getCurrentServer().getLevel(region.getDim());
                if (level != null) {
                    Team team = level.getScoreboard().getPlayerTeam(groupName);
                    yield team == null ? new TextComponent(groupName) : buildTeamHoverComponent(team);
                } else {
                    yield new TextComponent(groupName);
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


    public static MutableComponent buildRegionRemoveChildLink(IProtectedRegion region, IProtectedRegion child) {
        String command = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), REMOVE.toString(), CHILD.toString(), child.getName());
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

    public static MutableComponent buildRegionFlagInfoHeader(IProtectedRegion region, RegionType regionType) {
        MutableComponent res;
        switch (regionType) {
            case GLOBAL:
                res = buildHeader(new TranslatableComponent("cli.msg.info.header.in", buildFlagListLink(region, regionType), buildRegionInfoLink(region, regionType)));
                break;
            case DIMENSION:
                res = buildHeader(new TranslatableComponent("cli.msg.info.header.in", buildFlagListLink(region, regionType), buildRegionInfoLink(region, regionType)));
                break;
            case LOCAL:
                res = buildHeader(new TranslatableComponent("cli.msg.info.header.in", buildFlagListLink(region, regionType), buildRegionInfoLink(region, regionType)));
                break;
            default:
                throw new IllegalStateException("Unexpected value: " + regionType);
        }
        return res;
    }

    public static MutableComponent buildFlagInfoHeader(IProtectedRegion region, IFlag flag, RegionType regionType) {
        MutableComponent res;
        switch (regionType) {
            case DIMENSION:
                res = buildHeader(new TranslatableComponent("cli.msg.info.header.flag.in", buildFlagInfoLink(region, flag, regionType), buildRegionInfoLink(region, regionType)));
                break;
            case LOCAL:
                res = buildHeader(new TranslatableComponent("cli.msg.info.header.flag.in", buildFlagInfoLink(region, flag, regionType), buildRegionInfoLink(region, regionType)));
                break;
            case GLOBAL:
                res = buildHeader(new TranslatableComponent("cli.msg.info.header.flag.in", buildFlagInfoLink(region, flag, regionType), buildRegionInfoLink(region, regionType)));
                break;
            default:
                throw new IllegalStateException("Unexpected value: " + regionType);
        }
        return res;
    }
}
