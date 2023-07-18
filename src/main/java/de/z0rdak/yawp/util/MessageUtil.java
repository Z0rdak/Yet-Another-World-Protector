package de.z0rdak.yawp.util;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.commands.CommandConstants;
import de.z0rdak.yawp.config.server.CommandPermissionConfig;
import de.z0rdak.yawp.config.server.RegionConfig;
import de.z0rdak.yawp.core.affiliation.AffiliationType;
import de.z0rdak.yawp.core.affiliation.PlayerContainer;
import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.area.IMarkableArea;
import de.z0rdak.yawp.core.flag.BooleanFlag;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.*;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.ChatFormatting;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.core.BlockPos;
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
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.config.server.RegionConfig.CLI_REGION_DEFAULT_PRIORITY_INC;
import static de.z0rdak.yawp.core.region.RegionType.LOCAL;
import static de.z0rdak.yawp.util.CommandUtil.buildCommandStr;
import static net.minecraft.ChatFormatting.RESET;
import static net.minecraft.ChatFormatting.*;
import static net.minecraft.network.chat.ClickEvent.Action.RUN_COMMAND;
import static net.minecraft.network.chat.ClickEvent.Action.SUGGEST_COMMAND;


public class MessageUtil {

    private MessageUtil() {
    }

    public static MutableComponent buildHeader(String translationKey) {
        return buildHeader(Component.translatable(translationKey));
    }

    public static MutableComponent buildHeader(String translationKey, String fallback) {
        return buildHeader(Component.translatableWithFallback(translationKey, fallback));
    }

    public static MutableComponent buildHeader(MutableComponent header) {
        return Component.literal(BOLD + "")
                .append(header)
                .append(Component.literal(BOLD + ""));
    }

    public static void sendCmdFeedback(CommandSourceStack src, MutableComponent text) {
        try {
            if (src.getEntity() == null) {
                src.sendSuccess(() -> text, true);
            } else {
                sendMessage(src.getPlayerOrException(), text);
            }
        } catch (CommandSyntaxException e) {
            YetAnotherWorldProtector.LOGGER.error(e);
        }
    }

    public static void sendCmdFeedback(CommandSourceStack src, String langKey) {
        sendCmdFeedback(src, Component.translatable(langKey));
    }

    public static void sendCmdFeedback(CommandSourceStack src, String langKey, String fallback) {
        sendCmdFeedback(src, Component.translatableWithFallback(langKey, fallback));
    }

    public static void sendMessage(Player player, MutableComponent textComponent) {
        player.sendSystemMessage(textComponent);
    }

    public static void sendDimFlagNotification(Player player, RegionFlag flag) {
        player.displayClientMessage(Component.translatableWithFallback("flag.dim.player.msg.push.deny", "The '%s' flag denies this action in this dimension!", flag.name), true);
    }

    public static void sendFlagNotification(Player player, IMarkableRegion region, RegionFlag flag) {
        player.displayClientMessage(Component.translatableWithFallback("flag.local.player.msg.push.deny", "[%s]: The '%s' flag denies this action here!", region.getName(), flag.name), true);
    }

    public final static ChatFormatting SUGGEST_COLOR = BLUE;
    public final static ChatFormatting TP_COLOR = GREEN;
    public final static ChatFormatting LINK_COLOR = AQUA;
    public final static ChatFormatting INACTIVE_LINK_COLOR = GRAY;
    public final static ChatFormatting ADD_CMD_COLOR = DARK_GREEN;
    public final static ChatFormatting REMOVE_CMD_COLOR = DARK_RED;
    public static int FIRST_PAGE_IDX = 0;

    public static String buildTeleportCmd(String tpSource, BlockPos target) {
        return "tp " + tpSource + " " + target.getX() + " " + target.getY() + " " + target.getZ();
    }

    public static String buildTeleportLinkText(ResourceKey<Level> dim, BlockPos target) {
        return buildTeleportLinkText(dim.location().toString(), target);
    }

    public static String buildTeleportLinkText(String regionName, BlockPos target) {
        return regionName + " @ [" + buildBlockPosTeleportLinkText(target) + "]";
    }

    public static String buildBlockPosTeleportLinkText(BlockPos target) {
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
                .withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, Component.translatableWithFallback("cli.msg.info.region.affiliation.link.hover","Click to display team info")))
                .withClickEvent(new ClickEvent(RUN_COMMAND, "/team list " + team.getName())));
        return playerName;
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

    public static MutableComponent buildRegionAreaDetailComponent(IMarkableRegion region) {
        IMarkableArea area = region.getArea();
        MutableComponent areaInfo = Component.literal(area.getAreaType().areaType)
                .append("\n");
        switch (area.getAreaType()) {
            case CUBOID: {
                CuboidArea cuboidArea = (CuboidArea) area;
                MutableComponent sizeInfo = Component.translatableWithFallback("cli.msg.info.region.spatial.area.size", "Size")
                        .append(": ")
                        .append("X=" + cuboidArea.getXsize() + ", Y=" + cuboidArea.getYsize() + ", Z=" + cuboidArea.getZsize());
                MutableComponent markedBlocksInfo = Component.translatableWithFallback("cli.msg.info.region.spatial.area.blocks", "Marked Blocks")
                        .append(": ")
                        .append(buildBlockPosTpLinks(region));
                areaInfo.append(sizeInfo).append("\n")
                        .append(markedBlocksInfo);
                return areaInfo;
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

    public static MutableComponent buildTextWithHoverMsg(MutableComponent text, MutableComponent hoverText, ChatFormatting color) {
        MutableComponent bracketedText = ComponentUtils.wrapInSquareBrackets(text);
        bracketedText.setStyle(bracketedText.getStyle().withColor(color).withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, hoverText)));
        return bracketedText;
    }

    public static MutableComponent buildDimensionTeleportLink(IMarkableRegion region) {
        String cmdLinkText = buildTeleportLinkText(region.getDim(), region.getTpTarget());
        String executeCmdStr = buildDimTeleportCmd(region.getDim(), "@s", region.getTpTarget());
        MutableComponent teleportCmdHoverText = Component.translatableWithFallback("cli.msg.info.region.spatial.location.teleport", "Teleport to region %s in dimension %s", region.getName(), region.getDim().location().toString());
        return buildExecuteCmdComponent(Component.literal(cmdLinkText), teleportCmdHoverText, executeCmdStr, RUN_COMMAND, TP_COLOR);
    }

    // Not used at the moment
    public static MutableComponent buildHelpSuggestionLink(String translationKey, CommandConstants baseCmd, CommandConstants cmd) {
        String command = "/" + CommandPermissionConfig.BASE_CMD + " " + baseCmd + " " + cmd + " ";
        MutableComponent text = Component.translatableWithFallback("=>", "=>");
        MutableComponent hover = Component.translatableWithFallback("not.existing.key", "place-text-here");
        return Component.literal(" ")
                .append(buildExecuteCmdComponent(text, hover, command, SUGGEST_COMMAND, SUGGEST_COLOR))
                .append(Component.literal(" "))
                .append(Component.translatable(translationKey));
    }

    public static String buildDimCmdStr(IProtectedRegion region, CommandConstants constant) {
        return CommandUtil.buildCommandStr(DIM.toString(), region.getDim().location().toString(), constant.toString());
    }

    public static String buildRegionCmdStr(IProtectedRegion region, CommandConstants constant) {
        return CommandUtil.buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), constant.toString());
    }

    public static MutableComponent buildRegionEnableComponent(IMarkableRegion region) {
        String cmd = CommandUtil.buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), STATE.toString(), ENABLE.toString());
        MutableComponent text = Component.translatableWithFallback("cli.msg.info.region.state.enable." + region.isActive() + ".link.text", region.isActive() ? "yes" : "no");
        MutableComponent hover = Component.translatableWithFallback("cli.msg.info.region.state.enable." + !region.isActive() + ".link.hover",
                !region.isActive() ? "Disable flag checks" : "Enable flag checks");
        ChatFormatting color = region.isActive() ? ADD_CMD_COLOR : REMOVE_CMD_COLOR;
        return buildExecuteCmdComponent(text, hover, cmd, ClickEvent.Action.RUN_COMMAND, color);
    }

    public static MutableComponent buildRegionPriorityComponent(IMarkableRegion region) {
        String incPriorityCmd = CommandUtil.buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), STATE.toString(), PRIORITY.toString(), INC.toString(), String.valueOf(CLI_REGION_DEFAULT_PRIORITY_INC.get()));
        MutableComponent incLinkText = Component.translatableWithFallback("cli.msg.info.region.state.priority.increase.link.text", "+%s", CLI_REGION_DEFAULT_PRIORITY_INC.get());
        MutableComponent incHoverText = Component.translatableWithFallback("cli.msg.info.region.state.priority.increase.link.hover", "Increase region priority by %s", CLI_REGION_DEFAULT_PRIORITY_INC.get());
        MutableComponent increaseLink = buildExecuteCmdComponent(incLinkText, incHoverText, incPriorityCmd, ClickEvent.Action.RUN_COMMAND, ADD_CMD_COLOR);
        String decPriorityCmd = CommandUtil.buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), STATE.toString(), PRIORITY.toString(), DEC.toString(), String.valueOf(CLI_REGION_DEFAULT_PRIORITY_INC.get()));
        MutableComponent decLinkText = Component.translatableWithFallback("cli.msg.info.region.state.priority.decrease.link.text", "-%s", CLI_REGION_DEFAULT_PRIORITY_INC.get());
        MutableComponent decHoverText = Component.translatableWithFallback("cli.msg.info.region.state.priority.decrease.link.hover", "Decrease region priority by %s", CLI_REGION_DEFAULT_PRIORITY_INC.get());
        MutableComponent decreaseLink = buildExecuteCmdComponent(decLinkText, decHoverText, decPriorityCmd, ClickEvent.Action.RUN_COMMAND, REMOVE_CMD_COLOR);
        MutableComponent priorityValue = Component.literal(String.valueOf(region.getPriority()));
        String setPriorityCmd = CommandUtil.buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), STATE.toString(), PRIORITY.toString(), "");
        MutableComponent setPriorityLinkText = Component.translatableWithFallback("cli.msg.info.region.state.priority.set.link.text", "#");
        MutableComponent setPriorityHoverText = Component.translatableWithFallback("cli.msg.info.region.state.priority.set.link.hover", "Set priority for region");
        MutableComponent setPriorityLink = buildExecuteCmdComponent(setPriorityLinkText, setPriorityHoverText, setPriorityCmd, SUGGEST_COMMAND, SUGGEST_COLOR);
        return priorityValue.append(" ")
                .append(setPriorityLink).append(" ")
                .append(increaseLink).append(" ")
                .append(decreaseLink);
    }

    public static MutableComponent buildRegionAlertComponentLink(IMarkableRegion region) {
        String cmd = CommandUtil.buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), STATE.toString(), ALERT.toString());
        MutableComponent text = Component.translatableWithFallback("cli.msg.info.region.state.alert." + !region.isMuted() + ".link.text",
                !region.isMuted() ? "off" : "on");
        MutableComponent hover = Component.translatableWithFallback( "cli.msg.info.region.state.alert." + region.isMuted() + ".link.hover",
                "Turn flag alerts "+ (region.isMuted()? "on" : "off"));
        ChatFormatting color = region.isMuted() ? REMOVE_CMD_COLOR : ADD_CMD_COLOR;
        return buildExecuteCmdComponent(text, hover, cmd, ClickEvent.Action.RUN_COMMAND, color);
    }

    public static MutableComponent buildRegionInfoLink(IProtectedRegion region, RegionType type) {
        return switch (type) {
            case DIMENSION -> {
                String command = buildCommandStr(DIM.toString(), region.getDim().location().toString(), INFO.toString());
                MutableComponent hoverText = Component.translatableWithFallback("cli.msg.dim.info", "Prompt region info");
                MutableComponent linkText = Component.literal(region.getDim().location().toString());
                yield buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, LINK_COLOR);
            }
            case LOCAL -> {
                String cmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), INFO.toString());
                MutableComponent regionInfoLinkText = Component.literal(region.getName());
                MutableComponent regionInfoLinkHover = Component.translatableWithFallback("cli.msg.info.region", "Show region info for region %s", region.getName());
                yield buildExecuteCmdComponent(regionInfoLinkText, regionInfoLinkHover, cmd, RUN_COMMAND, LINK_COLOR);
            }
            default -> throw new IllegalStateException("Unexpected value: " + type);
        };
    }

    public static MutableComponent buildRegionSpatialPropLink(IMarkableRegion region) {
        String showSpatialPropLink = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), SPATIAL.toString());
        MutableComponent spatialPropLinkText = Component.translatableWithFallback("cli.msg.info.region.spatial.link.text", "Spatial Properties");
        MutableComponent spatialPropHoverText = Component.translatableWithFallback("cli.msg.info.region.spatial.link.hover", "Show region spatial properties for %s", region.getName());
        return buildExecuteCmdComponent(spatialPropLinkText, spatialPropHoverText, showSpatialPropLink, RUN_COMMAND, LINK_COLOR);
    }

    public static MutableComponent buildRegionOverviewHeader(IProtectedRegion region, RegionType type) {
        return switch (type) {
            case DIMENSION -> {
                MutableComponent dumpLinkText = Component.translatableWithFallback("cli.msg.dim.overview.header.dump.link.text", "Dimension overview");
                MutableComponent dumpLinkHover = Component.translatableWithFallback("cli.msg.dim.overview.header.dump.link.hover","Copy Dimensional Region NBT to clipboard");
                MutableComponent clipBoardDumpLink = buildExecuteCmdComponent(dumpLinkText, dumpLinkHover, NbtUtils.prettyPrint(region.serializeNBT()), ClickEvent.Action.COPY_TO_CLIPBOARD, GOLD);
                yield buildHeader(Component.translatableWithFallback("cli.msg.info.header.for", "== %s for %s ==", clipBoardDumpLink, buildRegionInfoLink(region, RegionType.DIMENSION)));
            }
            case LOCAL -> {
                MutableComponent dumpLinkText = Component.translatableWithFallback("cli.msg.info.region.overview.dump.link.text","Region overview");
                MutableComponent dumpLinkHover = Component.translatableWithFallback("cli.msg.info.region.overview.dump.link.hover","Copy Region NBT to clipboard");
                MutableComponent clipBoardDumpLink = buildExecuteCmdComponent(dumpLinkText, dumpLinkHover, NbtUtils.prettyPrint(region.serializeNBT()), ClickEvent.Action.COPY_TO_CLIPBOARD, GOLD);
                yield buildHeader(Component.translatableWithFallback("cli.msg.info.header.for",  "== %s for %s ==", clipBoardDumpLink, buildRegionInfoLink(region, LOCAL)));
            }
            default -> throw new IllegalStateException("Unexpected value: " + type);
        };
    }

    /**
     * Players: [n player(s)] [+]
     * // TODO:
     */
    public static MutableComponent buildPlayerListLink(IProtectedRegion region, PlayerContainer players, String affiliation, RegionType regionType) {
        MutableComponent hoverText = Component.translatableWithFallback("cli.msg.info.region.affiliation.player.list.link.hover", "List players of affiliation '%s' in region %s", affiliation, region.getName());
        MutableComponent linkText = Component.translatableWithFallback("cli.msg.info.region.affiliation.player.list.link.text", "%s player(s)", players.getPlayers().size());
        return switch (regionType) {
            case DIMENSION -> {
                String cmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), LIST.toString(), affiliation, PLAYER.toString());
                yield buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            case LOCAL -> {
                String cmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), affiliation, PLAYER.toString());
                yield buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            default -> throw new IllegalArgumentException();
        };
    }

    /**
     * Teams: [n team(s)] [+]
     */
    public static MutableComponent buildTeamListLink(IProtectedRegion region, PlayerContainer teams, String affiliation, RegionType regionType) {
        MutableComponent hoverText = Component.translatableWithFallback("cli.msg.info.region.affiliation.team.list.link.hover", "List teams of affiliation '%s' in region %s", affiliation, region.getName());
        MutableComponent linkText = Component.translatableWithFallback("cli.msg.info.region.affiliation.team.list.link.text", "%s team(s)", teams.getTeams().size());
        return switch (regionType) {
            case DIMENSION -> {
                String cmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), LIST.toString(), affiliation, TEAM.toString());
                yield buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            case LOCAL -> {
                String cmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), affiliation, TEAM.toString());
                yield buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            default -> throw new IllegalArgumentException();
        };
    }

    public static MutableComponent buildAddAffiliateLink(IProtectedRegion region, String affiliation, AffiliationType affiliationType, RegionType regionType) {
        MutableComponent linkText = Component.translatableWithFallback("cli.link.add","+");
        String affiliationFallback = "Add "+ affiliationType.name+" as '%s' to region %s";
        MutableComponent hoverText = Component.translatableWithFallback("cli.msg.info.region.affiliation." + affiliationType.name + ".add.link.hover", affiliationFallback, affiliation, region.getName());
        String subCmd = " " + affiliationType.name + " " + affiliation + " ";
        return switch (regionType) {
            case LOCAL -> {
                String cmd = buildRegionCmdStr(region, ADD) + subCmd;
                yield buildExecuteCmdComponent(linkText, hoverText, cmd, SUGGEST_COMMAND, ADD_CMD_COLOR);
            }
            case DIMENSION -> {
                String cmd = buildDimCmdStr(region, ADD) + subCmd;
                yield buildExecuteCmdComponent(linkText, hoverText, cmd, SUGGEST_COMMAND, ADD_CMD_COLOR);
            }
            default -> throw new IllegalArgumentException();
        };
    }

    public static MutableComponent buildAffiliationLinks(IProtectedRegion region, RegionType regionType) {
        MutableComponent affiliationLinks = Component.literal("");
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

    public static MutableComponent buildAffiliationHeader(IProtectedRegion region, String affiliation, RegionType regionType) {
        int amountOwners = (region.getOwners().getTeams().size() + region.getOwners().getPlayers().size());
        int amountMembers = (region.getMembers().getTeams().size() + region.getMembers().getPlayers().size());
        int affiliationSize = affiliation.equals("owner") ? amountOwners : amountMembers;
        MutableComponent affiliationLink = buildAffiliationLink(region, affiliation, affiliationSize, regionType);
        return buildHeader(Component.translatableWithFallback("cli.msg.info.header.in", "== %s in %s ==", affiliationLink, buildRegionInfoLink(region, regionType)));
    }

    public static MutableComponent buildAffiliationHeader(IProtectedRegion region, String affiliation, AffiliationType affiliationType, RegionType regionType) {
        String fallbackString = "== Region '%s' "+ affiliationType.name+"s in %s ==";
        return Component.translatableWithFallback("cli.msg.info.region.affiliation." + affiliationType.name + ".list", fallbackString, buildRegionInfoLink(region, regionType), affiliation);
    }

    public static MutableComponent buildAffiliationLink(IProtectedRegion region, String affiliation, int affiliationSize, RegionType regionType) {
        MutableComponent linkText = Component.translatableWithFallback("cli.msg.info.region.affiliation.list.link.text", "%s %s(s)", affiliationSize, affiliation);
        MutableComponent hoverText = Component.translatableWithFallback("cli.msg.info.region.affiliation.list.link.hover", "List %ss for region %s", affiliation, region.getName());
        return switch (regionType) {
            case LOCAL -> {
                String cmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), affiliation);
                yield buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            case DIMENSION -> {
                String cmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), LIST.toString(), affiliation);
                yield buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            default -> throw new IllegalArgumentException();
        };
    }

    // TODO: combine team and player list link
    public static MutableComponent buildAffiliationTeamListLink(IProtectedRegion region, String affiliation, RegionType regionType) {
        // Teams: [n team(s)] [+]
        PlayerContainer playerContainer = affiliation.equals("owner") ? region.getOwners() : region.getMembers();
        MutableComponent teams = Component.translatableWithFallback("cli.msg.info.region.affiliation.team", "Teams").append(": ");
        MutableComponent teamAddLink = buildAddAffiliateLink(region, affiliation, AffiliationType.TEAM, regionType);
        MutableComponent teamListLink = playerContainer.hasTeams()
                ? buildTeamListLink(region, playerContainer, affiliation, regionType)
                : Component.translatableWithFallback("cli.msg.info.region.affiliation.team.list.link.text", "%s team(s)", playerContainer.getTeams().size());
        teams.append(teamListLink).append(teamAddLink);
        return teams;
    }

    // TODO: combine team and player list link
    public static MutableComponent buildAffiliationPlayerListLink(IProtectedRegion region, String affiliation, RegionType regionType) {
        // Players: [n player(s)] [+]
        PlayerContainer playerContainer = affiliation.equals("owner") ? region.getOwners() : region.getMembers();
        MutableComponent players = Component.translatableWithFallback("cli.msg.info.region.affiliation.player", "Players").append(": ");
        MutableComponent playersAddLink = buildAddAffiliateLink(region, affiliation, AffiliationType.PLAYER, regionType);
        MutableComponent playerListLink = playerContainer.hasPlayers()
                ? buildPlayerListLink(region, playerContainer, affiliation, regionType)
                : Component.translatableWithFallback("cli.msg.info.region.affiliation.player.list.link.text", "%s player(s)", playerContainer.getPlayers().size());
        players.append(playerListLink).append(playersAddLink);
        return players;
    }

    private static MutableComponent buildRemoveFlagEntry(IProtectedRegion region, IFlag flag, RegionType regionType) {
        MutableComponent flagRemoveEntry = Component.literal(" - ");
        MutableComponent flagRemoveLink;
        switch (regionType) {
            case DIMENSION: {
                String command = buildCommandStr(DIM.toString(), region.getDim().location().toString(), REMOVE.toString(), FLAG.toString(), flag.getFlagIdentifier());
                MutableComponent hoverText = Component.translatableWithFallback("cli.msg.dim.info.flag.remove.link.hover", "Remove flag '%s' from dimension %s", flag.getFlagIdentifier(), region.getDim().location().toString());
                MutableComponent linkText = Component.translatableWithFallback("cli.link.remove", "x");
                flagRemoveLink = buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, REMOVE_CMD_COLOR);
                break;
            }
            case LOCAL: {
                String cmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), REMOVE.toString(), FLAG.toString(), flag.getFlagIdentifier());
                MutableComponent hoverText = Component.translatableWithFallback("cli.msg.info.region.flag.remove.link.hover", "Remove flag '%s' from region %s", flag.getFlagIdentifier(), region.getName());
                MutableComponent linkText = Component.translatableWithFallback("cli.link.remove", "x");
                flagRemoveLink = buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, REMOVE_CMD_COLOR);
                break;
            }
            default:
                throw new IllegalArgumentException();
        }
        return flagRemoveEntry.append(flagRemoveLink).append(" ").append(buildFlagQuickInfo(flag));
    }

    // TODO: Add command to toggle negated and active state, and add link here as well
    public static MutableComponent buildFlagQuickInfo(IFlag flag) {
        return switch (flag.getFlagType()) {
            case BOOLEAN_FLAG -> {
                BooleanFlag boolFlag = (BooleanFlag) flag;
                MutableComponent flagName = Component.literal(boolFlag.getFlagIdentifier());
                MutableComponent flagInfo = Component.literal("Flag state: Active=" + boolFlag.isActive() + ", negated=" + boolFlag.isInverted());
                yield buildTextWithHoverMsg(flagName, flagInfo, ITALIC);
            }
            default -> throw new IllegalStateException("Unexpected value: " + flag.getFlagType());
        };
    }

    public static MutableComponent buildFlagCmdInfoLink(IProtectedRegion region, RegionType regionType, IFlag iflag) {
        switch (regionType) {
            case DIMENSION: {
                String cmd = buildCommandStr(FLAG.toString(), DIM.toString(), region.getDim().location().toString());
            }
            break;
            case LOCAL: {
            }
            break;
        }
        return Component.literal(iflag.getFlagIdentifier());
    }

    public static List<MutableComponent> buildRemoveFlagEntries(IProtectedRegion region, List<IFlag> flags, RegionType regionType) {
        return flags.stream().map(flag -> buildRemoveFlagEntry(region, flag, regionType)).collect(Collectors.toList());
    }

    public static List<MutableComponent> buildRemoveRegionEntries(IProtectedRegion parent, List<IMarkableRegion> regions, RegionType parentType) {
        return regions.stream().map(region -> buildRemoveRegionEntry(parent, region, parentType)).collect(Collectors.toList());
    }

    public static MutableComponent buildRemoveRegionEntry(IProtectedRegion parent, IMarkableRegion region, RegionType parentType) {
        Style resetStyle = Style.EMPTY.withColor(WHITE).withHoverEvent(null).withClickEvent(null);
        MutableComponent separator = Component.literal(" ").setStyle(resetStyle);
        MutableComponent regionRemoveLink = switch (parentType) {
            case DIMENSION -> {
                MutableComponent removeLink = buildDimSuggestRegionRemovalLink(region);
                removeLink.append(separator).append(buildRegionInfoLink(region, LOCAL));
                MutableComponent childIndicator = buildTextWithHoverMsg(Component.literal("*"),
                        Component.translatableWithFallback("cli.msg.info.dim.region.child.hover", "Marks this region as a direct child region"), GOLD);
                if (parent.hasChild(region)) {
                    removeLink.append(childIndicator.setStyle(childIndicator.getStyle().withInsertion("Test")));
                }
                removeLink.append(Component.literal(" @ ").setStyle(resetStyle)).append(buildRegionTeleportLink(region));
                yield removeLink;
            }
            case LOCAL ->
                    buildRegionRemoveChildLink(parent, region).append(separator).append(buildRegionInfoLink(region, LOCAL));
            default -> throw new IllegalArgumentException();
        };
        return Component.literal(" - ").append(regionRemoveLink);
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
            paginationComponents.add(Component.translatableWithFallback("cli.msg.info.pagination.error.index", "Invalid page index supplied: %s (Try 0-%s)",pageNo, numberOfPages - 1).withStyle(RED));
            return paginationComponents;
        }
        boolean hasMultiplePages = numberOfPages > 1;

        MutableComponent firstText = Component.translatableWithFallback("cli.msg.info.pagination.first.text", "⏮");
        MutableComponent firstHover = Component.translatableWithFallback("cli.msg.info.pagination.first.hover", "First page");
        MutableComponent prevText = Component.translatableWithFallback("cli.msg.info.pagination.previous.text", "◀");
        MutableComponent prevHover = Component.translatableWithFallback("cli.msg.info.pagination.previous.hover", "Previous page");
        MutableComponent nextText = Component.translatableWithFallback("cli.msg.info.pagination.next.text", "▶");
        MutableComponent nextHover = Component.translatableWithFallback("cli.msg.info.pagination.next.hover", "Next page");
        MutableComponent lastText = Component.translatableWithFallback("cli.msg.info.pagination.last.text", "⏭");
        MutableComponent lastHover = Component.translatableWithFallback("cli.msg.info.pagination.last.hover", "Last page");
        MutableComponent first = hasMultiplePages && pageNo != FIRST_PAGE_IDX
                ? buildExecuteCmdComponent(firstText, firstHover, buildPageCommand(cmd, FIRST_PAGE_IDX), RUN_COMMAND, LINK_COLOR)
                : ComponentUtils.wrapInSquareBrackets(firstText).withStyle(INACTIVE_LINK_COLOR);
        MutableComponent prev = hasMultiplePages && pageNo > FIRST_PAGE_IDX
                ? buildExecuteCmdComponent(prevText, prevHover, buildPageCommand(cmd, Math.max(pageNo - 1, FIRST_PAGE_IDX)), RUN_COMMAND, LINK_COLOR)
                : ComponentUtils.wrapInSquareBrackets(prevText).withStyle(INACTIVE_LINK_COLOR);
        MutableComponent next = hasMultiplePages && pageNo < numberOfPages - 1
                ? buildExecuteCmdComponent(nextText, nextHover, buildPageCommand(cmd, Math.min(pageNo + 1, numberOfPages - 1)), RUN_COMMAND, LINK_COLOR)
                : ComponentUtils.wrapInSquareBrackets(nextText).withStyle(INACTIVE_LINK_COLOR);
        MutableComponent last = hasMultiplePages && pageNo < numberOfPages - 1
                ? buildExecuteCmdComponent(lastText, lastHover, buildPageCommand(cmd, numberOfPages - 1), RUN_COMMAND, LINK_COLOR)
                : ComponentUtils.wrapInSquareBrackets(lastText).withStyle(INACTIVE_LINK_COLOR);

        MutableComponent paginationControl = buildPaginationControl(first, prev, pageNo, numberOfPages, next, last);
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

    public static MutableComponent buildPaginationControl(MutableComponent front, MutableComponent back, int pageNo, int maxPage, MutableComponent forward, MutableComponent last) {
        // [<<]  [<]  x/n  [>]  [>>]
        MutableComponent pageIndicator = Component.literal((pageNo + 1) + "/" + (maxPage));
        pageIndicator.setStyle(pageIndicator.getStyle().withColor(RESET).withHoverEvent(null).withClickEvent(null));
        MutableComponent resetSpace = Component.literal("  ");
        resetSpace.setStyle(resetSpace.getStyle().withColor(RESET).withHoverEvent(null).withClickEvent(null));
        return Component.literal(" ")
                .append(front).append(resetSpace)
                .append(back).append(resetSpace)
                .append(pageIndicator).append(resetSpace)
                .append(forward).append(resetSpace)
                .append(last).append(resetSpace);
    }

    public static MutableComponent buildRegionChildrenHeader(IProtectedRegion region, RegionType type) {
        return buildHeader(Component.translatableWithFallback("cli.msg.info.header.in", "== %s in %s ==",
                buildRegionChildrenLink(region, type), buildRegionInfoLink(region, type)));
    }

    public static MutableComponent buildRegionParentLink(IMarkableRegion region) {
        MutableComponent parentLink = null;
        if (region.getParent() != null) { // FIXME: should not happen. it is either a dim or local region as parent
            String regionParentInfoCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getParent().getName(), INFO.toString());
            MutableComponent parentLinkText = Component.translatableWithFallback("cli.msg.info.region.parent.link.text", "%s", region.getParent().getName());
            MutableComponent parentHoverText = Component.translatableWithFallback("cli.msg.info.region.parent.link.hover",  "Show info about regions parent '%s'", region.getParent().getName());
            if (region.getParent() instanceof DimensionalRegion) {
                return buildRegionInfoLink(region.getParent(), RegionType.DIMENSION);
            }
            if (region.getParent() instanceof IMarkableRegion) {
                String clearRegionParentCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), PARENT.toString(), CLEAR.toString(), region.getParent().getName());
                MutableComponent parentClearLinkText = Component.translatableWithFallback("cli.msg.info.region.parent.clear.link.text", "-");
                MutableComponent parentClearHoverText = Component.translatableWithFallback("cli.msg.info.region.parent.clear.link.hover", "Clear %s as parent region", region.getParent().getName());
                parentLink = buildExecuteCmdComponent(parentLinkText, parentHoverText, regionParentInfoCmd, RUN_COMMAND, LINK_COLOR)
                        .append(buildExecuteCmdComponent(parentClearLinkText, parentClearHoverText, clearRegionParentCmd, RUN_COMMAND, REMOVE_CMD_COLOR));
                return parentLink;
            }
            if (region.getParent() instanceof GlobalRegion) { // FIXME: Not needed here
                // TODO: Hierarchy Info for dimensional Regions
            }

        } else {
            String setRegionParentCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), PARENT.toString(), SET.toString(), "");
            MutableComponent setParentLinkText = Component.translatableWithFallback("cli.link.add", "+");
            MutableComponent setParentHoverText = Component.translatableWithFallback("cli.msg.info.region.parent.set.link.hover", "Set parent for region %s", region.getName());
            parentLink = Component.translatableWithFallback("cli.msg.info.region.parent.null", "No parent set")
                    .append(" ")
                    .append(buildExecuteCmdComponent(setParentLinkText, setParentHoverText, setRegionParentCmd, RUN_COMMAND, GREEN));
        }
        return parentLink;
    }

    public static MutableComponent buildDimRegionListHeader(DimensionalRegion dimRegion) {
        return buildHeader(Component.translatableWithFallback("cli.msg.info.header.in", "== %s in %s ==",
                buildRegionChildrenLink(dimRegion, RegionType.DIMENSION), buildRegionInfoLink(dimRegion, RegionType.DIMENSION)));
    }

    public static MutableComponent buildRegionChildrenLink(IProtectedRegion region, RegionType type) {
        return switch (type) {
            case DIMENSION -> {
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(region.getDim());
                String command = buildCommandStr(DIM.toString(), region.getDim().location().toString(), LIST.toString(), REGION.toString());
                MutableComponent listDimRegionsLinkText = Component.translatableWithFallback("cli.msg.dim.info.region.list.link.text", "%s region(s)",dimCache.getRegions().size());
                MutableComponent listDimRegionsHoverText = Component.translatableWithFallback("cli.msg.dim.info.region.list.link.hover", "List regions in %s", region.getName());
                MutableComponent listDimRegionsListLink = buildExecuteCmdComponent(listDimRegionsLinkText, listDimRegionsHoverText, command, RUN_COMMAND, LINK_COLOR);
                MutableComponent createRegionLink = buildDimCreateRegionLink(region);
                yield (region.getChildren().size() == 0) ? listDimRegionsLinkText.append(createRegionLink) : listDimRegionsListLink.append(createRegionLink);
            }
            case LOCAL -> {
                String regionChildrenListLink = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), CHILDREN.toString());
                MutableComponent childrenLinkText = Component.translatableWithFallback("cli.msg.info.region.children.link.text", "%s children", region.getChildren().size());
                MutableComponent childrenHoverText = Component.translatableWithFallback("cli.msg.info.region.children.link.hover", "Show list of children for region %s", region.getName());
                MutableComponent regionChildrenLink = buildExecuteCmdComponent(childrenLinkText, childrenHoverText, regionChildrenListLink, RUN_COMMAND, LINK_COLOR);
                MutableComponent addChildrenLink = buildRegionAddChildrenLink(region);
                yield (region.getChildren().size() == 0) ? childrenLinkText.append(addChildrenLink) : regionChildrenLink.append(addChildrenLink);
            }
            default -> throw new IllegalStateException("Unexpected value: " + type);
        };
    }

    public static MutableComponent buildRegionAddChildrenLink(IProtectedRegion region) {
        String addChildrenCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), ADD.toString(), CHILD.toString(), "");
        MutableComponent addChildrenLinkText = Component.translatableWithFallback("cli.link.add", "+");
        MutableComponent addChildrenHoverText = Component.translatableWithFallback("cli.msg.info.region.children.add.link.hover", "Add child for region %s", region.getName());
        return buildExecuteCmdComponent(addChildrenLinkText, addChildrenHoverText, addChildrenCmd, SUGGEST_COMMAND, ADD_CMD_COLOR);
    }

    public static MutableComponent buildDimCreateRegionLink(IProtectedRegion region) {
        String dimCreateRegionCmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), CREATE.toString(), REGION.toString(), "");
        MutableComponent createRegionLinkText = Component.translatableWithFallback("cli.link.add", "+");
        MutableComponent createRegionHoverText = Component.translatableWithFallback("cli.msg.dim.info.region.create.link.hover", "Create region in dimension %s", region.getName());
        return buildExecuteCmdComponent(createRegionLinkText, createRegionHoverText, dimCreateRegionCmd, SUGGEST_COMMAND, ADD_CMD_COLOR);
    }

    public static MutableComponent buildFlagListLink(IProtectedRegion region, RegionType type) {
        MutableComponent flagLink = switch (type) {
            case DIMENSION -> {
                String flagListCmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), LIST.toString(), FLAG.toString());
                MutableComponent flagListLinkText = Component.translatableWithFallback("cli.msg.info.region.flag.link.text", "%s flag(s)", region.getFlags().size());
                MutableComponent flagListHoverText = Component.translatableWithFallback("cli.msg.dim.flag.list.link.hover", "Show region flags for region %s", region.getName());
                MutableComponent dimFlagListLink = buildExecuteCmdComponent(flagListLinkText, flagListHoverText, flagListCmd, RUN_COMMAND, LINK_COLOR);
                yield dimFlagListLink.append(buildDimAddFlagLink(region));
            }
            case LOCAL -> {
                String listCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), FLAG.toString());
                MutableComponent flagListLinkText = Component.translatableWithFallback("cli.msg.info.region.flag.link.text", "%s flag(s)", region.getFlags().size());
                MutableComponent flagListHoverText = Component.translatableWithFallback("cli.msg.info.region.flag.link.hover", "Show region flags for region %s", region.getName());
                MutableComponent flagListLink = buildExecuteCmdComponent(flagListLinkText, flagListHoverText, listCmd, RUN_COMMAND, LINK_COLOR);
                yield flagListLink.append(buildRegionAddFlagLink(region));
            }
            default -> throw new IllegalStateException("Unexpected value: " + type);
        };
        return flagLink;
    }

    public static MutableComponent buildDimAddFlagLink(IProtectedRegion dimRegion) {
        String command = buildCommandStr(DIM.toString(), dimRegion.getDim().location().toString(), ADD.toString(), FLAG.toString(), "");
        MutableComponent hoverText = Component.translatableWithFallback("cli.msg.dim.flag.add.link.hover", "Add new flag to dimension %s", dimRegion.getDim().location().toString());
        MutableComponent linkText = Component.translatableWithFallback("cli.link.add", "+");
        return buildExecuteCmdComponent(linkText, hoverText, command, SUGGEST_COMMAND, ADD_CMD_COLOR);
    }

    public static MutableComponent buildRegionAddFlagLink(IProtectedRegion region) {
        String addCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), ADD.toString(), FLAG.toString(), "");
        MutableComponent flagAddHoverText = Component.translatableWithFallback("cli.msg.info.region.flag.add.link.hover", "Add new flag to region %s", region.getName());
        MutableComponent flagAddLinkText = Component.translatableWithFallback("cli.link.add", "+");
        MutableComponent addFlag = buildExecuteCmdComponent(flagAddLinkText, flagAddHoverText, addCmd, SUGGEST_COMMAND, ADD_CMD_COLOR);
        return addFlag;
    }

    public static MutableComponent buildRegionStateLink(IMarkableRegion region) {
        String showStateCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), STATE.toString());
        MutableComponent stateLinkText = Component.translatableWithFallback("cli.msg.info.region.state.link.text", "State");
        MutableComponent stateHoverText = Component.translatableWithFallback("cli.msg.info.region.state.link.hover", "Show region state for %s", region.getName());
        return buildExecuteCmdComponent(stateLinkText, stateHoverText, showStateCmd, RUN_COMMAND, LINK_COLOR);
    }

    public static MutableComponent buildStateLink(IProtectedRegion region) {
        String command = CommandUtil.buildCommandStr(DIM.toString(), region.getDim().location().toString(), ENABLE.toString());
        String onClickAction = region.isActive() ? "deactivate" : "activate";
        ChatFormatting color = region.isActive() ? ADD_CMD_COLOR : REMOVE_CMD_COLOR;
        MutableComponent text = Component.translatableWithFallback("cli.msg.info.state." + onClickAction,
                region.isActive() ? "active" : "inactive");
        MutableComponent hover = Component.translatableWithFallback("cli.msg.info.state.link." + (region.isActive() ? "activate" : "deactivate"),
                region.isActive() ? "Deactivate region" : "Activate region");
        MutableComponent stateLink = buildExecuteCmdComponent(text, hover, command, ClickEvent.Action.RUN_COMMAND, color);
        return Component.translatableWithFallback("cli.msg.info.state", "State")
                .append(Component.literal(": "))
                .append(stateLink);
    }

    public static MutableComponent buildInfoComponent(String subjectLangKey, String fallback, MutableComponent payload) {
        return Component.translatableWithFallback(subjectLangKey, fallback).append(": ").append(payload);
    }

    public static MutableComponent buildRegionTeleportLink(IMarkableRegion region) {
        String teleportCmd = buildDimTeleportCmd(region.getDim(), "@s", region.getTpTarget());
        String text = buildBlockPosTeleportLinkText(region.getTpTarget());
        MutableComponent textComp = Component.translatableWithFallback(text, text);
        MutableComponent hover = Component.translatableWithFallback("cli.msg.region.info.tp.link.hover", "Teleport to region");

        return buildExecuteCmdComponent(textComp, hover, teleportCmd, RUN_COMMAND, TP_COLOR);
    }

    public static MutableComponent buildDimensionalBlockTpLink(ResourceKey<Level> dim, BlockPos target) {
        String teleportCmd = buildDimTeleportCmd(dim, "@s", target);
        String text = buildBlockPosTeleportLinkText(target);
        MutableComponent textComp = Component.translatableWithFallback(text, text);
        MutableComponent hover = Component.translatableWithFallback("cli.msg.info.region.spatial.location.teleport.link.hover", "Teleport to block");
        return buildExecuteCmdComponent(textComp, hover, teleportCmd, RUN_COMMAND, TP_COLOR);
    }

    public static MutableComponent buildDimSuggestRegionRemovalLink(IMarkableRegion region) {
        String cmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), DELETE.toString(), region.getName());
        MutableComponent hover = Component.translatableWithFallback("cli.msg.info.dim.region.remove.link.hover", "Remove region %s", region.getName());
        MutableComponent text = Component.translatableWithFallback("cli.link.remove", "x");
        return buildExecuteCmdComponent(text, hover, cmd, SUGGEST_COMMAND, REMOVE_CMD_COLOR);
    }

    /* TODO: extract method for n component(s) [+] */
    public static MutableComponent buildDimFlagListLink(IProtectedRegion region) {
        String command = buildCommandStr(DIM.toString(), region.getDim().location().toString(), LIST.toString(), FLAG.toString());
        MutableComponent hoverLink = Component.translatableWithFallback("cli.msg.dim.flag.list.link.hover", "List flags in dimension %s", region.getDim().location().toString());
        MutableComponent linkText = Component.translatableWithFallback("cli.msg.flag.list.link.text", "%s flag(s)", region.getFlags().size());
        return region.getFlags().isEmpty()
                ? Component.translatableWithFallback("cli.msg.info.region.flag.link.text", "%s flag(s)", region.getFlags().size())
                : buildExecuteCmdComponent(linkText, hoverLink, command, RUN_COMMAND, LINK_COLOR);
    }


    public static List<MutableComponent> buildRemoveAffiliationEntries(IProtectedRegion region, List<String> affiliationNames, AffiliationType affiliationType, String affiliation, RegionType parentType) {
        return affiliationNames.stream().map(affiliate -> buildRemoveAffiliateEntry(region, affiliate, affiliationType, affiliation, parentType)).collect(Collectors.toList());
    }

    public static MutableComponent buildRemoveAffiliateEntry(IProtectedRegion region, String affiliateName, AffiliationType affiliationType, String affiliation, RegionType regionType) {
        MutableComponent linkText = Component.translatableWithFallback("cli.link.remove", "x");
        String fallback = "Remove "+ affiliationType.name+" '%s' from region %s";
        String key = "cli.msg.info.region.affiliation." + affiliationType.name + ".remove.link.hover";
        MutableComponent hoverText = Component.translatableWithFallback(key, fallback, affiliateName, region.getName());
        MutableComponent regionRemoveLink = switch (regionType) {
            case DIMENSION -> {
                String command = buildCommandStr(DIM.toString(), region.getDim().location().toString(), REMOVE.toString(), affiliationType.name, affiliation, affiliateName);
                yield buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, REMOVE_CMD_COLOR);
            }
            case LOCAL -> {
                String command = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), REMOVE.toString(), affiliationType.name, affiliation, affiliateName);
                yield buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, REMOVE_CMD_COLOR);
            }
            default -> throw new IllegalArgumentException();
        };
        return Component.literal(" - ")
                .append(regionRemoveLink).append(" ")
                .append(buildAffiliateInfo(region, affiliateName, affiliationType));
    }

    public static MutableComponent buildAffiliateInfo(IProtectedRegion region, String affiliateName, AffiliationType affiliationType) {
        return switch (affiliationType) {
            case PLAYER -> {
                Player player = ServerLifecycleHooks.getCurrentServer().getPlayerList().getPlayerByName(affiliateName);
                MutableComponent playerEntry = Component.translatableWithFallback("cli.msg.info.player.list.entry.offline", "(offline)");
                yield player == null
                        ? Component.literal(affiliateName).withStyle(GRAY).append(" ").append(playerEntry)
                        : buildPlayerHoverComponent(player);
            }
            case TEAM -> {
                ServerLevel level = ServerLifecycleHooks.getCurrentServer().getLevel(region.getDim());
                if (level != null) {
                    Team team = level.getScoreboard().getPlayerTeam(affiliateName);
                    yield team == null ? Component.literal(affiliateName) : buildTeamHoverComponent(team);
                } else {
                    yield Component.literal(affiliateName);
                }
            }
        };
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


    public static MutableComponent buildRegionRemoveChildLink(IProtectedRegion region, IProtectedRegion child) {
        String command = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), REMOVE.toString(), CHILD.toString(), child.getName());
        MutableComponent linkText = Component.translatableWithFallback("cli.link.remove", "x");
        MutableComponent linkHoverText = Component.translatableWithFallback("cli.msg.info.region.children.remove.link.hover", "Remove child '%s' from region %s", child.getName(), region.getName());
        return buildExecuteCmdComponent(linkText, linkHoverText, command, SUGGEST_COMMAND, REMOVE_CMD_COLOR);
    }

    public static MutableComponent buildRegionActionUndoLink(String cmd, CommandConstants toReplace, CommandConstants replacement) {
        String revertCmd = CommandUtil.revertCommand(cmd, toReplace, replacement);
        MutableComponent revertLinkText = Component.translatableWithFallback("cli.link.action.undo.text", "←");
        MutableComponent revertLinkHover = Component.translatableWithFallback("cli.link.action.undo.hover", "Undo action.");
        return buildExecuteCmdComponent(revertLinkText, revertLinkHover, revertCmd, RUN_COMMAND, DARK_RED);
    }

    public static MutableComponent buildRegionActionUndoLink(String cmd, String toReplace, String replacement) {
        String revertCmd = CommandUtil.revertCommand(cmd, toReplace, replacement);
        MutableComponent revertLinkText = Component.translatableWithFallback("cli.link.action.undo.text", "←");
        MutableComponent revertLinkHover = Component.translatableWithFallback("cli.link.action.undo.hover", "Undo action.");
        return buildExecuteCmdComponent(revertLinkText, revertLinkHover, revertCmd, RUN_COMMAND, DARK_RED);
    }

    public static MutableComponent buildFlagHeader(IProtectedRegion region, RegionType regionType) {
        return switch (regionType) {
            case DIMENSION ->
                    buildHeader(Component.translatableWithFallback("cli.msg.info.header.in", "== %s in %s ==", buildFlagListLink(region, regionType), buildRegionInfoLink(region, regionType)));
            case LOCAL ->
                    buildHeader(Component.translatableWithFallback("cli.msg.info.header.in", "== %s in %s ==", buildFlagListLink(region, regionType), buildRegionInfoLink(region, regionType)));
            default -> throw new IllegalStateException("Unexpected value: " + regionType);
        };
    }
}
