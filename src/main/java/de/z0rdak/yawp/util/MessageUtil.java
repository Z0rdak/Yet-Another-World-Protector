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
import net.minecraft.entity.EntityType;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.NbtHelper;
import net.minecraft.registry.RegistryKey;
import net.minecraft.scoreboard.Team;
import net.minecraft.server.command.ServerCommandSource;
import net.minecraft.text.*;
import net.minecraft.util.Formatting;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.apache.commons.lang3.NotImplementedException;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.config.server.RegionConfig.CLI_REGION_DEFAULT_PRIORITY_INC;
import static de.z0rdak.yawp.core.region.RegionType.LOCAL;
import static de.z0rdak.yawp.util.CommandUtil.buildCommandStr;
import static net.minecraft.text.ClickEvent.Action.RUN_COMMAND;
import static net.minecraft.text.ClickEvent.Action.SUGGEST_COMMAND;
import static net.minecraft.util.Formatting.RESET;
import static net.minecraft.util.Formatting.*;


public class MessageUtil {

    private MessageUtil() {
    }

    public static MutableText buildHeader(String translationKey) {
        return buildHeader(Text.translatable(translationKey));
    }

    public static MutableText buildHeader(String translationKey, String fallback) {
        return buildHeader(Text.translatableWithFallback(translationKey, fallback));
    }

    public static MutableText buildHeader(MutableText header) {
        return Text.literal(BOLD + "")
                .append(header)
                .append(Text.literal(BOLD + ""));
    }

    public static void sendCmdFeedback(ServerCommandSource src, MutableText text) {
        try {
            if (src.getEntity() == null) {
                src.sendFeedback(() -> text, true);
            } else {
                sendMessage(src.getPlayerOrThrow(), text);
            }
        } catch (CommandSyntaxException e) {
            YetAnotherWorldProtector.LOGGER.error(e);
        }
    }

    public static void sendCmdFeedback(ServerCommandSource src, String langKey) {
        sendCmdFeedback(src, Text.translatable(langKey));
    }

    public static void sendCmdFeedback(ServerCommandSource src, String langKey, String fallback) {
        sendCmdFeedback(src, Text.translatableWithFallback(langKey, fallback));
    }

    public static void sendMessage(PlayerEntity player, MutableText textComponent) {
        player.sendMessage(textComponent);
    }

    public static void sendDimFlagNotification(PlayerEntity player, RegionFlag flag) {
        player.sendMessage(Text.translatableWithFallback("flag.dim.player.msg.push.deny", "The '%s' flag denies this action in this dimension!", flag.name), true);
    }

    public static void sendFlagNotification(PlayerEntity player, IMarkableRegion region, RegionFlag flag) {
        player.sendMessage(Text.translatableWithFallback("flag.local.player.msg.push.deny", "[%s]: The '%s' flag denies this action here!", region.getName(), flag.name), true);
    }

    public final static Formatting SUGGEST_COLOR = BLUE;
    public final static Formatting TP_COLOR = GREEN;
    public final static Formatting LINK_COLOR = AQUA;
    public final static Formatting INACTIVE_LINK_COLOR = GRAY;
    public final static Formatting ADD_CMD_COLOR = DARK_GREEN;
    public final static Formatting REMOVE_CMD_COLOR = DARK_RED;
    public static int FIRST_PAGE_IDX = 0;

    public static String buildTeleportCmd(String tpSource, BlockPos target) {
        return "tp " + tpSource + " " + target.getX() + " " + target.getY() + " " + target.getZ();
    }

    public static String buildTeleportLinkText(RegistryKey<World> dim, BlockPos target) {
        return buildTeleportLinkText(dim.getValue().toString(), target);
    }

    public static String buildTeleportLinkText(String regionName, BlockPos target) {
        return regionName + " @ [" + buildBlockPosTeleportLinkText(target) + "]";
    }

    public static String buildBlockPosTeleportLinkText(BlockPos target) {
        return target.getX() + ", " + target.getY() + ", " + target.getZ();
    }


    public static String buildExecuteCommandString(RegistryKey<World> dim, String command) {
        return "/execute in " + dim.getValue() + " run " + command;
    }

    public static String buildDimTeleportCmd(RegistryKey<World> dim, String tpSource, BlockPos target) {
        return buildExecuteCommandString(dim, buildTeleportCmd(tpSource, target));
    }

    public static String buildRegionTpCmd(IMarkableRegion region, String target) {
        return buildDimTeleportCmd(region.getDim(), target, region.getTpTarget());
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
        MutableText playerName = Text.literal(player.getNameForScoreboard());
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
                .withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, Text.translatableWithFallback("cli.msg.info.region.affiliation.link.hover","Click to display team info")))
                .withClickEvent(new ClickEvent(RUN_COMMAND, "/team list " + team.getName())));
        return playerName;
    }

    public static MutableText buildBlockPosTpLinks(IMarkableRegion region) {
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
        MutableText areaInfo = Text.literal(area.getAreaType().areaType)
                .append("\n");
        switch (area.getAreaType()) {
            case CUBOID: {
                CuboidArea cuboidArea = (CuboidArea) area;
                MutableText sizeInfo = Text.translatableWithFallback("cli.msg.info.region.spatial.area.size", "Size")
                        .append(": ")
                        .append("X=" + cuboidArea.getXsize() + ", Y=" + cuboidArea.getYsize() + ", Z=" + cuboidArea.getZsize());
                MutableText markedBlocksInfo = Text.translatableWithFallback("cli.msg.info.region.spatial.area.blocks", "Marked Blocks")
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

    public static MutableText buildTextWithHoverMsg(MutableText text, MutableText hoverText, Formatting color) {
        MutableText bracketedText = Texts.bracketed(text);
        bracketedText.setStyle(bracketedText.getStyle().withColor(color).withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, hoverText)));
        return bracketedText;
    }

    public static MutableText buildDimensionTeleportLink(IMarkableRegion region) {
        String cmdLinkText = buildTeleportLinkText(region.getDim(), region.getTpTarget());
        String executeCmdStr = buildDimTeleportCmd(region.getDim(), "@s", region.getTpTarget());
        MutableText teleportCmdHoverText = Text.translatableWithFallback("cli.msg.info.region.spatial.location.teleport", "Teleport to region %s in dimension %s", region.getName(), region.getDim().getValue().toString());
        return buildExecuteCmdComponent(Text.literal(cmdLinkText), teleportCmdHoverText, executeCmdStr, RUN_COMMAND, TP_COLOR);
    }

    // Not used at the moment
    public static MutableText buildHelpSuggestionLink(String translationKey, CommandConstants baseCmd, CommandConstants cmd) {
        String command = "/" + CommandPermissionConfig.BASE_CMD + " " + baseCmd + " " + cmd + " ";
        MutableText text = Text.translatableWithFallback("=>", "=>");
        MutableText hover = Text.translatableWithFallback("not.existing.key", "place-text-here");
        return Text.literal(" ")
                .append(buildExecuteCmdComponent(text, hover, command, SUGGEST_COMMAND, SUGGEST_COLOR))
                .append(Text.literal(" "))
                .append(Text.translatable(translationKey));
    }

    public static String buildDimCmdStr(IProtectedRegion region, CommandConstants constant) {
        return CommandUtil.buildCommandStr(DIM.toString(), region.getDim().getValue().toString(), constant.toString());
    }

    public static String buildRegionCmdStr(IProtectedRegion region, CommandConstants constant) {
        return CommandUtil.buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getName(), constant.toString());
    }

    public static MutableText buildRegionEnableComponent(IMarkableRegion region) {
        String cmd = CommandUtil.buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getName(), STATE.toString(), ENABLE.toString());
        MutableText text = Text.translatableWithFallback("cli.msg.info.region.state.enable." + region.isActive() + ".link.text", region.isActive() ? "yes" : "no");
        MutableText hover = Text.translatableWithFallback("cli.msg.info.region.state.enable." + !region.isActive() + ".link.hover",
                !region.isActive() ? "Disable flag checks" : "Enable flag checks");
        Formatting color = region.isActive() ? ADD_CMD_COLOR : REMOVE_CMD_COLOR;
        return buildExecuteCmdComponent(text, hover, cmd, ClickEvent.Action.RUN_COMMAND, color);
    }

    public static MutableText buildRegionPriorityComponent(IMarkableRegion region) {
        String incPriorityCmd = CommandUtil.buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getName(), STATE.toString(), PRIORITY.toString(), INC.toString(), String.valueOf(CLI_REGION_DEFAULT_PRIORITY_INC.get()));
        MutableText incLinkText = Text.translatableWithFallback("cli.msg.info.region.state.priority.increase.link.text", "+%s", CLI_REGION_DEFAULT_PRIORITY_INC.get());
        MutableText incHoverText = Text.translatableWithFallback("cli.msg.info.region.state.priority.increase.link.hover", "Increase region priority by %s", CLI_REGION_DEFAULT_PRIORITY_INC.get());
        MutableText increaseLink = buildExecuteCmdComponent(incLinkText, incHoverText, incPriorityCmd, ClickEvent.Action.RUN_COMMAND, ADD_CMD_COLOR);
        String decPriorityCmd = CommandUtil.buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getName(), STATE.toString(), PRIORITY.toString(), DEC.toString(), String.valueOf(CLI_REGION_DEFAULT_PRIORITY_INC.get()));
        MutableText decLinkText = Text.translatableWithFallback("cli.msg.info.region.state.priority.decrease.link.text", "-%s", CLI_REGION_DEFAULT_PRIORITY_INC.get());
        MutableText decHoverText = Text.translatableWithFallback("cli.msg.info.region.state.priority.decrease.link.hover", "Decrease region priority by %s", CLI_REGION_DEFAULT_PRIORITY_INC.get());
        MutableText decreaseLink = buildExecuteCmdComponent(decLinkText, decHoverText, decPriorityCmd, ClickEvent.Action.RUN_COMMAND, REMOVE_CMD_COLOR);
        MutableText priorityValue = Text.literal(String.valueOf(region.getPriority()));
        String setPriorityCmd = CommandUtil.buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getName(), STATE.toString(), PRIORITY.toString(), "");
        MutableText setPriorityLinkText = Text.translatableWithFallback("cli.msg.info.region.state.priority.set.link.text", "#");
        MutableText setPriorityHoverText = Text.translatableWithFallback("cli.msg.info.region.state.priority.set.link.hover", "Set priority for region");
        MutableText setPriorityLink = buildExecuteCmdComponent(setPriorityLinkText, setPriorityHoverText, setPriorityCmd, SUGGEST_COMMAND, SUGGEST_COLOR);
        return priorityValue.append(" ")
                .append(setPriorityLink).append(" ")
                .append(increaseLink).append(" ")
                .append(decreaseLink);
    }

    public static MutableText buildRegionAlertComponentLink(IMarkableRegion region) {
        String cmd = CommandUtil.buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getName(), STATE.toString(), ALERT.toString());
        MutableText text = Text.translatableWithFallback("cli.msg.info.region.state.alert." + !region.isMuted() + ".link.text",
                !region.isMuted() ? "off" : "on");
        MutableText hover = Text.translatableWithFallback( "cli.msg.info.region.state.alert." + region.isMuted() + ".link.hover",
                "Turn flag alerts "+ (region.isMuted()? "on" : "off"));
        Formatting color = region.isMuted() ? REMOVE_CMD_COLOR : ADD_CMD_COLOR;
        return buildExecuteCmdComponent(text, hover, cmd, ClickEvent.Action.RUN_COMMAND, color);
    }

    public static MutableText buildRegionInfoLink(IProtectedRegion region, RegionType type) {
        return switch (type) {
            case DIMENSION -> {
                String command = buildCommandStr(DIM.toString(), region.getDim().getValue().toString(), INFO.toString());
                MutableText hoverText = Text.translatableWithFallback("cli.msg.dim.info", "Prompt region info");
                MutableText linkText = Text.literal(region.getDim().getValue().toString());
                yield buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, LINK_COLOR);
            }
            case LOCAL -> {
                String cmd = buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getName(), INFO.toString());
                MutableText regionInfoLinkText = Text.literal(region.getName());
                MutableText regionInfoLinkHover = Text.translatableWithFallback("cli.msg.info.region", "Show region info for region %s", region.getName());
                yield buildExecuteCmdComponent(regionInfoLinkText, regionInfoLinkHover, cmd, RUN_COMMAND, LINK_COLOR);
            }
            default -> throw new IllegalStateException("Unexpected value: " + type);
        };
    }

    public static MutableText buildRegionSpatialPropLink(IMarkableRegion region) {
        String showSpatialPropLink = buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getName(), SPATIAL.toString());
        MutableText spatialPropLinkText = Text.translatableWithFallback("cli.msg.info.region.spatial.link.text", "Spatial Properties");
        MutableText spatialPropHoverText = Text.translatableWithFallback("cli.msg.info.region.spatial.link.hover", "Show region spatial properties for %s", region.getName());
        return buildExecuteCmdComponent(spatialPropLinkText, spatialPropHoverText, showSpatialPropLink, RUN_COMMAND, LINK_COLOR);
    }

    public static MutableText buildRegionOverviewHeader(IProtectedRegion region, RegionType type) {
        return switch (type) {
            case DIMENSION -> {
                MutableText dumpLinkText = Text.translatableWithFallback("cli.msg.dim.overview.header.dump.link.text", "Dimension overview");
                MutableText dumpLinkHover = Text.translatableWithFallback("cli.msg.dim.overview.header.dump.link.hover","Copy Dimensional Region NBT to clipboard");
                MutableText clipBoardDumpLink = buildExecuteCmdComponent(dumpLinkText, dumpLinkHover, NbtHelper.toPrettyPrintedText(region.serializeNBT()).getString(), ClickEvent.Action.COPY_TO_CLIPBOARD, GOLD);
                yield buildHeader(Text.translatableWithFallback("cli.msg.info.header.for", "== %s for %s ==", clipBoardDumpLink, buildRegionInfoLink(region, RegionType.DIMENSION)));
            }
            case LOCAL -> {
                MutableText dumpLinkText = Text.translatableWithFallback("cli.msg.info.region.overview.dump.link.text","Region overview");
                MutableText dumpLinkHover = Text.translatableWithFallback("cli.msg.info.region.overview.dump.link.hover","Copy Region NBT to clipboard");
                MutableText clipBoardDumpLink = buildExecuteCmdComponent(dumpLinkText, dumpLinkHover, NbtHelper.toPrettyPrintedText(region.serializeNBT()).getString(), ClickEvent.Action.COPY_TO_CLIPBOARD, GOLD);
                yield buildHeader(Text.translatableWithFallback("cli.msg.info.header.for",  "== %s for %s ==", clipBoardDumpLink, buildRegionInfoLink(region, LOCAL)));
            }
            default -> throw new IllegalStateException("Unexpected value: " + type);
        };
    }

    /**
     * Players: [n player(s)] [+]
     * // TODO:
     */
    public static MutableText buildPlayerListLink(IProtectedRegion region, PlayerContainer players, String affiliation, RegionType regionType) {
        MutableText hoverText = Text.translatableWithFallback("cli.msg.info.region.affiliation.player.list.link.hover", "List players of affiliation '%s' in region %s", affiliation, region.getName());
        MutableText linkText = Text.translatableWithFallback("cli.msg.info.region.affiliation.player.list.link.text", "%s player(s)", players.getPlayers().size());
        return switch (regionType) {
            case DIMENSION -> {
                String cmd = buildCommandStr(DIM.toString(), region.getDim().getValue().toString(), LIST.toString(), affiliation, PLAYER.toString());
                yield buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            case LOCAL -> {
                String cmd = buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getName(), LIST.toString(), affiliation, PLAYER.toString());
                yield buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            default -> throw new IllegalArgumentException();
        };
    }

    /**
     * Teams: [n team(s)] [+]
     */
    public static MutableText buildTeamListLink(IProtectedRegion region, PlayerContainer teams, String affiliation, RegionType regionType) {
        MutableText hoverText = Text.translatableWithFallback("cli.msg.info.region.affiliation.team.list.link.hover", "List teams of affiliation '%s' in region %s", affiliation, region.getName());
        MutableText linkText = Text.translatableWithFallback("cli.msg.info.region.affiliation.team.list.link.text", "%s team(s)", teams.getTeams().size());
        return switch (regionType) {
            case DIMENSION -> {
                String cmd = buildCommandStr(DIM.toString(), region.getDim().getValue().toString(), LIST.toString(), affiliation, TEAM.toString());
                yield buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            case LOCAL -> {
                String cmd = buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getName(), LIST.toString(), affiliation, TEAM.toString());
                yield buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            default -> throw new IllegalArgumentException();
        };
    }

    public static MutableText buildAddAffiliateLink(IProtectedRegion region, String affiliation, AffiliationType affiliationType, RegionType regionType) {
        MutableText linkText = Text.translatableWithFallback("cli.link.add","+");
        String affiliationFallback = "Add "+ affiliationType.name+" as '%s' to region %s";
        MutableText hoverText = Text.translatableWithFallback("cli.msg.info.region.affiliation." + affiliationType.name + ".add.link.hover", affiliationFallback, affiliation, region.getName());
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

    public static MutableText buildAffiliationLinks(IProtectedRegion region, RegionType regionType) {
        MutableText affiliationLinks = Text.literal("");
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

    public static MutableText buildAffiliationHeader(IProtectedRegion region, String affiliation, RegionType regionType) {
        int amountOwners = (region.getOwners().getTeams().size() + region.getOwners().getPlayers().size());
        int amountMembers = (region.getMembers().getTeams().size() + region.getMembers().getPlayers().size());
        int affiliationSize = affiliation.equals("owner") ? amountOwners : amountMembers;
        MutableText affiliationLink = buildAffiliationLink(region, affiliation, affiliationSize, regionType);
        return buildHeader(Text.translatableWithFallback("cli.msg.info.header.in", "== %s in %s ==", affiliationLink, buildRegionInfoLink(region, regionType)));
    }

    public static MutableText buildAffiliationHeader(IProtectedRegion region, String affiliation, AffiliationType affiliationType, RegionType regionType) {
        String fallbackString = "== Region '%s' "+ affiliationType.name+"s in %s ==";
        return Text.translatableWithFallback("cli.msg.info.region.affiliation." + affiliationType.name + ".list", fallbackString, buildRegionInfoLink(region, regionType), affiliation);
    }

    public static MutableText buildAffiliationLink(IProtectedRegion region, String affiliation, int affiliationSize, RegionType regionType) {
        MutableText linkText = Text.translatableWithFallback("cli.msg.info.region.affiliation.list.link.text", "%s %s(s)", affiliationSize, affiliation);
        MutableText hoverText = Text.translatableWithFallback("cli.msg.info.region.affiliation.list.link.hover", "List %ss for region %s", affiliation, region.getName());
        return switch (regionType) {
            case LOCAL -> {
                String cmd = buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getName(), LIST.toString(), affiliation);
                yield buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            case DIMENSION -> {
                String cmd = buildCommandStr(DIM.toString(), region.getDim().getValue().toString(), LIST.toString(), affiliation);
                yield buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
            }
            default -> throw new IllegalArgumentException();
        };
    }

    // TODO: combine team and player list link
    public static MutableText buildAffiliationTeamListLink(IProtectedRegion region, String affiliation, RegionType regionType) {
        // Teams: [n team(s)] [+]
        PlayerContainer playerContainer = affiliation.equals("owner") ? region.getOwners() : region.getMembers();
        MutableText teams = Text.translatableWithFallback("cli.msg.info.region.affiliation.team", "Teams").append(": ");
        MutableText teamAddLink = buildAddAffiliateLink(region, affiliation, AffiliationType.TEAM, regionType);
        MutableText teamListLink = playerContainer.hasTeams()
                ? buildTeamListLink(region, playerContainer, affiliation, regionType)
                : Text.translatableWithFallback("cli.msg.info.region.affiliation.team.list.link.text", "%s team(s)", playerContainer.getTeams().size());
        teams.append(teamListLink).append(teamAddLink);
        return teams;
    }

    // TODO: combine team and player list link
    public static MutableText buildAffiliationPlayerListLink(IProtectedRegion region, String affiliation, RegionType regionType) {
        // Players: [n player(s)] [+]
        PlayerContainer playerContainer = affiliation.equals("owner") ? region.getOwners() : region.getMembers();
        MutableText players = Text.translatableWithFallback("cli.msg.info.region.affiliation.player", "Players").append(": ");
        MutableText playersAddLink = buildAddAffiliateLink(region, affiliation, AffiliationType.PLAYER, regionType);
        MutableText playerListLink = playerContainer.hasPlayers()
                ? buildPlayerListLink(region, playerContainer, affiliation, regionType)
                : Text.translatableWithFallback("cli.msg.info.region.affiliation.player.list.link.text", "%s player(s)", playerContainer.getPlayers().size());
        players.append(playerListLink).append(playersAddLink);
        return players;
    }

    private static MutableText buildRemoveFlagEntry(IProtectedRegion region, IFlag flag, RegionType regionType) {
        MutableText flagRemoveEntry = Text.literal(" - ");
        MutableText flagRemoveLink;
        switch (regionType) {
            case DIMENSION: {
                String command = buildCommandStr(DIM.toString(), region.getDim().getValue().toString(), REMOVE.toString(), FLAG.toString(), flag.getFlagIdentifier());
                MutableText hoverText = Text.translatableWithFallback("cli.msg.dim.info.flag.remove.link.hover", "Remove flag '%s' from dimension %s", flag.getFlagIdentifier(), region.getDim().getValue().toString());
                MutableText linkText = Text.translatableWithFallback("cli.link.remove", "x");
                flagRemoveLink = buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, REMOVE_CMD_COLOR);
                break;
            }
            case LOCAL: {
                String cmd = buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getName(), REMOVE.toString(), FLAG.toString(), flag.getFlagIdentifier());
                MutableText hoverText = Text.translatableWithFallback("cli.msg.info.region.flag.remove.link.hover", "Remove flag '%s' from region %s", flag.getFlagIdentifier(), region.getName());
                MutableText linkText = Text.translatableWithFallback("cli.link.remove", "x");
                flagRemoveLink = buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, REMOVE_CMD_COLOR);
                break;
            }
            default:
                throw new IllegalArgumentException();
        }
        return flagRemoveEntry.append(flagRemoveLink).append(" ").append(buildFlagQuickInfo(flag));
    }

    // TODO: Add command to toggle negated and active state, and add link here as well
    public static MutableText buildFlagQuickInfo(IFlag flag) {
        return switch (flag.getFlagType()) {
            case BOOLEAN_FLAG -> {
                BooleanFlag boolFlag = (BooleanFlag) flag;
                MutableText flagName = Text.literal(boolFlag.getFlagIdentifier());
                MutableText flagInfo = Text.literal("Flag state: Active=" + boolFlag.isActive() + ", negated=" + boolFlag.isInverted());
                yield buildTextWithHoverMsg(flagName, flagInfo, ITALIC);
            }
            default -> throw new IllegalStateException("Unexpected value: " + flag.getFlagType());
        };
    }

    public static MutableText buildFlagCmdInfoLink(IProtectedRegion region, RegionType regionType, IFlag iflag) {
        switch (regionType) {
            case DIMENSION: {
                String cmd = buildCommandStr(FLAG.toString(), DIM.toString(), region.getDim().getValue().toString());
            }
            break;
            case LOCAL: {
            }
            break;
        }
        return Text.literal(iflag.getFlagIdentifier());
    }

    public static List<MutableText> buildRemoveFlagEntries(IProtectedRegion region, List<IFlag> flags, RegionType regionType) {
        return flags.stream().map(flag -> buildRemoveFlagEntry(region, flag, regionType)).collect(Collectors.toList());
    }

    public static List<MutableText> buildRemoveRegionEntries(IProtectedRegion parent, List<IMarkableRegion> regions, RegionType parentType) {
        return regions.stream().map(region -> buildRemoveRegionEntry(parent, region, parentType)).collect(Collectors.toList());
    }

    public static MutableText buildRemoveRegionEntry(IProtectedRegion parent, IMarkableRegion region, RegionType parentType) {
        Style resetStyle = Style.EMPTY.withColor(WHITE).withHoverEvent(null).withClickEvent(null);
        MutableText separator = Text.literal(" ").setStyle(resetStyle);
        MutableText regionRemoveLink = switch (parentType) {
            case DIMENSION -> {
                MutableText removeLink = buildDimSuggestRegionRemovalLink(region);
                removeLink.append(separator).append(buildRegionInfoLink(region, LOCAL));
                MutableText childIndicator = buildTextWithHoverMsg(Text.literal("*"),
                        Text.translatableWithFallback("cli.msg.info.dim.region.child.hover", "Marks this region as a direct child region"), GOLD);
                if (parent.hasChild(region)) {
                    removeLink.append(childIndicator.setStyle(childIndicator.getStyle().withInsertion("Test")));
                }
                removeLink.append(Text.literal(" @ ").setStyle(resetStyle)).append(buildRegionTeleportLink(region));
                yield removeLink;
            }
            case LOCAL ->
                    buildRegionRemoveChildLink(parent, region).append(separator).append(buildRegionInfoLink(region, LOCAL));
            default -> throw new IllegalArgumentException();
        };
        return Text.literal(" - ").append(regionRemoveLink);
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
            paginationComponents.add(Text.translatableWithFallback("cli.msg.info.pagination.error.index", "Invalid page index supplied: %s (Try 0-%s)",pageNo, numberOfPages - 1).formatted(RED));
            return paginationComponents;
        }
        boolean hasMultiplePages = numberOfPages > 1;

        MutableText firstText = Text.translatableWithFallback("cli.msg.info.pagination.first.text", "⏮");
        MutableText firstHover = Text.translatableWithFallback("cli.msg.info.pagination.first.hover", "First page");
        MutableText prevText = Text.translatableWithFallback("cli.msg.info.pagination.previous.text", "◀");
        MutableText prevHover = Text.translatableWithFallback("cli.msg.info.pagination.previous.hover", "Previous page");
        MutableText nextText = Text.translatableWithFallback("cli.msg.info.pagination.next.text", "▶");
        MutableText nextHover = Text.translatableWithFallback("cli.msg.info.pagination.next.hover", "Next page");
        MutableText lastText = Text.translatableWithFallback("cli.msg.info.pagination.last.text", "⏭");
        MutableText lastHover = Text.translatableWithFallback("cli.msg.info.pagination.last.hover", "Last page");
        MutableText first = hasMultiplePages && pageNo != FIRST_PAGE_IDX
                ? buildExecuteCmdComponent(firstText, firstHover, buildPageCommand(cmd, FIRST_PAGE_IDX), RUN_COMMAND, LINK_COLOR)
                : Texts.bracketed(firstText).formatted(INACTIVE_LINK_COLOR);
        MutableText prev = hasMultiplePages && pageNo > FIRST_PAGE_IDX
                ? buildExecuteCmdComponent(prevText, prevHover, buildPageCommand(cmd, Math.max(pageNo - 1, FIRST_PAGE_IDX)), RUN_COMMAND, LINK_COLOR)
                : Texts.bracketed(prevText).formatted(INACTIVE_LINK_COLOR);
        MutableText next = hasMultiplePages && pageNo < numberOfPages - 1
                ? buildExecuteCmdComponent(nextText, nextHover, buildPageCommand(cmd, Math.min(pageNo + 1, numberOfPages - 1)), RUN_COMMAND, LINK_COLOR)
                : Texts.bracketed(nextText).formatted(INACTIVE_LINK_COLOR);
        MutableText last = hasMultiplePages && pageNo < numberOfPages - 1
                ? buildExecuteCmdComponent(lastText, lastHover, buildPageCommand(cmd, numberOfPages - 1), RUN_COMMAND, LINK_COLOR)
                : Texts.bracketed(lastText).formatted(INACTIVE_LINK_COLOR);

        MutableText paginationControl = buildPaginationControl(first, prev, pageNo, numberOfPages, next, last);
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

    public static MutableText buildPaginationControl(MutableText front, MutableText back, int pageNo, int maxPage, MutableText forward, MutableText last) {
        // [<<]  [<]  x/n  [>]  [>>]
        MutableText pageIndicator = Text.literal((pageNo + 1) + "/" + (maxPage));
        pageIndicator.setStyle(pageIndicator.getStyle().withColor(RESET).withHoverEvent(null).withClickEvent(null));
        MutableText resetSpace = Text.literal("  ");
        resetSpace.setStyle(resetSpace.getStyle().withColor(RESET).withHoverEvent(null).withClickEvent(null));
        return Text.literal(" ")
                .append(front).append(resetSpace)
                .append(back).append(resetSpace)
                .append(pageIndicator).append(resetSpace)
                .append(forward).append(resetSpace)
                .append(last).append(resetSpace);
    }

    public static MutableText buildRegionChildrenHeader(IProtectedRegion region, RegionType type) {
        return buildHeader(Text.translatableWithFallback("cli.msg.info.header.in", "== %s in %s ==",
                buildRegionChildrenLink(region, type), buildRegionInfoLink(region, type)));
    }

    public static MutableText buildRegionParentLink(IMarkableRegion region) {
        MutableText parentLink = null;
        if (region.getParent() != null) { // FIXME: should not happen. it is either a dim or local region as parent
            String regionParentInfoCmd = buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getParent().getName(), INFO.toString());
            MutableText parentLinkText = Text.translatableWithFallback("cli.msg.info.region.parent.link.text", "%s", region.getParent().getName());
            MutableText parentHoverText = Text.translatableWithFallback("cli.msg.info.region.parent.link.hover",  "Show info about regions parent '%s'", region.getParent().getName());
            if (region.getParent() instanceof DimensionalRegion) {
                return buildRegionInfoLink(region.getParent(), RegionType.DIMENSION);
            }
            if (region.getParent() instanceof IMarkableRegion) {
                String clearRegionParentCmd = buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getName(), PARENT.toString(), CLEAR.toString(), region.getParent().getName());
                MutableText parentClearLinkText = Text.translatableWithFallback("cli.msg.info.region.parent.clear.link.text", "-");
                MutableText parentClearHoverText = Text.translatableWithFallback("cli.msg.info.region.parent.clear.link.hover", "Clear %s as parent region", region.getParent().getName());
                parentLink = buildExecuteCmdComponent(parentLinkText, parentHoverText, regionParentInfoCmd, RUN_COMMAND, LINK_COLOR)
                        .append(buildExecuteCmdComponent(parentClearLinkText, parentClearHoverText, clearRegionParentCmd, RUN_COMMAND, REMOVE_CMD_COLOR));
                return parentLink;
            }
            if (region.getParent() instanceof GlobalRegion) { // FIXME: Not needed here
                // TODO: Hierarchy Info for dimensional Regions
            }

        } else {
            String setRegionParentCmd = buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getName(), PARENT.toString(), SET.toString(), "");
            MutableText setParentLinkText = Text.translatableWithFallback("cli.link.add", "+");
            MutableText setParentHoverText = Text.translatableWithFallback("cli.msg.info.region.parent.set.link.hover", "Set parent for region %s", region.getName());
            parentLink = Text.translatableWithFallback("cli.msg.info.region.parent.null", "No parent set")
                    .append(" ")
                    .append(buildExecuteCmdComponent(setParentLinkText, setParentHoverText, setRegionParentCmd, RUN_COMMAND, GREEN));
        }
        return parentLink;
    }

    public static MutableText buildDimRegionListHeader(DimensionalRegion dimRegion) {
        return buildHeader(Text.translatableWithFallback("cli.msg.info.header.in", "== %s in %s ==",
                buildRegionChildrenLink(dimRegion, RegionType.DIMENSION), buildRegionInfoLink(dimRegion, RegionType.DIMENSION)));
    }

    public static MutableText buildRegionChildrenLink(IProtectedRegion region, RegionType type) {
        return switch (type) {
            case DIMENSION -> {
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(region.getDim());
                String command = buildCommandStr(DIM.toString(), region.getDim().getValue().toString(), LIST.toString(), REGION.toString());
                MutableText listDimRegionsLinkText = Text.translatableWithFallback("cli.msg.dim.info.region.list.link.text", "%s region(s)",dimCache.getRegions().size());
                MutableText listDimRegionsHoverText = Text.translatableWithFallback("cli.msg.dim.info.region.list.link.hover", "List regions in %s", region.getName());
                MutableText listDimRegionsListLink = buildExecuteCmdComponent(listDimRegionsLinkText, listDimRegionsHoverText, command, RUN_COMMAND, LINK_COLOR);
                MutableText createRegionLink = buildDimCreateRegionLink(region);
                yield (region.getChildren().size() == 0) ? listDimRegionsLinkText.append(createRegionLink) : listDimRegionsListLink.append(createRegionLink);
            }
            case LOCAL -> {
                String regionChildrenListLink = buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getName(), LIST.toString(), CHILDREN.toString());
                MutableText childrenLinkText = Text.translatableWithFallback("cli.msg.info.region.children.link.text", "%s children", region.getChildren().size());
                MutableText childrenHoverText = Text.translatableWithFallback("cli.msg.info.region.children.link.hover", "Show list of children for region %s", region.getName());
                MutableText regionChildrenLink = buildExecuteCmdComponent(childrenLinkText, childrenHoverText, regionChildrenListLink, RUN_COMMAND, LINK_COLOR);
                MutableText addChildrenLink = buildRegionAddChildrenLink(region);
                yield (region.getChildren().size() == 0) ? childrenLinkText.append(addChildrenLink) : regionChildrenLink.append(addChildrenLink);
            }
            default -> throw new IllegalStateException("Unexpected value: " + type);
        };
    }

    public static MutableText buildRegionAddChildrenLink(IProtectedRegion region) {
        String addChildrenCmd = buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getName(), ADD.toString(), CHILD.toString(), "");
        MutableText addChildrenLinkText = Text.translatableWithFallback("cli.link.add", "+");
        MutableText addChildrenHoverText = Text.translatableWithFallback("cli.msg.info.region.children.add.link.hover", "Add child for region %s", region.getName());
        return buildExecuteCmdComponent(addChildrenLinkText, addChildrenHoverText, addChildrenCmd, SUGGEST_COMMAND, ADD_CMD_COLOR);
    }

    public static MutableText buildDimCreateRegionLink(IProtectedRegion region) {
        String dimCreateRegionCmd = buildCommandStr(DIM.toString(), region.getDim().getValue().toString(), CREATE.toString(), REGION.toString(), "");
        MutableText createRegionLinkText = Text.translatableWithFallback("cli.link.add", "+");
        MutableText createRegionHoverText = Text.translatableWithFallback("cli.msg.dim.info.region.create.link.hover", "Create region in dimension %s", region.getName());
        return buildExecuteCmdComponent(createRegionLinkText, createRegionHoverText, dimCreateRegionCmd, SUGGEST_COMMAND, ADD_CMD_COLOR);
    }

    public static MutableText buildFlagListLink(IProtectedRegion region, RegionType type) {
        MutableText flagLink = switch (type) {
            case DIMENSION -> {
                String flagListCmd = buildCommandStr(DIM.toString(), region.getDim().getValue().toString(), LIST.toString(), FLAG.toString());
                MutableText flagListLinkText = Text.translatableWithFallback("cli.msg.info.region.flag.link.text", "%s flag(s)", region.getFlags().size());
                MutableText flagListHoverText = Text.translatableWithFallback("cli.msg.dim.flag.list.link.hover", "Show region flags for region %s", region.getName());
                MutableText dimFlagListLink = buildExecuteCmdComponent(flagListLinkText, flagListHoverText, flagListCmd, RUN_COMMAND, LINK_COLOR);
                yield dimFlagListLink.append(buildDimAddFlagLink(region));
            }
            case LOCAL -> {
                String listCmd = buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getName(), LIST.toString(), FLAG.toString());
                MutableText flagListLinkText = Text.translatableWithFallback("cli.msg.info.region.flag.link.text", "%s flag(s)", region.getFlags().size());
                MutableText flagListHoverText = Text.translatableWithFallback("cli.msg.info.region.flag.link.hover", "Show region flags for region %s", region.getName());
                MutableText flagListLink = buildExecuteCmdComponent(flagListLinkText, flagListHoverText, listCmd, RUN_COMMAND, LINK_COLOR);
                yield flagListLink.append(buildRegionAddFlagLink(region));
            }
            default -> throw new IllegalStateException("Unexpected value: " + type);
        };
        return flagLink;
    }

    public static MutableText buildDimAddFlagLink(IProtectedRegion dimRegion) {
        String command = buildCommandStr(DIM.toString(), dimRegion.getDim().getValue().toString(), ADD.toString(), FLAG.toString(), "");
        MutableText hoverText = Text.translatableWithFallback("cli.msg.dim.flag.add.link.hover", "Add new flag to dimension %s", dimRegion.getDim().getValue().toString());
        MutableText linkText = Text.translatableWithFallback("cli.link.add", "+");
        return buildExecuteCmdComponent(linkText, hoverText, command, SUGGEST_COMMAND, ADD_CMD_COLOR);
    }

    public static MutableText buildRegionAddFlagLink(IProtectedRegion region) {
        String addCmd = buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getName(), ADD.toString(), FLAG.toString(), "");
        MutableText flagAddHoverText = Text.translatableWithFallback("cli.msg.info.region.flag.add.link.hover", "Add new flag to region %s", region.getName());
        MutableText flagAddLinkText = Text.translatableWithFallback("cli.link.add", "+");
        MutableText addFlag = buildExecuteCmdComponent(flagAddLinkText, flagAddHoverText, addCmd, SUGGEST_COMMAND, ADD_CMD_COLOR);
        return addFlag;
    }

    public static MutableText buildRegionStateLink(IMarkableRegion region) {
        String showStateCmd = buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getName(), STATE.toString());
        MutableText stateLinkText = Text.translatableWithFallback("cli.msg.info.region.state.link.text", "State");
        MutableText stateHoverText = Text.translatableWithFallback("cli.msg.info.region.state.link.hover", "Show region state for %s", region.getName());
        return buildExecuteCmdComponent(stateLinkText, stateHoverText, showStateCmd, RUN_COMMAND, LINK_COLOR);
    }

    public static MutableText buildStateLink(IProtectedRegion region) {
        String command = CommandUtil.buildCommandStr(DIM.toString(), region.getDim().getValue().toString(), ENABLE.toString());
        String onClickAction = region.isActive() ? "deactivate" : "activate";
        Formatting color = region.isActive() ? ADD_CMD_COLOR : REMOVE_CMD_COLOR;
        MutableText text = Text.translatableWithFallback("cli.msg.info.state." + onClickAction,
                region.isActive() ? "active" : "inactive");
        MutableText hover = Text.translatableWithFallback("cli.msg.info.state.link." + (region.isActive() ? "activate" : "deactivate"),
                region.isActive() ? "Deactivate region" : "Activate region");
        MutableText stateLink = buildExecuteCmdComponent(text, hover, command, ClickEvent.Action.RUN_COMMAND, color);
        return Text.translatableWithFallback("cli.msg.info.state", "State")
                .append(Text.literal(": "))
                .append(stateLink);
    }

    public static MutableText buildInfoComponent(String subjectLangKey, String fallback, MutableText payload) {
        return Text.translatableWithFallback(subjectLangKey, fallback).append(": ").append(payload);
    }

    public static MutableText buildRegionTeleportLink(IMarkableRegion region) {
        String teleportCmd = buildDimTeleportCmd(region.getDim(), "@s", region.getTpTarget());
        String text = buildBlockPosTeleportLinkText(region.getTpTarget());
        MutableText textComp = Text.translatableWithFallback(text, text);
        MutableText hover = Text.translatableWithFallback("cli.msg.region.info.tp.link.hover", "Teleport to region");

        return buildExecuteCmdComponent(textComp, hover, teleportCmd, RUN_COMMAND, TP_COLOR);
    }

    public static MutableText buildDimensionalBlockTpLink(RegistryKey<World> dim, BlockPos target) {
        String teleportCmd = buildDimTeleportCmd(dim, "@s", target);
        String text = buildBlockPosTeleportLinkText(target);
        MutableText textComp = Text.translatableWithFallback(text, text);
        MutableText hover = Text.translatableWithFallback("cli.msg.info.region.spatial.location.teleport.link.hover", "Teleport to block");
        return buildExecuteCmdComponent(textComp, hover, teleportCmd, RUN_COMMAND, TP_COLOR);
    }

    public static MutableText buildDimSuggestRegionRemovalLink(IMarkableRegion region) {
        String cmd = buildCommandStr(DIM.toString(), region.getDim().getValue().toString(), DELETE.toString(), region.getName());
        MutableText hover = Text.translatableWithFallback("cli.msg.info.dim.region.remove.link.hover", "Remove region %s", region.getName());
        MutableText text = Text.translatableWithFallback("cli.link.remove", "x");
        return buildExecuteCmdComponent(text, hover, cmd, SUGGEST_COMMAND, REMOVE_CMD_COLOR);
    }

    /* TODO: extract method for n component(s) [+] */
    public static MutableText buildDimFlagListLink(IProtectedRegion region) {
        String command = buildCommandStr(DIM.toString(), region.getDim().getValue().toString(), LIST.toString(), FLAG.toString());
        MutableText hoverLink = Text.translatableWithFallback("cli.msg.dim.flag.list.link.hover", "List flags in dimension %s", region.getDim().getValue().toString());
        MutableText linkText = Text.translatableWithFallback("cli.msg.flag.list.link.text", "%s flag(s)", region.getFlags().size());
        return region.getFlags().isEmpty()
                ? Text.translatableWithFallback("cli.msg.info.region.flag.link.text", "%s flag(s)", region.getFlags().size())
                : buildExecuteCmdComponent(linkText, hoverLink, command, RUN_COMMAND, LINK_COLOR);
    }


    public static List<MutableText> buildRemoveAffiliationEntries(IProtectedRegion region, List<String> affiliationNames, AffiliationType affiliationType, String affiliation, RegionType parentType) {
        return affiliationNames.stream().map(affiliate -> buildRemoveAffiliateEntry(region, affiliate, affiliationType, affiliation, parentType)).collect(Collectors.toList());
    }

    public static MutableText buildRemoveAffiliateEntry(IProtectedRegion region, String affiliateName, AffiliationType affiliationType, String affiliation, RegionType regionType) {
        MutableText linkText = Text.translatableWithFallback("cli.link.remove", "x");
        String fallback = "Remove "+ affiliationType.name+" '%s' from region %s";
        String key = "cli.msg.info.region.affiliation." + affiliationType.name + ".remove.link.hover";
        MutableText hoverText = Text.translatableWithFallback(key, fallback, affiliateName, region.getName());
        MutableText regionRemoveLink = switch (regionType) {
            case DIMENSION -> {
                String command = buildCommandStr(DIM.toString(), region.getDim().getValue().toString(), REMOVE.toString(), affiliationType.name, affiliation, affiliateName);
                yield buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, REMOVE_CMD_COLOR);
            }
            case LOCAL -> {
                String command = buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getName(), REMOVE.toString(), affiliationType.name, affiliation, affiliateName);
                yield buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, REMOVE_CMD_COLOR);
            }
            default -> throw new IllegalArgumentException();
        };
        return Text.literal(" - ")
                .append(regionRemoveLink).append(" ")
                .append(buildAffiliateInfo(region, affiliateName, affiliationType));
    }

    public static MutableText buildAffiliateInfo(IProtectedRegion region, String affiliateName, AffiliationType affiliationType) {
        return switch (affiliationType) {
            case PLAYER -> {
                PlayerEntity player = RegionDataManager.serverInstance.getPlayerManager().getPlayer(affiliateName);
                yield player == null
                        ? Text.literal((affiliateName)).formatted(GRAY).append(" ").append(Text.translatable("cli.msg.info.player.list.entry.offline", "(offline)"))
                        : buildPlayerHoverComponent(player);
            }
            case TEAM -> {
                Team team = RegionDataManager.serverInstance.getScoreboard().getTeam(affiliateName);
                yield team == null ? Text.literal((affiliateName)) : buildTeamHoverComponent(team);
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


    public static MutableText buildRegionRemoveChildLink(IProtectedRegion region, IProtectedRegion child) {
        String command = buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getName(), REMOVE.toString(), CHILD.toString(), child.getName());
        MutableText linkText = Text.translatableWithFallback("cli.link.remove", "x");
        MutableText linkHoverText = Text.translatableWithFallback("cli.msg.info.region.children.remove.link.hover", "Remove child '%s' from region %s", child.getName(), region.getName());
        return buildExecuteCmdComponent(linkText, linkHoverText, command, SUGGEST_COMMAND, REMOVE_CMD_COLOR);
    }

    public static MutableText buildRegionActionUndoLink(String cmd, CommandConstants toReplace, CommandConstants replacement) {
        String revertCmd = CommandUtil.revertCommand(cmd, toReplace, replacement);
        MutableText revertLinkText = Text.translatableWithFallback("cli.link.action.undo.text", "←");
        MutableText revertLinkHover = Text.translatableWithFallback("cli.link.action.undo.hover", "Undo action.");
        return buildExecuteCmdComponent(revertLinkText, revertLinkHover, revertCmd, RUN_COMMAND, DARK_RED);
    }

    public static MutableText buildRegionActionUndoLink(String cmd, String toReplace, String replacement) {
        String revertCmd = CommandUtil.revertCommand(cmd, toReplace, replacement);
        MutableText revertLinkText = Text.translatableWithFallback("cli.link.action.undo.text", "←");
        MutableText revertLinkHover = Text.translatableWithFallback("cli.link.action.undo.hover", "Undo action.");
        return buildExecuteCmdComponent(revertLinkText, revertLinkHover, revertCmd, RUN_COMMAND, DARK_RED);
    }

    public static MutableText buildFlagHeader(IProtectedRegion region, RegionType regionType) {
        return switch (regionType) {
            case DIMENSION ->
                    buildHeader(Text.translatableWithFallback("cli.msg.info.header.in", "== %s in %s ==", buildFlagListLink(region, regionType), buildRegionInfoLink(region, regionType)));
            case LOCAL ->
                    buildHeader(Text.translatableWithFallback("cli.msg.info.header.in", "== %s in %s ==", buildFlagListLink(region, regionType), buildRegionInfoLink(region, regionType)));
            default -> throw new IllegalStateException("Unexpected value: " + regionType);
        };
    }
}
