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

    public static MutableComponent buildHeader(MutableComponent header) {
        return Component.literal(BOLD + "")
                .append(header)
                .append(Component.literal(BOLD + ""));
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
        sendCmdFeedback(src, Component.translatable(langKey));
    }

    public static void sendMessage(Player player, MutableComponent textComponent) {
        player.sendSystemMessage(textComponent);
    }

    public static void sendMessage(Player player, String translationKey) {
        player.sendSystemMessage(Component.translatable(translationKey));
    }

    public static void sendDimFlagNotification(Player player, RegionFlag flag) {
        player.displayClientMessage(Component.translatable("flag.dim.player.msg.push.deny", flag.name), true);
    }

    public static void sendFlagNotification(Player player, IMarkableRegion region, RegionFlag flag) {
        player.displayClientMessage(Component.translatable("flag.local.player.msg.push.deny", region.getName(), flag.name), true);
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

    public static MutableComponent buildExecuteCmdComponent(String linkText, String hoverText, String command, ClickEvent.Action eventAction, ChatFormatting color) {
        MutableComponent text = ComponentUtils.wrapInSquareBrackets(Component.translatable(linkText));
        return text.setStyle(text.getStyle()
                .withColor(color)
                .withClickEvent(new ClickEvent(eventAction, command))
                .withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, Component.translatable(hoverText))));
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
                .withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, Component.translatable("cli.msg.info.region.affiliation.link.hover")))
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
                MutableComponent sizeInfo = Component.translatable("cli.msg.info.region.spatial.area.size")
                        .append(": ")
                        .append("X=" + cuboidArea.getArea().getXsize() + ", Y=" + cuboidArea.getArea().getYsize() + ", Z=" + cuboidArea.getArea().getZsize());
                MutableComponent markedBlocksInfo = Component.translatable("cli.msg.info.region.spatial.area.blocks")
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
        MutableComponent teleportCmdHoverText = Component.translatable("cli.msg.info.region.spatial.location.teleport", region.getName(), region.getDim().location().toString());
        return buildExecuteCmdComponent(Component.literal(cmdLinkText), teleportCmdHoverText, executeCmdStr, RUN_COMMAND, TP_COLOR);
    }

    public static MutableComponent buildHelpSuggestionLink(String translationKey, CommandConstants baseCmd, CommandConstants cmd) {
        String command = "/" + CommandPermissionConfig.BASE_CMD + " " + baseCmd + " " + cmd + " ";
        return Component.literal(" ")
                .append(buildExecuteCmdComponent("=>", "chat.link.hover.command.copy", command, SUGGEST_COMMAND, SUGGEST_COLOR))
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
        String linkTextKey = "cli.msg.info.region.state.enable." + region.isActive() + ".link.text";
        String hoverTextKey = "cli.msg.info.region.state.enable." + !region.isActive() + ".link.hover";
        ChatFormatting color = region.isActive() ? ADD_CMD_COLOR : REMOVE_CMD_COLOR;
        return buildExecuteCmdComponent(linkTextKey, hoverTextKey, cmd, ClickEvent.Action.RUN_COMMAND, color);
    }

    public static MutableComponent buildRegionPriorityComponent(IMarkableRegion region) {
        String incPriorityCmd = CommandUtil.buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), STATE.toString(), PRIORITY.toString(), INC.toString(), String.valueOf(CLI_REGION_DEFAULT_PRIORITY_INC.get()));
        MutableComponent incLinkText = Component.translatable("cli.msg.info.region.state.priority.increase.link.text", CLI_REGION_DEFAULT_PRIORITY_INC.get());
        MutableComponent incHoverText = Component.translatable("cli.msg.info.region.state.priority.increase.link.hover", CLI_REGION_DEFAULT_PRIORITY_INC.get());
        MutableComponent increaseLink = buildExecuteCmdComponent(incLinkText, incHoverText, incPriorityCmd, ClickEvent.Action.RUN_COMMAND, ADD_CMD_COLOR);
        String decPriorityCmd = CommandUtil.buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), STATE.toString(), PRIORITY.toString(), DEC.toString(), String.valueOf(CLI_REGION_DEFAULT_PRIORITY_INC.get()));
        MutableComponent decLinkText = Component.translatable("cli.msg.info.region.state.priority.decrease.link.text", CLI_REGION_DEFAULT_PRIORITY_INC.get());
        MutableComponent decHoverText = Component.translatable("cli.msg.info.region.state.priority.decrease.link.hover", CLI_REGION_DEFAULT_PRIORITY_INC.get());
        MutableComponent decreaseLink = buildExecuteCmdComponent(decLinkText, decHoverText, decPriorityCmd, ClickEvent.Action.RUN_COMMAND, REMOVE_CMD_COLOR);
        MutableComponent priorityValue = Component.literal(String.valueOf(region.getPriority()));
        String setPriorityCmd = CommandUtil.buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), STATE.toString(), PRIORITY.toString(), "");
        MutableComponent setPriorityLinkText = Component.translatable("cli.msg.info.region.state.priority.set.link.text");
        MutableComponent setPriorityHoverText = Component.translatable("cli.msg.info.region.state.priority.set.link.hover");
        MutableComponent setPriorityLink = buildExecuteCmdComponent(setPriorityLinkText, setPriorityHoverText, setPriorityCmd, SUGGEST_COMMAND, SUGGEST_COLOR);
        return priorityValue.append(" ")
                .append(setPriorityLink).append(" ")
                .append(increaseLink).append(" ")
                .append(decreaseLink);
    }

    public static MutableComponent buildRegionAlertComponentLink(IMarkableRegion region) {
        String cmd = CommandUtil.buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), STATE.toString(), ALERT.toString());
        String linkTextKey = "cli.msg.info.region.state.alert." + !region.isMuted() + ".link.text";
        String hoverTextKey = "cli.msg.info.region.state.alert." + region.isMuted() + ".link.hover";
        ChatFormatting color = region.isMuted() ? REMOVE_CMD_COLOR : ADD_CMD_COLOR;
        return buildExecuteCmdComponent(linkTextKey, hoverTextKey, cmd, ClickEvent.Action.RUN_COMMAND, color);
    }

    public static MutableComponent buildRegionInfoLink(IProtectedRegion region, RegionType type) {
        return switch (type) {
            case DIMENSION -> {
                String command = buildCommandStr(DIM.toString(), region.getDim().location().toString(), INFO.toString());
                MutableComponent hoverText = Component.translatable("cli.msg.dim.info");
                MutableComponent linkText = Component.literal(region.getDim().location().toString());
                yield buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, LINK_COLOR);
            }
            case LOCAL -> {
                String cmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), INFO.toString());
                MutableComponent regionInfoLinkText = Component.literal(region.getName());
                MutableComponent regionInfoLinkHover = Component.translatable("cli.msg.info.region", region.getName());
                yield buildExecuteCmdComponent(regionInfoLinkText, regionInfoLinkHover, cmd, RUN_COMMAND, LINK_COLOR);
            }
            default -> throw new IllegalStateException("Unexpected value: " + type);
        };
    }

    public static MutableComponent buildRegionSpatialPropLink(IMarkableRegion region) {
        String showSpatialPropLink = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), SPATIAL.toString());
        MutableComponent spatialPropLinkText = Component.translatable("cli.msg.info.region.spatial.link.text");
        MutableComponent spatialPropHoverText = Component.translatable("cli.msg.info.region.spatial.link.hover", region.getName());
        return buildExecuteCmdComponent(spatialPropLinkText, spatialPropHoverText, showSpatialPropLink, RUN_COMMAND, LINK_COLOR);
    }

    public static MutableComponent buildRegionOverviewHeader(IProtectedRegion region, RegionType type) {
        return switch (type) {
            case DIMENSION -> {
                MutableComponent clipBoardDumpLink = buildExecuteCmdComponent("cli.msg.dim.overview.header.dump.link.text", "cli.msg.dim.overview.header.dump.link.hover", NbtUtils.prettyPrint(region.serializeNBT()), ClickEvent.Action.COPY_TO_CLIPBOARD, GOLD);
                yield buildHeader(Component.translatable("cli.msg.info.header.for", clipBoardDumpLink, buildRegionInfoLink(region, RegionType.DIMENSION)));
            }
            case LOCAL -> {
                MutableComponent clipBoardDumpLink = buildExecuteCmdComponent("cli.msg.info.region.overview.dump.link.text", "cli.msg.info.region.overview.dump.link.hover", NbtUtils.prettyPrint(region.serializeNBT()), ClickEvent.Action.COPY_TO_CLIPBOARD, GOLD);
                yield buildHeader(Component.translatable("cli.msg.info.header.for", clipBoardDumpLink, buildRegionInfoLink(region, LOCAL)));
            }
            default -> throw new IllegalStateException("Unexpected value: " + type);
        };
    }

    /**
     * Players: [n player(s)] [+]
     * // TODO:
     */
    public static MutableComponent buildPlayerListLink(IProtectedRegion region, PlayerContainer players, String affiliation, RegionType regionType) {
        MutableComponent hoverText = Component.translatable("cli.msg.info.region.affiliation.player.list.link.hover", affiliation, region.getName());
        MutableComponent linkText = Component.translatable("cli.msg.info.region.affiliation.player.list.link.text", players.getPlayers().size());
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
     * // TODO:
     */
    public static MutableComponent buildTeamListLink(IProtectedRegion region, PlayerContainer teams, String affiliation, RegionType regionType) {
        MutableComponent hoverText = Component.translatable("cli.msg.info.region.affiliation.team.list.link.hover", affiliation, region.getName());
        MutableComponent linkText = Component.translatable("cli.msg.info.region.affiliation.team.list.link.text", teams.getTeams().size());
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
        MutableComponent linkText = Component.translatable("cli.link.add");
        MutableComponent hoverText = Component.translatable("cli.msg.info.region.affiliation." + affiliationType.name + ".add.link.hover", affiliation, region.getName());
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
        return buildHeader(Component.translatable("cli.msg.info.header.in", affiliationLink, buildRegionInfoLink(region, regionType)));
    }

    public static MutableComponent buildAffiliationHeader(IProtectedRegion region, String affiliation, AffiliationType affiliationType, RegionType regionType) {
        return Component.translatable("cli.msg.info.region.affiliation." + affiliationType.name + ".list", buildRegionInfoLink(region, regionType), affiliation);
    }

    public static MutableComponent buildAffiliationLink(IProtectedRegion region, String affiliation, int affiliationSize, RegionType regionType) {
        MutableComponent linkText = Component.translatable("cli.msg.info.region.affiliation.list.link.text", affiliationSize, affiliation);
        MutableComponent hoverText = Component.translatable("cli.msg.info.region.affiliation.list.link.hover", affiliation, region.getName());
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
        MutableComponent teams = Component.translatable("cli.msg.info.region.affiliation.team").append(": ");
        MutableComponent teamAddLink = buildAddAffiliateLink(region, affiliation, AffiliationType.TEAM, regionType);
        MutableComponent teamListLink = playerContainer.hasTeams()
                ? buildTeamListLink(region, playerContainer, affiliation, regionType)
                : Component.translatable("cli.msg.info.region.affiliation.team.list.link.text", playerContainer.getTeams().size());
        teams.append(teamListLink).append(teamAddLink);
        return teams;
    }

    // TODO: combine team and player list link
    public static MutableComponent buildAffiliationPlayerListLink(IProtectedRegion region, String affiliation, RegionType regionType) {
        // Players: [n player(s)] [+]
        PlayerContainer playerContainer = affiliation.equals("owner") ? region.getOwners() : region.getMembers();
        MutableComponent players = Component.translatable("cli.msg.info.region.affiliation.player").append(": ");
        MutableComponent playersAddLink = buildAddAffiliateLink(region, affiliation, AffiliationType.PLAYER, regionType);
        MutableComponent playerListLink = playerContainer.hasPlayers()
                ? buildPlayerListLink(region, playerContainer, affiliation, regionType)
                : Component.translatable("cli.msg.info.region.affiliation.player.list.link.text", playerContainer.getPlayers().size());
        players.append(playerListLink).append(playersAddLink);
        return players;
    }

    private static MutableComponent buildRemoveFlagEntry(IProtectedRegion region, IFlag flag, RegionType regionType) {
        MutableComponent flagRemoveEntry = Component.literal(" - ");
        MutableComponent flagRemoveLink;
        switch (regionType) {
            case DIMENSION: {
                String command = buildCommandStr(DIM.toString(), region.getDim().location().toString(), REMOVE.toString(), FLAG.toString(), flag.getFlagIdentifier());
                MutableComponent hoverText = Component.translatable("cli.msg.dim.info.flag.remove.link.hover", flag.getFlagIdentifier(), region.getDim().location().toString());
                MutableComponent linkText = Component.translatable("cli.link.remove");
                flagRemoveLink = buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, REMOVE_CMD_COLOR);
                break;
            }
            case LOCAL: {
                String cmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), REMOVE.toString(), FLAG.toString(), flag.getFlagIdentifier());
                MutableComponent hoverText = Component.translatable("cli.msg.info.region.flag.remove.link.hover", flag.getFlagIdentifier(), region.getName());
                MutableComponent linkText = Component.translatable("cli.link.remove");
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
                MutableComponent childIndicator = buildTextWithHoverMsg(Component.literal("*"), Component.translatable("cli.msg.info.dim.region.child.hover"), GOLD);
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
            paginationComponents.add(Component.translatable("cli.msg.info.pagination.error.index", pageNo, numberOfPages - 1).withStyle(RED));
            return paginationComponents;
        }
        boolean hasMultiplePages = numberOfPages > 1;

        MutableComponent first = hasMultiplePages && pageNo != FIRST_PAGE_IDX
                ? buildExecuteCmdComponent(Component.translatable("cli.msg.info.pagination.first.text"), Component.translatable("cli.msg.info.pagination.first.hover"), buildPageCommand(cmd, FIRST_PAGE_IDX), RUN_COMMAND, LINK_COLOR)
                : ComponentUtils.wrapInSquareBrackets(Component.translatable("cli.msg.info.pagination.first.text")).withStyle(INACTIVE_LINK_COLOR);
        MutableComponent prev = hasMultiplePages && pageNo > FIRST_PAGE_IDX
                ? buildExecuteCmdComponent(Component.translatable("cli.msg.info.pagination.previous.text"), Component.translatable("cli.msg.info.pagination.previous.hover"), buildPageCommand(cmd, Math.max(pageNo - 1, FIRST_PAGE_IDX)), RUN_COMMAND, LINK_COLOR)
                : ComponentUtils.wrapInSquareBrackets(Component.translatable("cli.msg.info.pagination.previous.text")).withStyle(INACTIVE_LINK_COLOR);
        MutableComponent next = hasMultiplePages && pageNo < numberOfPages - 1
                ? buildExecuteCmdComponent(Component.translatable("cli.msg.info.pagination.next.text"), Component.translatable("cli.msg.info.pagination.next.hover"), buildPageCommand(cmd, Math.min(pageNo + 1, numberOfPages - 1)), RUN_COMMAND, LINK_COLOR)
                : ComponentUtils.wrapInSquareBrackets(Component.translatable("cli.msg.info.pagination.next.text")).withStyle(INACTIVE_LINK_COLOR);
        MutableComponent last = hasMultiplePages && pageNo < numberOfPages - 1
                ? buildExecuteCmdComponent(Component.translatable("cli.msg.info.pagination.last.text"), Component.translatable("cli.msg.info.pagination.last.hover"), buildPageCommand(cmd, numberOfPages - 1), RUN_COMMAND, LINK_COLOR)
                : ComponentUtils.wrapInSquareBrackets(Component.translatable("cli.msg.info.pagination.last.text")).withStyle(INACTIVE_LINK_COLOR);

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
        return buildHeader(Component.translatable("cli.msg.info.header.in", buildRegionChildrenLink(region, type), buildRegionInfoLink(region, type)));
    }

    public static MutableComponent buildRegionParentLink(IMarkableRegion region) {
        MutableComponent parentLink = null;
        if (region.getParent() != null) { // FIXME: should not happen. it is either a dim or local region as parent
            String regionParentInfoCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getParent().getName(), INFO.toString());
            MutableComponent parentLinkText = Component.translatable("cli.msg.info.region.parent.link.text", region.getParent().getName());
            MutableComponent parentHoverText = Component.translatable("cli.msg.info.region.parent.link.hover", region.getParent().getName());
            if (region.getParent() instanceof DimensionalRegion) {
                return buildRegionInfoLink(region.getParent(), RegionType.DIMENSION);
            }
            if (region.getParent() instanceof IMarkableRegion) {
                String clearRegionParentCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), PARENT.toString(), CLEAR.toString(), region.getParent().getName());
                MutableComponent parentClearLinkText = Component.translatable("cli.msg.info.region.parent.clear.link.text");
                MutableComponent parentClearHoverText = Component.translatable("cli.msg.info.region.parent.clear.link.hover", region.getParent().getName());
                parentLink = buildExecuteCmdComponent(parentLinkText, parentHoverText, regionParentInfoCmd, RUN_COMMAND, LINK_COLOR)
                        .append(buildExecuteCmdComponent(parentClearLinkText, parentClearHoverText, clearRegionParentCmd, RUN_COMMAND, REMOVE_CMD_COLOR));
                return parentLink;
            }
            if (region.getParent() instanceof GlobalRegion) { // FIXME: Not needed here
                // TODO: Hierarchy Info for dimensional Regions
            }

        } else {
            String setRegionParentCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), PARENT.toString(), SET.toString(), "");
            MutableComponent setParentLinkText = Component.translatable("cli.link.add");
            MutableComponent setParentHoverText = Component.translatable("cli.msg.info.region.parent.set.link.hover", region.getName());
            parentLink = Component.translatable("cli.msg.info.region.parent.null")
                    .append(" ")
                    .append(buildExecuteCmdComponent(setParentLinkText, setParentHoverText, setRegionParentCmd, RUN_COMMAND, GREEN));
        }
        return parentLink;
    }

    public static MutableComponent buildDimRegionListHeader(DimensionalRegion dimRegion) {
        return buildHeader(Component.translatable("cli.msg.info.header.in",
                buildRegionChildrenLink(dimRegion, RegionType.DIMENSION), buildRegionInfoLink(dimRegion, RegionType.DIMENSION)));
    }

    public static MutableComponent buildRegionChildrenLink(IProtectedRegion region, RegionType type) {
        return switch (type) {
            case DIMENSION -> {
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(region.getDim());
                String command = buildCommandStr(DIM.toString(), region.getDim().location().toString(), LIST.toString(), REGION.toString());
                MutableComponent listDimRegionsLinkText = Component.translatable("cli.msg.dim.info.region.list.link.text", dimCache.getRegions().size());
                MutableComponent listDimRegionsHoverText = Component.translatable("cli.msg.dim.info.region.list.link.hover", region.getName());
                MutableComponent listDimRegionsListLink = buildExecuteCmdComponent(listDimRegionsLinkText, listDimRegionsHoverText, command, RUN_COMMAND, LINK_COLOR);
                MutableComponent createRegionLink = buildDimCreateRegionLink(region);
                yield (region.getChildren().size() == 0) ? listDimRegionsLinkText.append(createRegionLink) : listDimRegionsListLink.append(createRegionLink);
            }
            case LOCAL -> {
                String regionChildrenListLink = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), CHILDREN.toString());
                MutableComponent childrenLinkText = Component.translatable("cli.msg.info.region.children.link.text", region.getChildren().size());
                MutableComponent childrenHoverText = Component.translatable("cli.msg.info.region.children.link.hover", region.getName());
                MutableComponent regionChildrenLink = buildExecuteCmdComponent(childrenLinkText, childrenHoverText, regionChildrenListLink, RUN_COMMAND, LINK_COLOR);
                MutableComponent addChildrenLink = buildRegionAddChildrenLink(region);
                yield (region.getChildren().size() == 0) ? childrenLinkText.append(addChildrenLink) : regionChildrenLink.append(addChildrenLink);
            }
            default -> throw new IllegalStateException("Unexpected value: " + type);
        };
    }

    public static MutableComponent buildRegionAddChildrenLink(IProtectedRegion region) {
        String addChildrenCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), ADD.toString(), CHILD.toString(), "");
        MutableComponent addChildrenLinkText = Component.translatable("cli.link.add");
        MutableComponent addChildrenHoverText = Component.translatable("cli.msg.info.region.children.add.link.hover", region.getName());
        return buildExecuteCmdComponent(addChildrenLinkText, addChildrenHoverText, addChildrenCmd, SUGGEST_COMMAND, ADD_CMD_COLOR);
    }

    public static MutableComponent buildDimCreateRegionLink(IProtectedRegion region) {
        String dimCreateRegionCmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), CREATE.toString(), REGION.toString(), "");
        MutableComponent createRegionLinkText = Component.translatable("cli.link.add");
        MutableComponent createRegionHoverText = Component.translatable("cli.msg.dim.info.region.create.link.hover", region.getName());
        return buildExecuteCmdComponent(createRegionLinkText, createRegionHoverText, dimCreateRegionCmd, SUGGEST_COMMAND, ADD_CMD_COLOR);
    }

    public static MutableComponent buildFlagListLink(IProtectedRegion region, RegionType type) {
        MutableComponent flagLink = switch (type) {
            case DIMENSION -> {
                String flagListCmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), LIST.toString(), FLAG.toString());
                MutableComponent flagListLinkText = Component.translatable("cli.msg.info.region.flag.link.text", region.getFlags().size());
                MutableComponent flagListHoverText = Component.translatable("cli.msg.dim.flag.list.link.hover", region.getName());
                MutableComponent dimFlagListLink = buildExecuteCmdComponent(flagListLinkText, flagListHoverText, flagListCmd, RUN_COMMAND, LINK_COLOR);
                yield dimFlagListLink.append(buildDimAddFlagLink(region));
            }
            case LOCAL -> {
                String listCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), FLAG.toString());
                MutableComponent flagListLinkText = Component.translatable("cli.msg.info.region.flag.link.text", region.getFlags().size());
                MutableComponent flagListHoverText = Component.translatable("cli.msg.info.region.flag.link.hover", region.getName());
                MutableComponent flagListLink = buildExecuteCmdComponent(flagListLinkText, flagListHoverText, listCmd, RUN_COMMAND, LINK_COLOR);
                yield flagListLink.append(buildRegionAddFlagLink(region));
            }
            default -> throw new IllegalStateException("Unexpected value: " + type);
        };
        return flagLink;
    }

    public static MutableComponent buildDimAddFlagLink(IProtectedRegion dimRegion) {
        String command = buildCommandStr(DIM.toString(), dimRegion.getDim().location().toString(), ADD.toString(), FLAG.toString(), "");
        MutableComponent hoverText = Component.translatable("cli.msg.dim.flag.add.link.hover", dimRegion.getDim().location().toString());
        MutableComponent linkText = Component.translatable("cli.link.add");
        return buildExecuteCmdComponent(linkText, hoverText, command, SUGGEST_COMMAND, ADD_CMD_COLOR);
    }

    public static MutableComponent buildRegionAddFlagLink(IProtectedRegion region) {
        String addCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), ADD.toString(), FLAG.toString(), "");
        MutableComponent flagAddHoverText = Component.translatable("cli.msg.info.region.flag.add.link.hover", region.getName());
        MutableComponent flagAddLinkText = Component.translatable("cli.link.add");
        MutableComponent addFlag = buildExecuteCmdComponent(flagAddLinkText, flagAddHoverText, addCmd, SUGGEST_COMMAND, ADD_CMD_COLOR);
        return addFlag;
    }

    public static MutableComponent buildRegionStateLink(IMarkableRegion region) {
        String showStateCmd = buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), STATE.toString());
        MutableComponent stateLinkText = Component.translatable("cli.msg.info.region.state.link.text");
        MutableComponent stateHoverText = Component.translatable("cli.msg.info.region.state.link.hover", region.getName());
        return buildExecuteCmdComponent(stateLinkText, stateHoverText, showStateCmd, RUN_COMMAND, LINK_COLOR);
    }

    public static MutableComponent buildStateLink(IProtectedRegion region) {
        String command = CommandUtil.buildCommandStr(DIM.toString(), region.getDim().location().toString(), ENABLE.toString());
        String onClickAction = region.isActive() ? "deactivate" : "activate";
        String hoverText = "cli.msg.info.state." + onClickAction;
        String linkText = "cli.msg.info.state.link." + (region.isActive() ? "activate" : "deactivate");
        ChatFormatting color = region.isActive() ? ADD_CMD_COLOR : REMOVE_CMD_COLOR;
        MutableComponent stateLink = buildExecuteCmdComponent(linkText, hoverText, command, ClickEvent.Action.RUN_COMMAND, color);
        return Component.translatable("cli.msg.info.state")
                .append(Component.literal(": "))
                .append(stateLink);
    }

    public static MutableComponent buildInfoComponent(String subjectLangKey, MutableComponent payload) {
        return Component.translatable(subjectLangKey).append(": ").append(payload);
    }

    public static MutableComponent buildRegionTeleportLink(IMarkableRegion region) {
        String teleportCmd = buildDimTeleportCmd(region.getDim(), "@s", region.getTpTarget());
        return buildExecuteCmdComponent(buildBlockPosTeleportLinkText(region.getTpTarget()),
                "cli.msg.region.info.tp.link.hover", teleportCmd, RUN_COMMAND, TP_COLOR);
    }

    public static MutableComponent buildDimensionalBlockTpLink(ResourceKey<Level> dim, BlockPos target) {
        String teleportCmd = buildDimTeleportCmd(dim, "@s", target);
        return buildExecuteCmdComponent(buildBlockPosTeleportLinkText(target),
                "cli.msg.info.region.spatial.location.teleport.link.hover", teleportCmd, RUN_COMMAND, TP_COLOR);
    }

    public static MutableComponent buildDimSuggestRegionRemovalLink(IMarkableRegion region) {
        String cmd = buildCommandStr(DIM.toString(), region.getDim().location().toString(), DELETE.toString(), region.getName());
        MutableComponent hover = Component.translatable("cli.msg.info.dim.region.remove.link.hover", region.getName());
        MutableComponent text = Component.translatable("cli.link.remove");
        return buildExecuteCmdComponent(text, hover, cmd, SUGGEST_COMMAND, REMOVE_CMD_COLOR);
    }

    /* TODO: extract method for n component(s) [+] */
    public static MutableComponent buildDimFlagListLink(IProtectedRegion region) {
        String command = buildCommandStr(DIM.toString(), region.getDim().location().toString(), LIST.toString(), FLAG.toString());
        MutableComponent hoverLink = Component.translatable("cli.msg.dim.flag.list.link.hover", region.getDim().location().toString());
        MutableComponent linkText = Component.translatable("cli.msg.flag.list.link.text", region.getFlags().size());
        return region.getFlags().isEmpty()
                ? Component.translatable("cli.msg.info.region.flag.link.text", region.getFlags().size())
                : buildExecuteCmdComponent(linkText, hoverLink, command, RUN_COMMAND, LINK_COLOR);
    }


    public static List<MutableComponent> buildRemoveAffiliationEntries(IProtectedRegion region, List<String> affiliationNames, AffiliationType affiliationType, String affiliation, RegionType parentType) {
        return affiliationNames.stream().map(affiliate -> buildRemoveAffiliateEntry(region, affiliate, affiliationType, affiliation, parentType)).collect(Collectors.toList());
    }

    public static MutableComponent buildRemoveAffiliateEntry(IProtectedRegion region, String affiliateName, AffiliationType affiliationType, String affiliation, RegionType regionType) {
        MutableComponent linkText = Component.translatable("cli.link.remove");
        MutableComponent hoverText = Component.translatable("cli.msg.info.region.affiliation." + affiliationType.name + ".remove.link.hover", affiliateName, region.getName());
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
                yield player == null
                        ? Component.literal(affiliateName).withStyle(GRAY).append(" ").append(Component.translatable("cli.msg.info.player.list.entry.offline"))
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
        MutableComponent linkText = Component.translatable("cli.link.remove");
        MutableComponent linkHoverText = Component.translatable("cli.msg.info.region.children.remove.link.hover", child.getName(), region.getName());
        return buildExecuteCmdComponent(linkText, linkHoverText, command, SUGGEST_COMMAND, REMOVE_CMD_COLOR);
    }

    public static MutableComponent buildRegionActionUndoLink(String cmd, CommandConstants toReplace, CommandConstants replacement) {
        String revertCmd = CommandUtil.revertCommand(cmd, toReplace, replacement);
        MutableComponent revertLinkText = Component.translatable("cli.link.action.undo.text");
        MutableComponent revertLinkHover = Component.translatable("cli.link.action.undo.hover");
        return buildExecuteCmdComponent(revertLinkText, revertLinkHover, revertCmd, RUN_COMMAND, DARK_RED);
    }

    public static MutableComponent buildRegionActionUndoLink(String cmd, String toReplace, String replacement) {
        String revertCmd = CommandUtil.revertCommand(cmd, toReplace, replacement);
        MutableComponent revertLinkText = Component.translatable("cli.link.action.undo.text");
        MutableComponent revertLinkHover = Component.translatable("cli.link.action.undo.hover");
        return buildExecuteCmdComponent(revertLinkText, revertLinkHover, revertCmd, RUN_COMMAND, DARK_RED);
    }

    public static MutableComponent buildFlagHeader(IProtectedRegion region, RegionType regionType) {
        return switch (regionType) {
            case DIMENSION ->
                    buildHeader(Component.translatable("cli.msg.info.header.in", buildFlagListLink(region, regionType), buildRegionInfoLink(region, regionType)));
            case LOCAL ->
                    buildHeader(Component.translatable("cli.msg.info.header.in", buildFlagListLink(region, regionType), buildRegionInfoLink(region, regionType)));
            default -> throw new IllegalStateException("Unexpected value: " + regionType);
        };
    }
}
