package de.z0rdak.yawp.util;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.commands.CommandConstants;
import de.z0rdak.yawp.config.server.CommandPermissionConfig;
import de.z0rdak.yawp.core.affiliation.PlayerContainer;
import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.area.IMarkableArea;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.*;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.NbtHelper;
import net.minecraft.registry.RegistryKey;
import net.minecraft.server.command.ServerCommandSource;
import net.minecraft.text.*;
import net.minecraft.util.Formatting;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.apache.commons.lang3.NotImplementedException;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.util.CommandUtil.buildCommandStr;
import static net.minecraft.text.ClickEvent.Action.RUN_COMMAND;
import static net.minecraft.text.ClickEvent.Action.SUGGEST_COMMAND;
import static net.minecraft.util.Formatting.*;


public class MessageUtil {

    private MessageUtil() {
    }

    public static MutableText buildHelpHeader(String translationKey) {
        return buildHelpHeader(MutableText.of(new TranslatableTextContent(translationKey)));
    }


    public static MutableText buildHelpHeader(MutableText TranslatableComponent) {
        return MutableText.of(new LiteralTextContent(BOLD + " == "))
                .append(TranslatableComponent)
                .append(MutableText.of(new LiteralTextContent(BOLD + " == ")));
    }

    public static void sendCmdFeedback(ServerCommandSource src, MutableText text) {
        try {
            if (src.getEntity() == null) {
                src.sendFeedback(text, true);
            } else {
                sendMessage(src.getPlayerOrThrow(), text);
            }
        } catch (CommandSyntaxException e) {
            YetAnotherWorldProtector.LOGGER.error(e);
        }
    }

    public static void sendCmdFeedback(ServerCommandSource src, String langKey) {
        sendCmdFeedback(src, MutableText.of(new TranslatableTextContent(langKey)));
    }

    public static void sendMessage(PlayerEntity player, MutableText textComponent) {
        player.sendMessage(textComponent);
    }

    public static void sendMessage(PlayerEntity player, String translationKey) {
        player.sendMessage(MutableText.of(new TranslatableTextContent(translationKey)));
    }

    public static void sendDimFlagNotification(PlayerEntity player, RegionFlag flag) {
        player.sendMessage(MutableText.of(new TranslatableTextContent("flag.dim.player.msg.push.deny", flag.name)), true);
    }

    public static void sendFlagNotification(PlayerEntity player, IMarkableRegion region, RegionFlag flag) {
        player.sendMessage(MutableText.of(new TranslatableTextContent("flag.local.player.msg.push.deny", region.getName(), flag.name)), true);
    }

    public static MutableText buildExecuteCmdComponent(String linkText, String hoverText, String command, ClickEvent.Action eventAction, Formatting color) {
        return Texts.bracketed(MutableText.of(new TranslatableTextContent(linkText)))
                .setStyle(Style.EMPTY.withColor(color)
                        .withClickEvent(new ClickEvent(eventAction, command))
                        .withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, MutableText.of(new TranslatableTextContent(hoverText)))));
    }

    public static MutableText buildExecuteCmdComponent(MutableText linkText, MutableText hoverText, String command, ClickEvent.Action eventAction, Formatting color) {
        return Texts.bracketed(linkText)
                .setStyle(Style.EMPTY.withColor(color)
                        .withClickEvent(new ClickEvent(eventAction, command))
                        .withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, hoverText)));
    }

    public static MutableText buildTextWithHoverMsg(MutableText text, MutableText hoverText, Formatting color) {
        return Texts.bracketed(text)
                .setStyle(Style.EMPTY.withColor(color)
                        .withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, hoverText)));
    }

    public static MutableText buildDimensionTeleportLink(IMarkableRegion region) {
        String cmdLinkText = buildTeleportLinkText(region.getDim(), region.getTpTarget());
        String executeCmdStr = buildDimTeleportCmd(region.getDim(), "@s", region.getTpTarget());
        MutableText teleportCmdHoverText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.spatial.location.teleport", region.getName(), region.getDim().getValue().toString()));
        return buildExecuteCmdComponent(MutableText.of(new LiteralTextContent(cmdLinkText)), teleportCmdHoverText, executeCmdStr, RUN_COMMAND, AQUA);
    }


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

    public static MutableText buildHelpSuggestionLink(String translationKey, CommandConstants baseCmd, CommandConstants cmd) {
        String command = "/" + CommandPermissionConfig.BASE_CMD + " " + baseCmd + " " + cmd + " ";
        return MutableText.of(new LiteralTextContent(" "))
                .append(buildExecuteCmdComponent("=>", "chat.link.hover.command.copy", command, SUGGEST_COMMAND, GREEN))
                .append(MutableText.of(new LiteralTextContent(" ")))
                .append(MutableText.of(new TranslatableTextContent(translationKey)));
    }

    public static String buildDimAddPlayerCmdStr(String region, CommandConstants memberOrOwner) {
        return buildDimAddCmdStr(region) + " " + PLAYER + " " + memberOrOwner + " ";
    }

    public static String buildDimAddTeamCmdStr(String region, CommandConstants memberOrOwner) {
        return buildDimAddCmdStr(region) + " " + TEAM + " " + memberOrOwner + " ";
    }

    public static String buildDimAddCmdStr(String region) {
        return "/" + CommandPermissionConfig.BASE_CMD + " " + DIMENSION + " " + region + " " + ADD;
    }

    public static String buildRegionCmdStr(IMarkableRegion region, CommandConstants constant) {
        return CommandUtil.buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getName(), constant.toString());
    }

    public static MutableText buildDimAddPlayerLink(DimensionalRegion dimRegion, String hoverTextLangKey, CommandConstants memberOrOwner) {
        String command = buildDimAddPlayerCmdStr(dimRegion.getName(), memberOrOwner);
        String linkText = "+";
        return buildExecuteCmdComponent(linkText, hoverTextLangKey, command, SUGGEST_COMMAND, GREEN);
    }

    public static MutableText buildDimAddTeamLink(DimensionalRegion dimRegion, String hoverTextLangKey, CommandConstants memberOrOwner) {
        return buildExecuteCmdComponent("+", hoverTextLangKey, buildDimAddTeamCmdStr(dimRegion.getName(), memberOrOwner),
                SUGGEST_COMMAND, GREEN);
    }

    public static MutableText buildRegionEnableComponent(IMarkableRegion region) {
        String cmd = CommandUtil.buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getName(), STATE.toString(), ENABLE.toString(), String.valueOf(!region.isActive()));
        String linkTextKey = "cli.msg.info.region.state.enable." + region.isActive() + ".link.text";
        String hoverTextKey = "cli.msg.info.region.state.enable." + !region.isActive() + ".link.hover";
        Formatting color = region.isActive() ? GREEN : RED;
        return buildExecuteCmdComponent(linkTextKey, hoverTextKey, cmd, RUN_COMMAND, color);
    }

    public static MutableText buildRegionPriorityComponent(IMarkableRegion region) {
        String incPriorityCmd = CommandUtil.buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getName(), STATE.toString(), PRIORITY.toString(), INC.toString(), String.valueOf(/* REGION_DEFAULT_PRIORITY_INC.get() */ 10));
        MutableText incLinkText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.state.priority.increase.link.text", /* REGION_DEFAULT_PRIORITY_INC.get() */ 10));
        MutableText incHoverText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.state.priority.increase.link.hover", /* REGION_DEFAULT_PRIORITY_INC.get() */ 10));
        MutableText increaseLink = buildExecuteCmdComponent(incLinkText, incHoverText, incPriorityCmd, RUN_COMMAND, GREEN);
        String decPriorityCmd = CommandUtil.buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getName(), STATE.toString(), PRIORITY.toString(), DEC.toString(), String.valueOf(/* REGION_DEFAULT_PRIORITY_INC.get() */ 10));
        MutableText decLinkText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.state.priority.decrease.link.text", /* REGION_DEFAULT_PRIORITY_INC.get() */ 10));
        MutableText decHoverText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.state.priority.decrease.link.hover",/* REGION_DEFAULT_PRIORITY_INC.get() */ 10));
        MutableText decreaseLink = buildExecuteCmdComponent(decLinkText, decHoverText, decPriorityCmd, RUN_COMMAND, RED);
        MutableText priorityValue = MutableText.of(new LiteralTextContent(String.valueOf(region.getPriority())));
        String setPriorityCmd = CommandUtil.buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getName(), STATE.toString(), PRIORITY.toString(), "");
        MutableText setPriorityLinkText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.state.priority.set.link.text"));
        MutableText setPriorityHoverText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.state.priority.set.link.hover"));
        MutableText setPriorityLink = buildExecuteCmdComponent(setPriorityLinkText, setPriorityHoverText, setPriorityCmd, SUGGEST_COMMAND, GREEN);
        return priorityValue.append(" ")
                .append(setPriorityLink).append(" ")
                .append(increaseLink).append(" ")
                .append(decreaseLink);
    }

    public static MutableText buildRegionAlertComponentLink(IMarkableRegion region) {
        String cmd = CommandUtil.buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getName(), STATE.toString(), ALERT.toString(), String.valueOf(!region.isMuted()));
        String linkTextKey = "cli.msg.info.region.state.alert." + !region.isMuted() + ".link.text";
        String hoverTextKey = "cli.msg.info.region.state.alert." + region.isMuted() + ".link.hover";
        Formatting color = region.isMuted() ? RED : GREEN;
        return buildExecuteCmdComponent(linkTextKey, hoverTextKey, cmd, RUN_COMMAND, color);
    }


    public static MutableText buildDimensionalInfoLink(RegistryKey<World> dim) {
        String command = "/" + CommandPermissionConfig.BASE_CMD + " " + DIMENSION + " " + dim.getValue() + " " + INFO;
        String hoverText = "cli.msg.dim.info";
        return buildExecuteCmdComponent(dim.getValue().toString(), hoverText, command, RUN_COMMAND, GREEN);
    }

    // TODO: How to make info links more generic for AbstractRegion
    public static MutableText buildRegionInfoLink(RegistryKey<World> dim, String regionName) {
        String cmd = buildCommandStr(REGION.toString(), dim.getValue().toString(), regionName, INFO.toString());
        MutableText regionInfoLinkText = MutableText.of(new LiteralTextContent(regionName));
        MutableText regionInfoLinkHover = MutableText.of(new TranslatableTextContent("cli.msg.info.region", regionName));
        return buildExecuteCmdComponent(regionInfoLinkText, regionInfoLinkHover, cmd, RUN_COMMAND, GREEN);
    }

    public static MutableText buildRegionInfoLink(IProtectedRegion region) {
        return buildRegionInfoLink(region.getDim(), region.getName());
    }

    public static MutableText buildRegionSpatialPropLink(IMarkableRegion region) {
        String showSpatialPropLink = buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getName(), SPATIAL.toString());
        MutableText spatialPropLinkText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.spatial.link.text"));
        MutableText spatialPropHoverText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.spatial.link.hover", region.getName()));
        return buildExecuteCmdComponent(spatialPropLinkText, spatialPropHoverText, showSpatialPropLink, RUN_COMMAND, AQUA);
    }

    public static MutableText buildRegionOverviewHeader(IMarkableRegion region) {
        MutableText clipBoardDumpLink = buildExecuteCmdComponent("cli.msg.info.region.overview.dump.link.text", "cli.msg.info.region.overview.dump.link.hover", NbtHelper.toPrettyPrintedText(region.serializeNBT()).getString(), ClickEvent.Action.COPY_TO_CLIPBOARD, GOLD);
        MutableText header = MutableText.of(new TranslatableTextContent("cli.msg.info.region.overview.header", clipBoardDumpLink, buildRegionInfoLink(region)));
        return MutableText.of(new LiteralTextContent(Formatting.BOLD + ""))
                .append(header)
                .append(MutableText.of(new LiteralTextContent(Formatting.BOLD + "")));
    }

    public static MutableText buildRegionSpatialHeader(IMarkableRegion region) {
        return MutableText.of(new LiteralTextContent(BOLD + ""))
                .append(MutableText.of(new TranslatableTextContent("cli.msg.info.region.spatial.header", buildRegionInfoLink(region))))
                .append(MutableText.of(new LiteralTextContent(BOLD + "")));
    }

    public static MutableText buildRegionLocationComponent(IMarkableRegion region) {
        return composeInfoComponent("cli.msg.info.region.spatial.location", buildDimensionTeleportLink(region));
    }

    public static MutableText buildRegionAreaComponent(IMarkableRegion region) {
        MutableText areaInfo = buildRegionAreaDetailComponent(region);
        return composeInfoComponent("cli.msg.info.region.spatial.area", areaInfo);
    }

    public static MutableText buildBlockPosTpLinks(IMarkableRegion region) {
        List<MutableText> tpLinks = region.getArea().getMarkedBlocks()
                .stream()
                .map(pos -> buildDimensionalBlockTpLink(region.getDim(), pos))
                .toList();
        MutableText blockPosTpLinkList = MutableText.of(new LiteralTextContent(""));
        tpLinks.forEach(tpLink -> blockPosTpLinkList.append(tpLink).append(" "));
        return blockPosTpLinkList;
    }

    public static MutableText buildRegionAreaDetailComponent(IMarkableRegion region) {
        IMarkableArea area = region.getArea();
        MutableText areaInfo = MutableText.of(new LiteralTextContent(area.getAreaType().areaType))
                .append("\n");
        switch (area.getAreaType()) {
            case CUBOID: {
                CuboidArea cuboidArea = (CuboidArea) area;
                MutableText sizeInfo = MutableText.of(new TranslatableTextContent("cli.msg.info.region.spatial.area.size"))
                        .append(": ")
                        .append("X=" + cuboidArea.getArea().getXLength() + ", Y=" + cuboidArea.getArea().getYLength() + ", Z=" + cuboidArea.getArea().getZLength());
                MutableText markedBlocksInfo = MutableText.of(new TranslatableTextContent("cli.msg.info.region.spatial.area.blocks"))
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

    public static MutableText buildRegionAffiliationLink(IMarkableRegion region) {
        MutableText affiliationLinks = MutableText.of(new LiteralTextContent(""));
        List<String> affiliations = Arrays.asList("owner", "member");
        affiliations.forEach(affiliation -> {
            String cmd = buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getName(), LIST.toString(), affiliation);
            int affiliationSize;
            switch (affiliation) {
                case "owner":
                    affiliationSize = region.getOwners().getPlayers().size() + region.getOwners().getTeams().size();
                    break;
                case "member":
                    affiliationSize = region.getMembers().getPlayers().size() + region.getMembers().getTeams().size();
                    break;
                default:
                    affiliationSize = -1;
                    break;
            }
            MutableText linkText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.list.link.text", affiliationSize, affiliation));
            MutableText hoverText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.list.link.hover", affiliation, region.getName()));
            MutableText affiliationLink = buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, AQUA);
            affiliationLinks.append(affiliationLink).append(" ");
        });
        return affiliationLinks;
    }

    private static MutableText buildRegionAddPlayerLink(IMarkableRegion region, MutableText hoverText, String affiliation) {
        String command = buildRegionCmdStr(region, ADD) + " " + PLAYER + " " + affiliation + " ";
        MutableText linkText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.player.add.link.text"));
        return buildExecuteCmdComponent(linkText, hoverText, command, SUGGEST_COMMAND, GREEN);
    }

    private static MutableText buildRegionAddTeamLink(IMarkableRegion region, MutableText hoverText, String affiliation) {
        String command = buildRegionCmdStr(region, ADD) + " " + TEAM + " " + affiliation + " ";
        MutableText linkText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.team.add.link.text"));
        return buildExecuteCmdComponent(linkText, hoverText, command, SUGGEST_COMMAND, GREEN);
    }

    public static MutableText buildRegionAffiliationHeader(IMarkableRegion region, String affiliation) {
        return MutableText.of(new LiteralTextContent(BOLD + ""))
                .append(MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.header", buildRegionInfoLink(region), affiliation)))
                .append(MutableText.of(new LiteralTextContent(BOLD + "")));
    }

    public static MutableText buildRegionAffiliationTeamListLink(IMarkableRegion region, String affiliation, PlayerContainer playerContainer) {
        // Teams: [n team(s)] [+]
        MutableText addTeamLinkHoverText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.team.add.link.hover", affiliation, region.getName()));
        MutableText teamAddLink = buildRegionAddTeamLink(region, addTeamLinkHoverText, affiliation);
        MutableText teamListLink = playerContainer.hasTeams()
                ? buildRegionTeamListLink(region, playerContainer, affiliation)
                : MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.team.list.link.text", playerContainer.getTeams().size()));
        MutableText teams = MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.team")).append(": ");
        teams.append(teamListLink).append(teamAddLink);
        return teams;
    }

    public static MutableText buildRegionAffiliationPlayerListLink(IMarkableRegion region, String affiliation, PlayerContainer playerContainer) {
        // Players: [n player(s)] [+]
        MutableText addPlayerLinkHoverText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.player.add.link.hover", affiliation, region.getName()));
        MutableText playersAddLink = buildRegionAddPlayerLink(region, addPlayerLinkHoverText, affiliation);
        MutableText playerListLink = playerContainer.hasPlayers()
                ? buildRegionPlayerListLink(region, playerContainer, affiliation)
                : MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.player.list.link.text", playerContainer.getPlayers().size()));
        MutableText players = MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.player")).append(": ");
        players.append(playerListLink).append(playersAddLink);
        return players;
    }

    public static MutableText buildRegionHierarchyLink(IMarkableRegion region) {
        return buildRegionParentLink(region);
    }


    public static MutableText buildRegionChildrenHeader(IMarkableRegion region) {
        return MutableText.of(new LiteralTextContent(BOLD + ""))
                .append(MutableText.of(new TranslatableTextContent("cli.msg.info.region.children.header", buildRegionInfoLink(region))))
                .append(MutableText.of(new LiteralTextContent(BOLD + "")));
    }


    public static MutableText buildRegionParentLink(IMarkableRegion region) {
        MutableText parentLink = null;
        if (region.getParent() != null) { // FIXME: should not happen. it is either a dim or local region as parent
            String regionParentInfoCmd = buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getParent().getName(), INFO.toString());
            MutableText parentLinkText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.parent.link.text", region.getParent().getName()));
            MutableText parentHoverText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.parent.link.hover", region.getParent().getName()));
            if (region.getParent() instanceof DimensionalRegion) {
                return buildDimensionalInfoLink(region.getDim());
            }
            if (region.getParent() instanceof IMarkableRegion) {
                String clearRegionParentCmd = buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getName(), PARENT.toString(), CLEAR.toString(), region.getParent().getName());
                MutableText parentClearLinkText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.parent.clear.link.text"));
                MutableText parentClearHoverText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.parent.clear.link.hover", region.getParent().getName()));
                parentLink = buildExecuteCmdComponent(parentLinkText, parentHoverText, regionParentInfoCmd, RUN_COMMAND, AQUA)
                        .append(buildExecuteCmdComponent(parentClearLinkText, parentClearHoverText, clearRegionParentCmd, SUGGEST_COMMAND, RED));
                return parentLink;
            }
            if (region.getParent() instanceof GlobalRegion) { // FIXME: Not needed here
                // TODO: Hierarchy Info for dimensional Regions
            }

        } else {
            String setRegionParentCmd = buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getName(), PARENT.toString(), SET.toString(), "");
            MutableText setParentLinkText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.parent.set.link.text"));
            MutableText setParentHoverText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.parent.set.link.hover", region.getName()));
            parentLink = MutableText.of(new TranslatableTextContent("cli.msg.info.region.parent.null"))
                    .append(" ")
                    .append(buildExecuteCmdComponent(setParentLinkText, setParentHoverText, setRegionParentCmd, SUGGEST_COMMAND, GREEN));
        }
        return parentLink;
    }

    public static MutableText buildRegionChildrenLink(IMarkableRegion region) {
        String regionChildrenListLink = buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getName(), LIST.toString(), CHILDREN.toString());
        MutableText childrenLinkText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.children.link.text", region.getChildren().size()));
        MutableText childrenHoverText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.children.link.hover", region.getName()));
        MutableText regionChildrenLink = buildExecuteCmdComponent(childrenLinkText, childrenHoverText, regionChildrenListLink, RUN_COMMAND, AQUA);
        String addChildrenCmd = buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getName(), ADD.toString(), CHILD.toString(), "");
        MutableText addChildrenLinkText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.children.add.link.text"));
        MutableText addChildrenHoverText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.children.add.link.hover", region.getName()));
        MutableText addChildrenLink = buildExecuteCmdComponent(addChildrenLinkText, addChildrenHoverText, addChildrenCmd, SUGGEST_COMMAND, GREEN);
        return (region.getChildren().size() == 0)
                ? childrenLinkText.append(addChildrenLink)
                : regionChildrenLink.append(addChildrenLink);
    }

    public static MutableText buildFlagListLink(IMarkableRegion region) {
        String listCmd = buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getName(), LIST.toString(), FLAG.toString());
        MutableText flagsLinkText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.flag.link.text", region.getFlags().size()));
        MutableText flagsHoverText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.flag.link.hover", region.getName()));
        MutableText flags = buildExecuteCmdComponent(flagsLinkText, flagsHoverText, listCmd, RUN_COMMAND, AQUA);
        String addCmd = buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getName(), ADD.toString(), FLAG.toString(), "");
        MutableText addFlagLinkText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.flag.add.link.text"));
        MutableText addFlagHoverText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.flag.add.link.hover", region.getName()));
        MutableText addFlag = buildExecuteCmdComponent(addFlagLinkText, addFlagHoverText, addCmd, SUGGEST_COMMAND, GREEN);
        return flags.append(addFlag);
    }

    public static MutableText buildRegionStateLink(IMarkableRegion region) {
        String showStateCmd = buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getName(), STATE.toString());
        MutableText stateLinkText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.state.link.text"));
        MutableText stateHoverText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.state.link.hover", region.getName()));
        return buildExecuteCmdComponent(stateLinkText, stateHoverText, showStateCmd, RUN_COMMAND, AQUA);
    }

    public static MutableText buildRegionStateHeader(IMarkableRegion region) {
        return MutableText.of(new TranslatableTextContent("cli.msg.info.region.state.header", buildRegionInfoLink(region)));
    }

    public static MutableText composeRegionEnableComponent(IMarkableRegion region) {
        return composeInfoComponent("cli.msg.info.region.state.enable", buildRegionEnableComponent(region));
    }

    public static MutableText composeRegionAlertComponent(IMarkableRegion region) {
        return composeInfoComponent("cli.msg.info.region.state.alert",
                buildRegionAlertComponentLink(region));
    }

    public static MutableText composeRegionPriorityComponent(IMarkableRegion region) {
        return composeInfoComponent("cli.msg.info.region.state.priority",
                buildRegionPriorityComponent(region));
    }

    private static MutableText composeInfoComponent(String subjectLangKey, MutableText payload) {
        return MutableText.of(new TranslatableTextContent(subjectLangKey)).append(": ").append(payload);
    }

    public static MutableText buildDimPlayerListLink(DimensionalRegion dimRegion, PlayerContainer players, CommandConstants memberOrOwner) {
        String command = "/" + CommandPermissionConfig.BASE_CMD + " " + DIMENSION + " " + dimRegion.getName() + " " + LIST + " " + memberOrOwner;
        MutableText hoverText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.player.list.link.hover", memberOrOwner.toString(), dimRegion.getName()));
        MutableText linkText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.player.list.link.text", players.getPlayers().size()));
        return buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, AQUA);
    }

    public static MutableText buildDimTeamListLink(DimensionalRegion dimRegion, PlayerContainer teams, CommandConstants memberOrOwner) {
        String command = "/" + CommandPermissionConfig.BASE_CMD + " " + DIMENSION + " " + dimRegion.getName() + " " + LIST + " " + memberOrOwner;
        MutableText hoverText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.team.list.link.hover", memberOrOwner.toString(), dimRegion.getName()));
        MutableText linkText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.team.list.link.text", teams.getTeams().size()));
        return buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, AQUA);
    }

    public static MutableText buildDimRegionListLink(DimensionRegionCache dimCache, DimensionalRegion dimRegion) {
        String command = "/" + CommandPermissionConfig.BASE_CMD + " " + DIMENSION + " " + dimRegion.getName() + " " + LIST + " " + REGION;
        MutableText hoverText = MutableText.of(new TranslatableTextContent("cli.msg.dim.info.region.list.link.hover", dimRegion.getName()));
        MutableText linkText = MutableText.of(new TranslatableTextContent("cli.msg.dim.info.region.list.link.text", dimCache.getRegions().size()));
        return buildExecuteCmdComponent(linkText, hoverText, command, RUN_COMMAND, AQUA);
    }

    public static MutableText buildRegionPlayerListLink(IMarkableRegion region, PlayerContainer players, String affiliation) {
        String cmd = buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getName(), LIST.toString(), affiliation, PLAYER.toString());
        MutableText hoverText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.player.list.link.hover", affiliation, region.getName()));
        MutableText linkText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.player.list.link.text", players.getPlayers().size()));
        return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, AQUA);
    }

    public static MutableText buildRegionTeamListLink(IMarkableRegion region, PlayerContainer teams, String affiliation) {
        String cmd = buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getName(), LIST.toString(), affiliation, TEAM.toString());
        MutableText hoverText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.team.list.link.hover", affiliation, region.getName()));
        MutableText linkText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.team.list.link.text", teams.getTeams().size()));
        return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, AQUA);
    }

    public static MutableText buildRegionTeleportLink(IMarkableRegion region) {
        String teleportCmd = buildDimTeleportCmd(region.getDim(), "@s", region.getTpTarget());
        MutableText teleportLink = buildExecuteCmdComponent(buildBlockPosTeleportLinkText(region.getTpTarget()),
                "cli.msg.region.info.tp.link.hover", teleportCmd, RUN_COMMAND, AQUA);
        return teleportLink;
    }

    public static MutableText buildDimensionalBlockTpLink(RegistryKey<World> dim, BlockPos target) {
        String teleportCmd = buildDimTeleportCmd(dim, "@s", target);
        return buildExecuteCmdComponent(buildBlockPosTeleportLinkText(target),
                "cli.msg.info.region.spatial.location.teleport.link.hover", teleportCmd, RUN_COMMAND, AQUA);
    }

    public static MutableText buildDimSuggestRegionRemovalLink(IMarkableRegion region) {
        String cmd = buildCommandStr(DIMENSION.toString(), region.getDim().getValue().toString(), DELETE.toString(), region.getName());
        MutableText hover = MutableText.of(new TranslatableTextContent("cli.msg.info.dim.region.remove.link.hover", region.getName()));
        MutableText text = MutableText.of(new TranslatableTextContent("cli.msg.info.dim.region.remove.link.text"));
        return buildExecuteCmdComponent(text, hover, cmd, SUGGEST_COMMAND, RED);
    }

    public static MutableText buildAddDimFlagLink(DimensionalRegion dimRegion) {
        String command = buildCommandStr(DIMENSION.toString(), dimRegion.getName(), ADD.toString(), FLAG.toString(), "");
        MutableText hoverText = MutableText.of(new TranslatableTextContent("cli.msg.dim.flag.add.link.hover", dimRegion.getName()));
        MutableText linkText = MutableText.of(new TranslatableTextContent("cli.msg.dim.flag.add.link.text"));
        return buildExecuteCmdComponent(linkText, hoverText, command, SUGGEST_COMMAND, GREEN);
    }

    public static MutableText buildDimFlagListLink(DimensionalRegion dimRegion) {
        String command = buildCommandStr(DIMENSION.toString(), dimRegion.getName(), LIST.toString(), FLAG.toString());
        MutableText hoverLink = MutableText.of(new TranslatableTextContent("cli.msg.dim.flag.list.link.hover", dimRegion.getName()));
        MutableText linkText = MutableText.of(new TranslatableTextContent("cli.msg.flag.list.link.text", dimRegion.getFlags().size()));
        return buildExecuteCmdComponent(linkText, hoverLink, command, RUN_COMMAND, AQUA);
    }

    public static MutableText buildDimRemovePlayerLink(String playerName, RegistryKey<World> dim, CommandConstants memberOrOwner) {
        String command = buildCommandStr(DIMENSION.toString(), dim.getValue().toString(), REMOVE.toString(), PLAYER.toString(), memberOrOwner.toString(), playerName);
        MutableText hoverText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.player.remove.link.hover", playerName, dim.getValue().toString()));
        MutableText linkText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.player.remove.link.text"));
        return buildExecuteCmdComponent(linkText, hoverText, command, SUGGEST_COMMAND, RED);
    }

    public static MutableText buildDimRemoveTeamLink(String teamName, RegistryKey<World> dim, CommandConstants memberOrOwner) {
        String command = buildCommandStr(DIMENSION.toString(), dim.getValue().toString(), REMOVE.toString(), TEAM.toString(), memberOrOwner.toString(), teamName);
        MutableText hoverText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.team.remove.link.hover", teamName, dim.getValue().toString()));
        MutableText linkText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.team.remove.link.text"));
        return buildExecuteCmdComponent(linkText, hoverText, command, SUGGEST_COMMAND, RED);
    }

    public static MutableText buildRegionRemoveTeamLink(IMarkableRegion region, String team, String affiliation) {
        String command = buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getName(), REMOVE.toString(), TEAM.toString(), affiliation, team);
        MutableText linkText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.team.remove.link.text"));
        MutableText linkHoverText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.team.remove.link.hover", team, region.getName()));
        return buildExecuteCmdComponent(linkText, linkHoverText, command, SUGGEST_COMMAND, RED);
    }

    public static MutableText buildRegionRemovePlayerLink(IMarkableRegion region, String player, String affiliation) {
        String command = buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getName(), REMOVE.toString(), PLAYER.toString(), affiliation, player);
        MutableText linkText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.player.remove.link.text"));
        MutableText linkHoverText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.player.remove.link.hover", player, region.getName()));
        return buildExecuteCmdComponent(linkText, linkHoverText, command, SUGGEST_COMMAND, RED);
    }

    public static MutableText buildRegionRemoveChildLink(IProtectedRegion region, IProtectedRegion child) {
        String command = buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getName(), REMOVE.toString(), CHILD.toString(), child.getName());
        MutableText linkText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.children.remove.link.text"));
        MutableText linkHoverText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.children.remove.link.hover", child.getName(), region.getName()));
        return buildExecuteCmdComponent(linkText, linkHoverText, command, SUGGEST_COMMAND, RED);
    }

    public static MutableText buildDimensionRemoveFlagLink(IFlag flag, RegistryKey<World> dim) {
        String command = "/" + CommandPermissionConfig.BASE_CMD + " " + DIMENSION + " " + dim.getValue() + " " + REMOVE + " " + FLAG + " " + flag.getFlagIdentifier();
        MutableText hoverText = MutableText.of(new TranslatableTextContent("cli.msg.dim.info.flag.remove.link.hover", flag.getFlagIdentifier(), dim.getValue().toString()));
        MutableText linkText = MutableText.of(new TranslatableTextContent("cli.msg.dim.info.flag.remove.link.text"));
        return buildExecuteCmdComponent(linkText, hoverText, command, SUGGEST_COMMAND, RED);
    }

    public static MutableText buildTeamList(DimensionalRegion dimCache, CommandConstants memberOrOwner) {
        Set<String> teamNames = getAssociateList(dimCache, memberOrOwner.toString(), TEAM.toString());
        String playerLangKeyPart = memberOrOwner == CommandConstants.OWNER ? "owners" : "members";
        MutableText teamList = MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.team"));
        if (teamNames.isEmpty()) {
            teamList.append(": ").append(MutableText.of(new TranslatableTextContent("cli.msg.dim.info." + playerLangKeyPart + ".teams.empty", dimCache.getDim().getValue())));
        }
        teamList.append(MutableText.of(new LiteralTextContent("\n")));
        teamNames.forEach(teamName -> {
            MutableText removeTeamLink = MutableText.of(new LiteralTextContent(" - "))
                    .append(buildDimRemoveTeamLink(teamName, dimCache.getDim(), memberOrOwner))
                    .append(MutableText.of(new LiteralTextContent(" '" + teamName + "'\n")));
            teamList.append(removeTeamLink);
        });
        return teamList;
    }

    public static Set<String> getAssociateList(AbstractRegion region, String affiliation, String playerOrTeam) {
        Set<String> associateNames = new HashSet<>();
        switch (affiliation) {
            case "owner":
                switch (playerOrTeam) {
                    case "player":
                        associateNames = new HashSet<>(region.getOwners().getPlayers().values());
                        break;
                    case "team":
                        associateNames = new HashSet<>(region.getOwners().getTeams());
                        break;
                }
                break;
            case "member":
                switch (playerOrTeam) {
                    case "player":
                        associateNames = new HashSet<>(region.getMembers().getPlayers().values());
                        break;
                    case "team":
                        associateNames = new HashSet<>(region.getMembers().getTeams());
                        break;
                }
                break;
            default:
                break;
        }
        return associateNames;
    }

    public static MutableText buildPlayerList(DimensionalRegion dimCache, CommandConstants memberOrOwner) {
        Set<String> playerNames = getAssociateList(dimCache, memberOrOwner.toString(), PLAYER.toString());
        MutableText playerList = MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.player")).append(": ");
        String playerLangKeyPart = memberOrOwner == CommandConstants.OWNER ? "owners" : "members";
        if (playerNames.isEmpty()) {
            return playerList.append(MutableText.of(new TranslatableTextContent("cli.msg.dim.info." + playerLangKeyPart + ".players.empty", dimCache.getDim().getValue())));
        }
        playerList.append(MutableText.of(new LiteralTextContent("\n")));
        playerNames.forEach(playerName -> {
            MutableText removePlayerLink = MutableText.of(new LiteralTextContent(" - "))
                    .append(buildDimRemovePlayerLink(playerName, dimCache.getDim(), memberOrOwner))
                    .append(MutableText.of(new LiteralTextContent(" '" + playerName + "'\n")));
            playerList.append(removePlayerLink);
        });
        return playerList;
    }

    public static MutableText buildRemoveFlagLink(IFlag flag, IMarkableRegion region) {
        String cmd = buildCommandStr(REGION.toString(), region.getDim().getValue().toString(), region.getName(), REMOVE.toString(), FLAG.toString(), flag.getFlagIdentifier());
        MutableText hoverText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.flag.remove.link.hover", flag.getFlagIdentifier(), region.getName()));
        MutableText linkText = MutableText.of(new TranslatableTextContent("cli.msg.info.region.flag.remove.link.text"));
        return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, RED);
    }

}
