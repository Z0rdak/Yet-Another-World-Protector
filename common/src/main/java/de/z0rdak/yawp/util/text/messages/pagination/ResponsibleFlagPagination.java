package de.z0rdak.yawp.util.text.messages.pagination;

import de.z0rdak.yawp.api.commands.CommandConstants;
import de.z0rdak.yawp.core.flag.FlagMessage;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.util.ChatLinkBuilder;
import de.z0rdak.yawp.util.text.Messages;
import net.minecraft.ChatFormatting;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;

import java.util.*;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.api.commands.CommandConstants.FLAG;
import static de.z0rdak.yawp.api.commands.CommandConstants.REMOVE;
import static de.z0rdak.yawp.api.commands.Commands.buildCommandStr;
import static de.z0rdak.yawp.api.commands.Commands.buildListFlagsCommand;
import static de.z0rdak.yawp.handler.HandlerUtil.getFlagMapRecursive;
import static de.z0rdak.yawp.util.ChatComponentBuilder.buildExecuteCmdComponent;
import static de.z0rdak.yawp.util.ChatComponentBuilder.buildHeader;
import static de.z0rdak.yawp.util.text.Messages.REMOVE_CMD_COLOR;
import static de.z0rdak.yawp.util.text.messages.pagination.RegionFlagPagination.buildFlagQuickActionComponent;
import static net.minecraft.ChatFormatting.*;
import static net.minecraft.network.chat.ClickEvent.Action.RUN_COMMAND;

public class ResponsibleFlagPagination extends BasePaginationMessage<FlagMessage.FlagCorrelation> {

    private final IProtectedRegion region;

    public ResponsibleFlagPagination(IProtectedRegion region, int pageNumber, int pageSize) throws InvalidPageNumberException {
        super(getCorrelations(region), buildListFlagsCommand(region), pageNumber, pageSize);
        this.region = region;
    }

    public static MutableComponent buildRegionFlagInfoHeader(IProtectedRegion region, MutableComponent flagListLink) {
        return buildHeader(Component.translatableWithFallback("cli.msg.info.header.in", "== %s in %s ==", flagListLink, ChatLinkBuilder.buildRegionInfoLink(region)));
    }

    public static List<Component> buildRegionFlagEntries(IProtectedRegion region) {
        return buildRegionFlagEntries(region, region.getFlagContainer().values().stream().toList());
    }

    public static List<Component> buildRegionFlagEntries(IProtectedRegion region, List<IFlag> selectedFlags) {
        List<Component> flagEntries = new ArrayList<>();
        flagEntries.addAll(buildFlagEntriesForState(region, selectedFlags, FlagState.DENIED));
        flagEntries.addAll(buildFlagEntriesForState(region, selectedFlags, FlagState.ALLOWED));
        flagEntries.addAll(buildFlagEntriesForState(region, selectedFlags, FlagState.DISABLED));
        return flagEntries;
    }

    public static List<MutableComponent> buildFlagEntriesForState(IProtectedRegion region, List<IFlag> selectedFlags, FlagState state) {
        List<IFlag> flagsByState = selectedFlags.stream()
                .filter(f -> f.getState() == state)
                .sorted(Comparator.comparing(IFlag::getName))
                .toList();
        return flagsByState.stream()
                .map(flag -> buildRemoveFlagEntry(region, flag, colorForState(flag.getState())))
                .collect(Collectors.toList());
    }

    /**
     * Creates a TextComponent for flag removal, followed by the flag infos
     */
    public static MutableComponent buildRemoveFlagEntry(IProtectedRegion region, IFlag flag, ChatFormatting flagLinkColor, ChatFormatting... ChatFormattings) {
        // [x] [flagname] [<region-indicator] [] []
        String cmd;
        switch (region.getRegionType()) {
            case GLOBAL: {
                cmd = buildCommandStr(CommandConstants.GLOBAL.toString(), REMOVE.toString(), FLAG.toString(), flag.getName());
                break;
            }
            case DIMENSION: {
                cmd = buildCommandStr(CommandConstants.DIM.toString(), region.getDim().location().toString(), REMOVE.toString(), FLAG.toString(), flag.getName());
                break;
            }
            case LOCAL: {
                cmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), REMOVE.toString(), FLAG.toString(), flag.getName());
                break;
            }
            default:
                throw new IllegalArgumentException();
        }
        MutableComponent hoverText = Component.translatableWithFallback("cli.msg.info.region.flag.remove.link.hover", "Remove flag '%s' from region %s", flag.getName(), region.getName());
        MutableComponent linkText = Component.translatableWithFallback("cli.link.remove", "x");
        MutableComponent flagRemoveLink = buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, REMOVE_CMD_COLOR);
        MutableComponent flagQuickActionComponent = buildFlagQuickActionComponent(region, flag, flagLinkColor);
        flagQuickActionComponent.withStyle(ChatFormattings);
        return Messages.substitutable(" - %s %s", flagRemoveLink, flagQuickActionComponent);
    }

    public static ChatFormatting colorForState(FlagState state) {
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

    /**
     * Show flags for region, with a color of their state
     * But also show parent flags in italic
     * How do we want to handle flags defined in the region but also in the parent?
     * If the child flag is dominant, we display the child flag, and add a hint to the parent
     * If the flag is overriden, we format them with strikethrough and add a link to the parent which overrides it
     */
    public static List<MutableComponent> buildFlagEntries(IProtectedRegion region) {
        List<MutableComponent> flagEntries = new ArrayList<>();
        Map<String, FlagMessage.FlagCorrelation> flagMapRecursive = getFlagMapRecursive(region, null);
        Map<FlagState, List<FlagMessage.FlagCorrelation>> flagStateListMap = sortFlagsByState(flagMapRecursive);
        flagEntries.addAll(buildFlagEntries(flagStateListMap, FlagState.ALLOWED));
        flagEntries.addAll(buildFlagEntries(flagStateListMap, FlagState.DENIED));
        flagEntries.addAll(buildFlagEntries(flagStateListMap, FlagState.DISABLED));
        return flagEntries;
    }

    public static List<Component> buildFlagEntries(List<FlagMessage.FlagCorrelation> flags) {
        return flags.stream()
                .map(fc -> buildRemoveFlagEntry(fc.getRegion(), fc.getFlag(), colorForState(fc.getFlag().getState())))
                .collect(Collectors.toList());
    }

    public static List<MutableComponent> buildFlagEntries(Map<FlagState, List<FlagMessage.FlagCorrelation>> flagStateListMap, FlagState state) {
        List<FlagMessage.FlagCorrelation> flagsByState = flagStateListMap.get(state);
        flagsByState.sort(Comparator.comparing(flagCorrelation -> flagCorrelation.getFlag().getName()));
        return flagsByState.stream()
                .map(flagCorrelation -> buildRemoveFlagEntry(flagCorrelation.getRegion(), flagCorrelation.getFlag(), colorForState(state)))
                .collect(Collectors.toList());
    }

    public static Map<FlagState, List<IFlag>> sortFlagsByState(IProtectedRegion region) {
        HashMap<FlagState, List<IFlag>> flagStateListMap = new HashMap<>();
        flagStateListMap.put(FlagState.DENIED, region.getFlagContainer().getFlags(FlagState.DENIED));
        flagStateListMap.put(FlagState.ALLOWED, region.getFlagContainer().getFlags(FlagState.ALLOWED));
        flagStateListMap.put(FlagState.DISABLED, region.getFlagContainer().getFlags(FlagState.DISABLED));
        return flagStateListMap;
    }

    public static Map<FlagState, List<FlagMessage.FlagCorrelation>> sortFlagsByState(Map<String, FlagMessage.FlagCorrelation> flagMap) {
        HashMap<FlagState, List<FlagMessage.FlagCorrelation>> flagStateListMap = new HashMap<>();
        flagStateListMap.put(FlagState.DENIED, getCorrelationByState(flagMap, FlagState.DENIED));
        flagStateListMap.put(FlagState.ALLOWED, getCorrelationByState(flagMap, FlagState.ALLOWED));
        flagStateListMap.put(FlagState.DISABLED, getCorrelationByState(flagMap, FlagState.DISABLED));
        return flagStateListMap;
    }

    public static List<FlagMessage.FlagCorrelation> getCorrelations(IProtectedRegion region) {
        Map<String, FlagMessage.FlagCorrelation> flagMap = getFlagMapRecursive(region, null);
        return flagMap.values().stream()
                .filter(c -> c.getFlag() != null)
                .sorted(Comparator.comparing(c -> c.getFlag().getState()))
                .sorted(Comparator.comparing(c -> c.getFlag().getName()))
                .collect(Collectors.toList());
    }

    private static List<FlagMessage.FlagCorrelation> getCorrelationByState(Map<String, FlagMessage.FlagCorrelation> flagMap, FlagState state) {
        return flagMap.values().stream()
                .filter(c -> c.getFlag() != null) // TODO: ??
                .filter(c -> c.getFlag().getState() == state)
                .sorted(Comparator.comparing(c -> c.getFlag().getName()))
                .collect(Collectors.toList());
    }

    @Override
    public Component noContentMsg() {
        return Component.translatableWithFallback("cli.msg.info.region.flag.empty", "No flags defined in %s", ChatLinkBuilder.buildRegionInfoLink(region));
    }

    @Override
    public Component header() {
        return buildRegionFlagInfoHeader(this.region, ChatLinkBuilder.buildRegionFlagListLink(this.region));
    }

    @Override
    public List<Component> buildEntries() {
        return buildFlagEntries(this.pageContent);
    }

    @Override
    public Component emptyEntry() {
        return Messages.substitutable(" - %s", ChatLinkBuilder.buildSuggestAddFlagLink(region));
    }


}
