package de.z0rdak.yawp.util.text.messages.pagination;

import de.z0rdak.yawp.api.commands.CommandConstants;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.util.text.Messages;
import net.minecraft.ChatFormatting;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.api.commands.CommandConstants.FLAG;
import static de.z0rdak.yawp.api.commands.CommandConstants.REMOVE;
import static de.z0rdak.yawp.api.commands.Commands.buildCommandStr;
import static de.z0rdak.yawp.api.commands.Commands.buildListRegionFlagsCommand;
import static de.z0rdak.yawp.util.ChatComponentBuilder.buildExecuteCmdComponent;
import static de.z0rdak.yawp.util.ChatComponentBuilder.buildHeader;
import static de.z0rdak.yawp.util.ChatLinkBuilder.*;
import static de.z0rdak.yawp.util.text.Messages.REMOVE_CMD_COLOR;
import static net.minecraft.ChatFormatting.*;
import static net.minecraft.network.chat.ClickEvent.Action.RUN_COMMAND;

public class RegionFlagPagination extends BasePaginationMessage<IFlag> {

    private final IProtectedRegion region;

    public RegionFlagPagination(IProtectedRegion region, int pageNumber, int pageSize) throws InvalidPageNumberException {
        super(region.getFlags().flags(), buildListRegionFlagsCommand(region), pageNumber, pageSize);
        this.region = region;
    }

    public static MutableComponent buildRegionFlagInfoHeader(IProtectedRegion region, MutableComponent flagListLink) {
        return buildHeader(Component.translatableWithFallback("cli.msg.info.header.in", "== %s in %s ==", flagListLink, buildRegionInfoLink(region)));
    }

    public static List<Component> buildRegionFlagEntries(IProtectedRegion region) {
        return buildRegionFlagEntries(region, region.getFlags().flags());
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
     * Creates a TextComponent with a Link for displaying the flag info. <br>
     * Text: [flagname] [regionTypeIdentifier] [s] [m] [o] <br>
     * Where <br>
     * - [s] is a suggest link to change the flag state, <br>
     * - [m] is a quick link to toggle the flag mute state, <br>
     * - [o] is a quick link to toggle the flag override state, <br>
     *
     * @param region
     * @param flag
     * @return text component for quick flag actions [flagname] [regionTypeIdentifier] [s] [m] [o]
     */
    public static MutableComponent buildFlagQuickActionComponent(IProtectedRegion region, IFlag flag, ChatFormatting flagLinkColor) {
        MutableComponent regionTypeIndicator = Component.literal(region.getRegionType().type.substring(0, 1).toUpperCase());
        MutableComponent hoverText = Component.translatableWithFallback("cli.flag.info.hover", "Show %s flag info of region '%s'", flag.getName(), region.getName());
        MutableComponent flagInfoLink = buildFlagInfoLink(region, flag, flagLinkColor);
        return Messages.substitutable("%s %s %s %s %s",
                flagInfoLink,
                buildFlagInfoLink(region, flag, regionTypeIndicator, hoverText, DARK_PURPLE),
                buildFlagStateSuggestionLink(region, flag),
                buildFlagMuteToggleLink(region, flag, true),
                buildFlagOverrideToggleLink(region, flag, true));
    }

    @Override
    public Component noContentMsg() {
        return Component.translatableWithFallback("cli.msg.info.region.flag.empty", "No flags defined in %s", buildRegionInfoLink(region));
    }

    @Override
    public Component header() {
        return buildRegionFlagInfoHeader(this.region, buildRegionFlagListLink(this.region));
    }

    @Override
    public List<Component> buildEntries() {
        return buildRegionFlagEntries(this.region, this.pageContent);
    }

    @Override
    public Component emptyEntry() {
        return Messages.substitutable(" - %s", buildSuggestAddFlagLink(region));
    }


}
