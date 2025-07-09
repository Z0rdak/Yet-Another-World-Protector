package de.z0rdak.yawp.util.text.messages.pagination;

import de.z0rdak.yawp.api.commands.CommandConstants;
import de.z0rdak.yawp.core.area.TeleportAnchor;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.util.text.Messages;
import net.minecraft.ChatFormatting;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.api.commands.CommandConstants.*;
import static de.z0rdak.yawp.api.commands.Commands.buildCommandStr;
import static de.z0rdak.yawp.api.commands.Commands.buildListRegionFlagsCommand;
import static de.z0rdak.yawp.util.ChatComponentBuilder.*;
import static de.z0rdak.yawp.util.ChatLinkBuilder.*;
import static de.z0rdak.yawp.util.text.Messages.REMOVE_CMD_COLOR;
import static de.z0rdak.yawp.util.text.messages.pagination.RegionFlagPagination.buildRemoveFlagEntry;
import static net.minecraft.ChatFormatting.*;
import static net.minecraft.network.chat.ClickEvent.Action.RUN_COMMAND;

public class TeleportAnchorPagination extends BasePaginationMessage<TeleportAnchor> {

    private final IMarkableRegion region;

    public TeleportAnchorPagination(IMarkableRegion region, int pageNumber, int pageSize) throws InvalidPageNumberException {
        super(region.getTpAnchors().getAnchors(), buildListRegionFlagsCommand(region), pageNumber, pageSize);
        this.region = region;
    }

    public static MutableComponent buildTeleportAnchorInfoHeader(IProtectedRegion region, MutableComponent flagListLink) {
        return buildHeader(Component.translatableWithFallback("cli.msg.info.header.in", "== %s in %s ==", flagListLink, buildRegionInfoLink(region)));
    }

    public static List<Component> buildTeleportAnchorEntries(IMarkableRegion region, List<TeleportAnchor> selectedFlags) {
        List<TeleportAnchor> alphabeticAnchors = selectedFlags.stream()
                .sorted(Comparator.comparing(TeleportAnchor::getName))
                .toList();
        return alphabeticAnchors.stream()
                .map(tpAnchor -> buildRemoveTeleportAnchorEntry(region, tpAnchor))
                .collect(Collectors.toList());
    }

    /**
     * Builds a TextComponent for teleport anchor management
     * [x] [teleport-anchor name] @ [blockpos link] | [rename] [set]
     */
    public static Component buildRemoveTeleportAnchorEntry(IMarkableRegion region, TeleportAnchor tpAnchor) {

        String cmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), REMOVE.toString(), TP_ANCHOR.toString(), tpAnchor.getName());


        MutableComponent hoverText = Component.translatableWithFallback("cli.msg.info.region.tp-anchor.remove.link.hover", "Remove teleport anchor '%s' from region %s", tpAnchor.getName(), region.getName());
        MutableComponent linkText = Component.translatableWithFallback("cli.link.remove", "x");
        MutableComponent teleportAnchorRemoveLink = buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, REMOVE_CMD_COLOR);
        MutableComponent text = Component.translatableWithFallback("cli.msg.info.region.area.tp.block.link.text", "%s", shortBlockPos(tpAnchor.getPos()));
        MutableComponent tpLink = buildDimensionalBlockTpLink(region.getDim(), tpAnchor.getPos(), text);


        return Messages.substitutable(" - %s @ %s", teleportAnchorRemoveLink, tpLink);
    }

    @Override
    public Component noContentMsg() {
        return Component.translatableWithFallback("cli.msg.info.region.flag.empty", "No teleport anchors defined in %s", buildRegionInfoLink(region));
    }

    @Override
    public Component header() {
        return buildTeleportAnchorInfoHeader(this.region, buildRegionTeleportAnchorListLink(this.region));
    }

    @Override
    public List<Component> buildEntries() {
        return buildTeleportAnchorEntries(this.region, this.pageContent);
    }

    @Override
    public Component emptyEntry() {
        return Messages.substitutable(" - %s", buildSuggestTeleportAnchorLink(region, "tpAnchor", BlockPos.ZERO));
    }


}
