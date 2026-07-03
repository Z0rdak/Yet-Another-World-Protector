package de.z0rdak.yawp.util.text.messages.pagination;

import de.z0rdak.yawp.api.commands.Commands;
import de.z0rdak.yawp.core.area.anchors.TeleportAnchor;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.util.text.Messages;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;

import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.api.commands.Commands.*;
import static de.z0rdak.yawp.util.ChatComponentBuilder.*;
import static de.z0rdak.yawp.util.ChatLinkBuilder.*;
import static de.z0rdak.yawp.util.text.Messages.*;
import static net.minecraft.ChatFormatting.*;
import static net.minecraft.network.chat.ClickEvent.Action.RUN_COMMAND;
import static net.minecraft.network.chat.ClickEvent.Action.SUGGEST_COMMAND;

public class TeleportAnchorPagination extends BasePaginationMessage<TeleportAnchor> {

    private final IMarkableRegion region;

    public TeleportAnchorPagination(IMarkableRegion region, int pageNumber, int pageSize) throws InvalidPageNumberException {
        super(region.getTpAnchors().getAnchors(), buildListRegionFlagsCommand(region), pageNumber, pageSize);
        this.region = region;
    }

    public static MutableComponent buildTeleportAnchorInfoHeader(IProtectedRegion region, MutableComponent flagListLink) {
        return buildHeader(Component.translatableWithFallback("cli.msg.info.header.in", "== %s in %s ==", flagListLink, buildRegionInfoLink(region)));
    }

    public static List<Component> buildTeleportAnchorEntries(IMarkableRegion region, List<TeleportAnchor> tpAnchors) {
        List<TeleportAnchor> alphabeticAnchors = tpAnchors.stream()
                .sorted(Comparator.comparing(TeleportAnchor::getName))
                .toList();
        return alphabeticAnchors.stream()
                .map(tpAnchor -> buildRemoveTeleportAnchorEntry(region, tpAnchor))
                .collect(Collectors.toList());
    }

    /**
     * Builds a TextComponent for teleport anchor management
     * [x] teleport-anchor-name @ [teleport link] | [show] [hide] [rename] [set]
     */
    public static Component buildRemoveTeleportAnchorEntry(IMarkableRegion region, TeleportAnchor tpAnchor) {
        var teleportAnchorRemoveLink = buildRemoveTeleportAnchorLink(region, tpAnchor);
        var anchorNameText = Component.translatableWithFallback("cli.msg.info.region.tp-anchor.text", "%s", tpAnchor.getName());
        var anchorNameHover = Component.translatableWithFallback("cli.msg.info.region.tp-anchor.hover", "Anchor '%s' @ %s", tpAnchor.getName(), shortBlockPosBracketed(tpAnchor.getPos()));
        var anchorName = buildTextWithHoverMsg(anchorNameText, anchorNameHover, GREEN);

        var tpLink = buildTeleportToAnchorLink(region, tpAnchor);

        var showLink = buildShowTeleportAnchorLink(region, tpAnchor);
        var hideLink = buildHideTeleportAnchorLink(region, tpAnchor);
        var renameLink = buildRenameTeleportAnchorLink(region, tpAnchor);
        var updateLink = buildUpdateTeleportAnchorLink(region, tpAnchor);

        return Messages.substitutable(" - %s %s @ %s | %s %s %s %s",
                teleportAnchorRemoveLink, anchorName, tpLink, showLink, hideLink, updateLink, renameLink);
    }

    public static Component buildShowTeleportAnchorLink(IMarkableRegion region, TeleportAnchor tpAnchor) {
        var cmd = buildShowTpAnchorCommand(region, tpAnchor.getName());
        var text = Component.translatableWithFallback("cli.msg.info.region.tp-anchor.show.link.text", "s");
        var hover = Component.translatableWithFallback("cli.msg.info.region.tp-anchor.show.link.hover", "Click to show teleport anchor");
        return buildExecuteCmdLinkWithBrackets(text, hover, cmd, RUN_COMMAND, LINK_COLOR);
    }

    public static Component buildHideTeleportAnchorLink(IMarkableRegion region, TeleportAnchor tpAnchor) {
        var cmd = buildHideTpAnchorCommand(region, tpAnchor.getName());
        var text = Component.translatableWithFallback("cli.msg.info.region.tp-anchor.hide.link.text", "h");
        var hover = Component.translatableWithFallback("cli.msg.info.region.tp-anchor.hide.link.hover", "Click to hide teleport anchor");
        return buildExecuteCmdLinkWithBrackets(text, hover, cmd, RUN_COMMAND, LINK_COLOR);
    }

    public static Component buildRemoveTeleportAnchorLink(IMarkableRegion region, TeleportAnchor tpAnchor) {
        var rmCmd = buildRemoveTeleportAnchorCommand(region, tpAnchor.getName());
        var hover = Component.translatableWithFallback("cli.msg.info.region.tp-anchor.remove.link.hover", "Click to remove '%s' from region %s", tpAnchor.getName(), region.getName());
        var text = Component.translatableWithFallback("cli.link.remove", "x");
        return buildExecuteCmdComponent(text, hover, rmCmd, RUN_COMMAND, REMOVE_CMD_COLOR);
    }

    public static Component buildTeleportToAnchorLink(IMarkableRegion region, TeleportAnchor tpAnchor) {
        var cmd = buildTeleportTpAnchorCommand(region, tpAnchor.getName());
        var text = Component.translatableWithFallback("cli.msg.info.region.tp-anchor.tp.link.text", "%s", commandBlockPosStr(tpAnchor.getPos()));
        var hover = Component.translatableWithFallback("cli.msg.info.region.tp-anchor.tp.link.hover", "Click to teleport to '%s' @ %s", tpAnchor.getName(), shortBlockPosBracketed(tpAnchor.getPos()));
        return buildExecuteCmdLinkWithBrackets(text, hover, cmd, RUN_COMMAND, LINK_COLOR);
    }

    public static Component buildRenameTeleportAnchorLink(IMarkableRegion region, TeleportAnchor tpAnchor) {
        String renameCmd = Commands.buildSuggestRenameTpAnchorCommand(region, tpAnchor.getName());
        var hover = Component.translatableWithFallback("cli.msg.info.region.tp-anchor.rename.link.hover", "Click to rename '%s'", tpAnchor.getName());
        var text = Component.translatableWithFallback("cli.msg.info.region.tp-anchor.rename.link.text", "r");
        return buildExecuteCmdLinkWithBrackets(text, hover, renameCmd, SUGGEST_COMMAND, LINK_COLOR);
    }

    public static Component buildUpdateTeleportAnchorLink(IMarkableRegion region, TeleportAnchor tpAnchor) {
        String renameCmd = Commands.buildSuggestUpdateTpAnchorCommand(region, tpAnchor.getName());
        var hover = Component.translatableWithFallback("cli.msg.info.region.tp-anchor.update.link.hover", "Click to set new teleport position");
        var text = Component.translatableWithFallback("cli.msg.info.region.tp-anchor.update.link.text", "p");
        return buildExecuteCmdLinkWithBrackets(text, hover, renameCmd, SUGGEST_COMMAND, LINK_COLOR);
    }

    @Override
    public Component noContentMsg() {
        return Component.translatableWithFallback("cli.msg.info.region.tp-anchor.empty", "No teleport anchors defined in %s", buildRegionInfoLink(region));
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
        return Messages.substitutable(" - %s", buildSuggestAddTeleportAnchorLink(region, "tpAnchor", BlockPos.ZERO));
    }

    /**
     * [m] teleport anchor(s) [+]
     */
    public static MutableComponent buildRegionTeleportAnchorListLink(IMarkableRegion region) {
        MutableComponent numberHover = Component.translatableWithFallback("cli.msg.info.region.tp-anchor.link.hover", "%s teleport anchor(s) defined in %s", region.getTpAnchors().getAnchors().size(), region.getName());
        MutableComponent regionTpAnchorAmountPlain = buildTextWithHoverMsg(Messages.substitutable("%s", region.getTpAnchors().getAnchors().size()), numberHover, LINK_COLOR);
        MutableComponent regionTpAnchorAmount = buildTextWithHoverMsg(Messages.substitutable("%s", region.getTpAnchors().getAnchors().size()), numberHover, LINK_COLOR);
        MutableComponent tpAnchorHoverText = Component.translatableWithFallback("cli.msg.info.region.tp-anchor.link.hover", "%s teleport anchor(s) defined in %s", region.getTpAnchors().getAnchors().size(), region.getName());
        String listAnchorCmd = buildListTeleportAnchorCommand(region);
        MutableComponent tpAnchorListLink = buildExecuteCmdComponent(regionTpAnchorAmount, tpAnchorHoverText, listAnchorCmd, RUN_COMMAND, LINK_COLOR);
        MutableComponent tpAnchorComp = region.getTpAnchors().getAnchors().isEmpty() ? regionTpAnchorAmountPlain : tpAnchorListLink;
        return Messages.substitutable("%s %s",
                Component.translatableWithFallback("cli.msg.info.region.tp-anchor.link.text", "%s teleport anchor(s)", tpAnchorComp),
                buildSuggestAddTeleportAnchorLink(region, "tpAnchor-name", BlockPos.ZERO));
    }

    public static MutableComponent buildSuggestAddTeleportAnchorLink(IMarkableRegion region, String name, BlockPos pos) {
        MutableComponent hoverText = Component.translatableWithFallback("cli.msg.info.region.tp-anchor.add.link.hover", "Click to create new teleport anchor in %s", region.getName());
        MutableComponent linkText = Component.translatableWithFallback("cli.link.add", "+");
        String cmd = buildAddTeleportAnchorCommand(region, name, pos);
        return buildExecuteCmdComponent(linkText, hoverText, cmd, SUGGEST_COMMAND, ADD_CMD_COLOR);
    }


}
