package de.z0rdak.yawp.util.text.messages.pagination;

import de.z0rdak.yawp.api.commands.Commands;
import de.z0rdak.yawp.core.area.TeleportAnchor;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.util.text.Messages;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;
import org.jetbrains.annotations.NotNull;

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
     * [x] teleport-anchor-name @ [blockpos link] | [rename] [set]
     */
    public static Component buildRemoveTeleportAnchorEntry(IMarkableRegion region, TeleportAnchor tpAnchor) {
        var teleportAnchorRemoveLink = buildRemoveTeleportAnchorLink(region, tpAnchor);

        var anchorNameText = Component.translatableWithFallback("cli.msg.info.region.tp-anchor.text", "%s", tpAnchor.getName());
        var anchorNameHover = Component.translatableWithFallback("cli.msg.info.region.tp-anchor.hover", "Teleport Anchor %s in at %s", tpAnchor.getName(), shortBlockPosBracketed(tpAnchor.getPos()));
        var anchorName = buildTextWithHoverMsg(anchorNameText, anchorNameHover, LIGHT_PURPLE);

        var tpText = Component.translatableWithFallback("cli.msg.info.region.tp-anchor.tp.link.text", "%s", shortBlockPos(tpAnchor.getPos()));
        var tphover = Component.translatableWithFallback("cli.msg.info.region.tp-anchor.tp.link.hover", "Click to teleport to %s in '%s'", tpAnchor.getName(), region.getName());
        var tpLink = buildDimensionalBlockTpLink(region.getDim(), tpAnchor.getPos(), tpText, tphover);

        var renameLink = buildRenameTeleportAnchorLink(region, tpAnchor);
        var updateLink = buildUpdateTeleportAnchorLink(region, tpAnchor);

        return Messages.substitutable(" - %s %s @ %s | %s %s",
                teleportAnchorRemoveLink, anchorName, tpLink, renameLink, updateLink);
    }

    public static Component buildRemoveTeleportAnchorLink(IMarkableRegion region, TeleportAnchor tpAnchor) {
        String rmCmd = Commands.buildRemoveTeleportAnchorCommand(region, tpAnchor.getName());
        var hover = Component.translatableWithFallback("cli.msg.info.region.tp-anchor.remove.link.hover", "Click to remove teleport anchor '%s' from region %s", tpAnchor.getName(), region.getName());
        var text = Component.translatableWithFallback("cli.link.remove", "x");
        return getTeleportAnchorRemoveLink(text, hover, rmCmd);
    }

    public static Component buildRenameTeleportAnchorLink(IMarkableRegion region, TeleportAnchor tpAnchor) {
        String renameCmd = Commands.buildSuggestRenameTpAnchorCommand(region, tpAnchor.getName());
        var hover = Component.translatableWithFallback("cli.msg.info.region.tp-anchor.rename.link.hover", "Click to rename teleport anchor '%s' in region %s", tpAnchor.getName(), region.getName());
        var text = Component.translatableWithFallback("cli.msg.info.region.tp-anchor.rename.link.text", "rename");
        return buildExecuteCmdComponent(text, hover, renameCmd, SUGGEST_COMMAND, LINK_COLOR);
    }

    public static Component buildUpdateTeleportAnchorLink(IMarkableRegion region, TeleportAnchor tpAnchor) {
        String renameCmd = Commands.buildSuggestUpdateTpAnchorCommand(region, tpAnchor.getName());
        var hover = Component.translatableWithFallback("cli.msg.info.region.tp-anchor.update.link.hover", "Click to set new teleport position for '%s' in region %s", tpAnchor.getName(), region.getName());
        var text = Component.translatableWithFallback("cli.msg.info.region.tp-anchor.update.link.text", "set");
        return buildExecuteCmdComponent(text, hover, renameCmd, SUGGEST_COMMAND, LINK_COLOR);
    }

        private static @NotNull MutableComponent getTeleportAnchorRemoveLink(MutableComponent rmText, MutableComponent rmHover, String rmCmd) {
        return buildExecuteCmdComponent(rmText, rmHover, rmCmd, RUN_COMMAND, REMOVE_CMD_COLOR);
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
        MutableComponent regionTpAnchorAmount = buildTextWithHoverMsg(Messages.substitutable("%s", region.getTpAnchors().getAnchors().size()), Component.translatableWithFallback("cli.msg.info.region.tp-anchor.number.hover", "%s teleport anchor(s) defined in", region.getTpAnchors().getAnchors().size(), region.getName()), LINK_COLOR);
        MutableComponent tpAnchorHoverText = Component.translatableWithFallback("cli.msg.info.region.tp-anchor.link.hover", "%s teleport anchor(s)", region.getName());
        String regionFlagListCmd = buildListRegionFlagsCommand(region);
        MutableComponent tpAnchorListLink = buildExecuteCmdComponent(regionTpAnchorAmount, tpAnchorHoverText, regionFlagListCmd, RUN_COMMAND, LINK_COLOR);
        MutableComponent tpAnchorComp = region.getTpAnchors().getAnchors().isEmpty() ? regionTpAnchorAmount : tpAnchorListLink;
        return Messages.substitutable("%s %s",
                Component.translatableWithFallback("cli.msg.info.region.tp-anchor.link.text", "%s teleport anchor(s)", tpAnchorComp),
                buildSuggestAddTeleportAnchorLink(region, "tpAnchor-name", BlockPos.ZERO));
    }

    public static MutableComponent buildSuggestAddTeleportAnchorLink(IMarkableRegion region, String name, BlockPos pos) {
        MutableComponent hoverText = Component.translatableWithFallback("cli.msg.info.region.tp-anchor.add.link.hover", "Click to create new teleport anchor in region %s", region.getName());
        MutableComponent linkText = Component.translatableWithFallback("cli.link.add", "+");
        String cmd = buildAddTeleportAnchorCommand(region, name, pos);
        return buildExecuteCmdComponent(linkText, hoverText, cmd, SUGGEST_COMMAND, ADD_CMD_COLOR);
    }


}
