package de.z0rdak.yawp.util.text.messages.multiline;

import de.z0rdak.yawp.core.area.visuals.DisplayType;
import de.z0rdak.yawp.core.area.IMarkableArea;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.util.text.Messages;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.resources.Identifier;

import java.util.ArrayList;
import java.util.List;

import static de.z0rdak.yawp.api.commands.Commands.*;
import static de.z0rdak.yawp.util.ChatComponentBuilder.*;
import static de.z0rdak.yawp.util.ChatLinkBuilder.*;
import static de.z0rdak.yawp.util.text.Messages.LINK_COLOR;
import static net.minecraft.network.chat.ClickEvent.Action.RUN_COMMAND;
import static net.minecraft.network.chat.ClickEvent.Action.SUGGEST_COMMAND;

public class VisualizationOptionsMessage implements MultiLineMessage<IMarkableArea> {

    private final IMarkableRegion region;
    private final List<Component> infoLines;

    public VisualizationOptionsMessage(IMarkableRegion region) {
        this.region = region;
        this.infoLines = new ArrayList<>();
    }

    /**
     * == Visualization for [region] == <br>
     * Show: [Hull] [Frame] [Minimal] [Marked] | [custom]
     * Hide: [Hull] [Frame] [Minimal] [Marked]
     * Hierarchy: [Show] | [Hide]
     * Intersecting: [Show] | [Hide]
     */
    @Override
    public List<Component> getLines() {
        infoLines.clear();
        var header = buildHeader(Component.translatableWithFallback("cli.msg.info.header.of", "== %s of %s ==", buildRegionVisualizationLink(region), buildRegionInfoLink(region)));
        var showSubject = Component.translatableWithFallback("cli.msg.info.region.visualization.show", "Show");
        var hideSubject = Component.translatableWithFallback("cli.msg.info.region.visualization.hide", "Hide");
        var hierarchySubject = Component.translatableWithFallback("cli.msg.info.region.visualization.hierarchy", "Hierarchy");
        var intersectingSubject = Component.translatableWithFallback("cli.msg.info.region.visualization.intersecting", "Intersecting");

        var customShowLink = buildShowAdvancedLink(region, DisplayType.FRAME, Identifier.withDefaultNamespace("cyan_stained_glass"), true, 15);
        var displayActions = buildInfoComponent(showSubject, buildShowLinks(region), customShowLink);
        var hideActions = buildInfoComponent(hideSubject, buildHideLinks(region));
        var hierarchy = buildInfoComponent(hierarchySubject, buildHierarchyShowLink(region), buildHierarchyHideLink(region));
        var intersecting = buildInfoComponent(intersectingSubject, buildIntersectingShowLink(region), buildIntersectingHideLink(region));

        infoLines.add(header);
        infoLines.add(displayActions);
        infoLines.add(hideActions);
        infoLines.add(hierarchy);
        infoLines.add(intersecting);
        return infoLines;
    }

    @Override
    public IMarkableArea getContent() {
        return region.getArea();
    }

    public static MutableComponent buildShowLinks(IMarkableRegion region) {
        var frameLink = buildShowLink(region, DisplayType.FRAME);
        var hullLink = buildShowLink(region, DisplayType.HULL);
        var minimalLink = buildShowLink(region, DisplayType.MINIMAL);
        var markedLink = buildShowLink(region, DisplayType.MARKED);
        return Messages.substitutable("%s %s %s %s", frameLink, hullLink, minimalLink, markedLink);
    }

    public static MutableComponent buildShowAdvancedLink(IMarkableRegion region, DisplayType displayType, Identifier block, boolean glow, int lightLevel) {
        var cmd = buildAdvancedVisualizationShowCommand(region, displayType, block, glow, lightLevel);
        var text = Component.translatableWithFallback("cli.msg.info.region.visualization.show.advanced.link.text", "custom");
        var hover = Component.translatableWithFallback("cli.msg.info.region.visualization.show.advanced.link.hover", "Click to paste custom visualization command for '%s'", displayType.name, region.getName());
        return buildExecuteCmdLinkWithBrackets(text, hover, cmd, SUGGEST_COMMAND, LINK_COLOR);
    }

    public static MutableComponent buildHideLinks(IMarkableRegion region) {
        var frameLink = buildHideLink(region, DisplayType.FRAME);
        var hullLink = buildHideLink(region, DisplayType.HULL);
        var minimalLink = buildHideLink(region, DisplayType.MINIMAL);
        var markedLink = buildHideLink(region, DisplayType.MARKED);
        return Messages.substitutable("%s %s %s %s", frameLink, hullLink, minimalLink, markedLink);
    }

    public static MutableComponent buildShowLink(IMarkableRegion region, DisplayType displayType) {
        var text = Component.translatableWithFallback("cli.msg.info.region.visualization.show.link.text", "%s", displayType.name);
        var hover = Component.translatableWithFallback("cli.msg.info.region.visualization.show.link.hover", "Click to show %s-Visualization for '%s'", displayType.name, region.getName());
        return buildExecuteCmdLinkWithBrackets(text, hover, buildVisualizationShowCommand(region, displayType), RUN_COMMAND, LINK_COLOR);
    }

    public static MutableComponent buildHideLink(IMarkableRegion region, DisplayType displayType) {
        var text = Component.translatableWithFallback("cli.msg.info.region.visualization.hide.link.text", "%s", displayType.name);
        var hover = Component.translatableWithFallback("cli.msg.info.region.visualization.hide.link.hover", "Click to hide %s-Visualization for '%s'", displayType.name, region.getName());
        return buildExecuteCmdLinkWithBrackets(text, hover, buildVisualizationHideCommand(region, displayType), RUN_COMMAND, LINK_COLOR);
    }

    public static MutableComponent buildHierarchyShowLink(IMarkableRegion region) {
        var text = Component.translatableWithFallback("cli.msg.info.region.visualization.hierarchy.show.link.text", "Show");
        var hover = Component.translatableWithFallback("cli.msg.info.region.visualization.hierarchy.show.link.hover", "Click to show visualization for all child regions of '%s'", region.getName());
        return buildExecuteCmdLinkWithBrackets(text, hover, buildVisualizationShowHierarchyCommand(region), RUN_COMMAND, LINK_COLOR);
    }

    public static MutableComponent buildHierarchyHideLink(IMarkableRegion region) {
        var text = Component.translatableWithFallback("cli.msg.info.region.visualization.hierarchy.hide.link.text", "Hide");
        var hover = Component.translatableWithFallback("cli.msg.info.region.visualization.hierarchy.hide.link.hover", "Click to hide visualization for all child regions of '%s'", region.getName());
        return buildExecuteCmdLinkWithBrackets(text, hover, buildVisualizationHideHierarchyCommand(region), RUN_COMMAND, LINK_COLOR);
    }

    public static MutableComponent buildIntersectingHideLink(IMarkableRegion region) {
        var text = Component.translatableWithFallback("cli.msg.info.region.visualization.intersecting.hide.link.text", "Hide");
        var hover = Component.translatableWithFallback("cli.msg.info.region.visualization.intersecting.hide.link.hover", "Click to hide visualization of intersecting regions for '%s'", region.getName());
        return buildExecuteCmdLinkWithBrackets(text, hover, buildVisualizationHideIntersectingCommand(region), RUN_COMMAND, LINK_COLOR);
    }

    public static MutableComponent buildIntersectingShowLink(IMarkableRegion region) {
        var text = Component.translatableWithFallback("cli.msg.info.region.visualization.intersecting.show.link.text", "Show");
        var hover = Component.translatableWithFallback("cli.msg.info.region.visualization.intersecting.show.link.hover", "Click to show visualization of intersecting regions for '%s'", region.getName());
        return buildExecuteCmdLinkWithBrackets(text, hover, buildVisualizationShowIntersectingCommand(region), RUN_COMMAND, LINK_COLOR);
    }





}
