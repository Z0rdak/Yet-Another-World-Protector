package de.z0rdak.yawp.util.text.messages.multiline;

import de.z0rdak.yawp.core.area.BlockDisplayProperties;
import de.z0rdak.yawp.core.area.TeleportAnchors;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.util.text.Messages;
import net.minecraft.network.chat.Component;

import java.util.ArrayList;
import java.util.List;

import static de.z0rdak.yawp.util.ChatComponentBuilder.*;
import static de.z0rdak.yawp.util.ChatComponentBuilder.buildInfoComponent;
import static de.z0rdak.yawp.util.ChatLinkBuilder.*;
import static de.z0rdak.yawp.util.ChatLinkBuilder.buildDisplayHideActionsLink;

public class DisplaySettingsMessage implements MultiLineMessage<BlockDisplayProperties> {

    private final IMarkableRegion region;
    private final List<Component> infoLines;

    public DisplaySettingsMessage(IMarkableRegion region) {
        this.region = region;
        this.infoLines = new ArrayList<>();
    }

    /**
     *  == Display Settings for [region]  == <br>
     * Block: [minecraft:cyan_stained_glass] | [set] <br>
     * Glow: enabled | [on] [off] <br>
     * Light-Level: 15 | [set] <br>
     * Show: [Hull] [Frame] [Teleports] [Marked] | [Hierarchy] [Intersecting]  ????
     * Hide: [Hull] [Frame] [Teleports] [Marked] | [Hierarchy] [Intersecting]
     */
    @Override
    public List<Component> getLines() {
        infoLines.clear();
        var header = buildHeader(Component.translatableWithFallback("cli.msg.info.header.of", "== %s of %s ==", buildRegionDisplaySettingsLink(region), buildRegionInfoLink(region)));
        var blockSubject = Component.translatableWithFallback("cli.msg.info.region.display.block", "Block");
        var glowSubject = Component.translatableWithFallback("cli.msg.info.region.display.glow", "Glow");
        var lightLevelSubject = Component.translatableWithFallback("cli.msg.info.region.display.light-level", "Light-Level");
        var showSubject = Component.translatableWithFallback("cli.msg.info.region.display.show", "Show");
        var hideSubject = Component.translatableWithFallback("cli.msg.info.region.display.hide", "Hide");

        var blockComponent = buildInfoComponent(blockSubject, buildDisplayBlockInfo(region), buildSetDisplayBlockLink(region));
        var glowActions = Messages.substitutable("%s %s", buildSetDisplayGlowOnLink(region), buildSetDisplayGlowOffLink(region));
        var glowComponent = buildInfoComponent(glowSubject, buildDisplayGlowInfo(region), glowActions);
        var lightLevelComponent = buildInfoComponent(lightLevelSubject, buildDisplayLightLevelInfo(region), buildSetDisplayLightLevelLink(region));
        var displayActions = buildInfoComponent(showSubject, buildDisplayShowActionLinks(region), buildDisplayShowAdvancedActionLinks(region));
        var hideActions = buildInfoComponent(hideSubject, buildDisplayHideActionsLink(region), buildDisplayHideAdvancedActionsLink(region));

        infoLines.add(header);
        infoLines.add(blockComponent);
        infoLines.add(glowComponent);
        infoLines.add(lightLevelComponent);
        infoLines.add(displayActions);
        infoLines.add(hideActions);
        return infoLines;
    }

    @Override
    public BlockDisplayProperties getContent() {
        return region.getArea().getDisplay();
    }
}
