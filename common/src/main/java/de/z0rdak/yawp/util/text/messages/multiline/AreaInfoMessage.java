package de.z0rdak.yawp.util.text.messages.multiline;

import de.z0rdak.yawp.core.area.IMarkableArea;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;

import java.util.ArrayList;
import java.util.List;

import static de.z0rdak.yawp.util.ChatComponentBuilder.*;
import static de.z0rdak.yawp.util.ChatLinkBuilder.*;

public class AreaInfoMessage implements MultiLineMessage<IMarkableArea> {

    private final IMarkableRegion region;
    private final IMarkableArea area;
    private final List<Component> regionInfoLines;

    public AreaInfoMessage(IMarkableRegion region) {
        this.region = region;
        this.area = region.getArea();
        this.regionInfoLines = new ArrayList<>();
    }

    @Override
    public IMarkableArea getContent() {
        return this.area;
    }

    /**
     * Prompt region area properties like teleport location and area.
     */
    @Override
    public List<Component> getLines() {
        // == Area for [<region>]  ==
        // Location: [region] @ [X,Y,Z]
        // AreaType: Cuboid, Size: X=69, Y=10, Z=42
        // Marked Blocks: [X,Y,Z], ..., [X,Y,Z]
        // Actions: [set area] [set TP] [show area] [<=expand=>] [<=max=>]
        regionInfoLines.clear();
        MutableComponent header = buildHeader(Component.translatableWithFallback("cli.msg.info.header.of", "== %s of %s ==", buildRegionAreaLink(region), buildRegionInfoLink(region)));
        MutableComponent location = buildInfoComponent("cli.msg.info.region.area.location", "Location", buildRegionTeleportLink(region, null));
        MutableComponent area = buildInfoComponent("cli.msg.info.region.area.area", "Area", buildRegionAreaDetailComponent(region));
        MutableComponent blocks = buildInfoComponent("cli.msg.info.region.area.marked", "Marked Blocks", buildAreaMarkedBlocksTpLinks(region));
        MutableComponent actions = buildInfoComponent("cli.msg.info.region.area.actions", "Actions", buildRegionAreaActionLinks(region));
        regionInfoLines.add(header);
        regionInfoLines.add(location);
        regionInfoLines.add(area);
        regionInfoLines.add(blocks);
        regionInfoLines.add(actions);
        return regionInfoLines;
    }

}
