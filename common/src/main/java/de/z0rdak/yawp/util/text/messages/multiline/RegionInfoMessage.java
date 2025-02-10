package de.z0rdak.yawp.util.text.messages.multiline;

import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.core.region.RegionType;
import de.z0rdak.yawp.data.region.DimensionRegionCache;
import de.z0rdak.yawp.data.region.RegionDataManager;
import de.z0rdak.yawp.util.text.Messages;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtUtils;
import net.minecraft.network.chat.ClickEvent;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;

import java.util.ArrayList;
import java.util.List;

import static de.z0rdak.yawp.util.ChatComponentBuilder.*;
import static de.z0rdak.yawp.util.ChatLinkBuilder.*;
import static de.z0rdak.yawp.util.text.messages.multiline.RegionStateMessage.buildRegionStateLink;
import static net.minecraft.ChatFormatting.GOLD;

public class RegionInfoMessage implements MultiLineMessage<IProtectedRegion> {

    private final IProtectedRegion region;
    private final List<Component> regionInfoLines;

    public RegionInfoMessage(IProtectedRegion region) {
        this.region = region;
        this.regionInfoLines = new ArrayList<>();
    }

    private final static int MAX_ENCODER_LIMIT = 30000;
    
    public static MutableComponent buildRegionOverviewHeader(IProtectedRegion region) {
        CompoundTag regionNbt = region.serializeNBT();
        String nbtClipBoardText = regionNbt.sizeInBytes() > MAX_ENCODER_LIMIT 
                ? "Sorry, region data is too big. I am working on a fix." 
                : NbtUtils.prettyPrint(regionNbt, true);
        switch (region.getRegionType()) {
            case GLOBAL: {
                MutableComponent dumpLinkText = Component.translatableWithFallback("cli.msg.global.overview.header.dump.link.text", "Global overview");
                MutableComponent dumpLinkHover = Component.translatableWithFallback("cli.msg.global.overview.header.dump.link.hover", "Copy Global Region NBT to clipboard");
                MutableComponent clipBoardDumpLink = buildExecuteCmdComponent(dumpLinkText, dumpLinkHover, nbtClipBoardText, ClickEvent.Action.COPY_TO_CLIPBOARD, GOLD);
                return buildHeader(Component.translatableWithFallback("cli.msg.info.header.for", "== %s for %s ==", clipBoardDumpLink, buildRegionInfoLink(region)));
            }
            case DIMENSION: {
                MutableComponent dumpLinkText = Component.translatableWithFallback("cli.msg.dim.overview.header.dump.link.text", "Dimension overview");
                MutableComponent dumpLinkHover = Component.translatableWithFallback("cli.msg.dim.overview.header.dump.link.hover", "Copy Dimensional Region NBT to clipboard");
                MutableComponent clipBoardDumpLink = buildExecuteCmdComponent(dumpLinkText, dumpLinkHover, nbtClipBoardText, ClickEvent.Action.COPY_TO_CLIPBOARD, GOLD);
                return buildHeader(Component.translatableWithFallback("cli.msg.info.header.for", "== %s for %s ==", clipBoardDumpLink, buildRegionInfoLink(region)));
            }
            case LOCAL: {
                MutableComponent dumpLinkText = Component.translatableWithFallback("cli.msg.local.overview.header.dump.link.text", "Region overview");
                MutableComponent dumpLinkHover = Component.translatableWithFallback("cli.msg.local.overview.header.dump.link.hover", "Copy Local Region NBT to clipboard");
                MutableComponent clipBoardDumpLink = buildExecuteCmdComponent(dumpLinkText, dumpLinkHover, nbtClipBoardText, ClickEvent.Action.COPY_TO_CLIPBOARD, GOLD);
                return buildHeader(Component.translatableWithFallback("cli.msg.info.header.for", "== %s for %s ==", clipBoardDumpLink, buildRegionInfoLink(region)));
            }
            default:
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        }
    }

    public static MutableComponent buildFlagsListLink(IProtectedRegion region) {
        if (region.getRegionType() == RegionType.GLOBAL) {
            return buildRegionFlagListLink(region);
        }
        return Messages.substitutable("%s | %s", buildResponsibleFlagListLink(region), buildRegionFlagListLink(region));
    }

    public static MutableComponent buildGroupLinks(IProtectedRegion region) {
        return getGroupsForRegion(region).stream()
                .map(group -> buildGroupLink(region, group, getGroupSize(region, group)))
                .reduce(Component.literal(""), (link1, link2) -> link1.append(" ").append(link2));
    }

    private static MutableComponent buildRegionHierarchyComponent(IProtectedRegion region) {
        MutableComponent listChildrenLink = buildRegionListChildrenLink(region);
        switch (region.getRegionType()) {
            case GLOBAL: {
                // Dimensions: [n dimensions(s)]
                return buildInfoComponent("cli.msg.info.dimensions", "Dimensions", listChildrenLink);
            }
            case DIMENSION: {
                // Parent: [global], [n children], [n regions] [+]
                MutableComponent globalRegionLink = buildRegionInfoLink(region.getParent(), Component.translatableWithFallback("cli.msg.info.region.global.link.hover", "Show global region info"));
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(region.getDim());
                MutableComponent hierarchyLinks = Messages.substitutable("%s, %s, %s", globalRegionLink, buildDimRegionsLink(dimCache), listChildrenLink);
                return buildInfoComponent("cli.msg.info.region.hierarchy", "Hierarchy", hierarchyLinks);
            }
            case LOCAL: {
                // Parent: [parent] [x], [n children] [+]
                MutableComponent parentRemoveThisChildLink = buildRegionRemoveChildLink(region.getParent(), region);
                MutableComponent hierarchyLinks = Component.literal("");
                if (region.getParent().getRegionType() == RegionType.DIMENSION) {
                    // don't show removal link, since it's not possible to remove the parent
                    hierarchyLinks = Messages.substitutable("%s, %s", buildRegionInfoLink(region.getParent()), listChildrenLink);
                }
                if (region.getParent().getRegionType() == RegionType.LOCAL) {
                    hierarchyLinks = Messages.substitutable("%s %s, %s", buildRegionInfoLink(region.getParent()), parentRemoveThisChildLink, listChildrenLink);
                }
                return buildInfoComponent("cli.msg.info.region.parent", "Parent", hierarchyLinks);
            }
            default:
                throw new IllegalStateException("Unexpected value: " + region.getRegionType());
        }
    }

    @Override
    public IProtectedRegion getContent() {
        return this.region;
    }

    @Override
    public List<Component> getLines() {
        // == Region [<name>] overview ==
        MutableComponent header = buildRegionOverviewHeader(region);
        regionInfoLines.add(header);

        // Flags: [n] | [m] flag(s)] [+]
        MutableComponent flagsText = buildInfoComponent("cli.msg.info.region.flag", "Flags", buildFlagsListLink(region));
        regionInfoLines.add(flagsText);

        if (region.getRegionType() == RegionType.LOCAL) {
            // Area: [Area]
            MutableComponent areaText = buildInfoComponent("cli.msg.info.region.area", "Area", buildRegionAreaLink((IMarkableRegion) region));
            regionInfoLines.add(areaText);
        }

        // Groups: [owners], [members], [<listGroups>]
        MutableComponent groupsText = buildInfoComponent("cli.msg.info.region.group", "Groups", buildGroupLinks(region));
        regionInfoLines.add(groupsText);

        // Regions: [global], [n children], [n regions][+],
        // Dimensions: [n dimensions(s)]
        // Parent: [parent][x], [n children][+]
        MutableComponent hierarchy = buildRegionHierarchyComponent(region);
        regionInfoLines.add(hierarchy);

        // State: [State]
        MutableComponent state = buildInfoComponent("cli.msg.info.region.state", "State", buildRegionStateLink(region));
        regionInfoLines.add(state);

        return regionInfoLines;
    }
}
