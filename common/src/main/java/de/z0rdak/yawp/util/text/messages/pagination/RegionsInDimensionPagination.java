package de.z0rdak.yawp.util.text.messages.pagination;

import de.z0rdak.yawp.api.commands.Commands;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.data.region.DimensionRegionCache;
import de.z0rdak.yawp.util.ChatLinkBuilder;
import de.z0rdak.yawp.util.text.Messages;
import net.minecraft.network.chat.Component;

import java.util.List;

import static de.z0rdak.yawp.util.ChatComponentBuilder.buildHeader;
import static de.z0rdak.yawp.util.ChatComponentBuilder.buildRemoveRegionEntries;

public class RegionsInDimensionPagination extends BasePaginationMessage<IProtectedRegion> {

    private final IProtectedRegion region;
    private final DimensionRegionCache cache;

    public RegionsInDimensionPagination(DimensionRegionCache cache, List<IProtectedRegion> entries, int pageNumber, int pageSize) throws InvalidPageNumberException {
        super(entries, Commands.buildListLocalRegionCommand(cache.dimensionKey()), pageNumber, pageSize);
        this.region = cache.getDimensionalRegion();
        this.cache = cache;
    }

    @Override
    public List<Component> buildEntries() {
        return buildRemoveRegionEntries(region, this.pageContent);
    }

    @Override
    public Component noContentMsg() {
        return Component.translatableWithFallback("cli.msg.dim.info.regions.empty", "No regions defined in %s", ChatLinkBuilder.buildRegionInfoLink(this.cache.getDimensionalRegion()));
    }

    @Override
    public Component header() {
        return buildHeader(Component.translatableWithFallback("cli.msg.info.header.in", "== %s in %s ==", ChatLinkBuilder.buildDimRegionsLink(this.cache), ChatLinkBuilder.buildRegionInfoLink(this.cache.getDimensionalRegion())));
    }

    @Override
    public Component emptyEntry() {
        return Messages.substitutable(" - %s", ChatLinkBuilder.buildDimCreateRegionLink(this.cache.getDimensionalRegion()));
    }
}
