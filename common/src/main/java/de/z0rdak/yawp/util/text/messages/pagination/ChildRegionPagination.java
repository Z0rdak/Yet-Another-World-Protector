package de.z0rdak.yawp.util.text.messages.pagination;

import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.util.ChatLinkBuilder;
import net.minecraft.network.chat.Component;

import java.util.List;

import static de.z0rdak.yawp.api.commands.Commands.buildListChildRegionCommand;
import static de.z0rdak.yawp.util.ChatComponentBuilder.buildHeader;
import static de.z0rdak.yawp.util.ChatComponentBuilder.buildRemoveRegionEntries;
import static de.z0rdak.yawp.util.ChatLinkBuilder.buildRegionInfoLink;

public class ChildRegionPagination extends BasePaginationMessage<IProtectedRegion> {

    private final IProtectedRegion region;

    public ChildRegionPagination(IProtectedRegion region, List<IProtectedRegion> entries, int pageNumber, int pageSize) throws InvalidPageNumberException {
        super(entries, buildListChildRegionCommand(region), pageNumber, pageSize);
        this.region = region;
    }

    @Override
    public List<Component> buildEntries() {
        return buildRemoveRegionEntries(region, this.pageContent);
    }

    @Override
    public Component noContentMsg() {
        return Component.translatableWithFallback("cli.msg.info.region.children.empty", "No children defined in region %s", buildRegionInfoLink(region));
    }

    @Override
    public Component header() {
        return buildHeader(Component.translatableWithFallback("cli.msg.info.header.in", "== %s in %s ==", ChatLinkBuilder.buildRegionListChildrenLink(region), buildRegionInfoLink(region)));
    }

    @Override
    public Component emptyEntry() {
        return Component.translatable(" - %s", ChatLinkBuilder.buildRegionAddChildrenLink(region));
    }
}
