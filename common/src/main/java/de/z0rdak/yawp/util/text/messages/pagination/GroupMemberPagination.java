package de.z0rdak.yawp.util.text.messages.pagination;

import de.z0rdak.yawp.core.group.GroupType;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.util.ChatComponentBuilder;
import de.z0rdak.yawp.util.text.Messages;
import net.minecraft.network.chat.Component;

import java.util.List;

import static de.z0rdak.yawp.api.commands.Commands.buildListGroupMemberCommand;
import static de.z0rdak.yawp.util.ChatComponentBuilder.buildRemoveGroupMemberEntries;
import static de.z0rdak.yawp.util.ChatLinkBuilder.buildAddToGroupLink;
import static de.z0rdak.yawp.util.ChatLinkBuilder.buildRegionInfoLink;

public class GroupMemberPagination extends BasePaginationMessage<String> {

    private final IProtectedRegion region;
    private final String groupName;
    private final GroupType groupType;

    public GroupMemberPagination(IProtectedRegion region, String groupName, GroupType groupType, List<String> entries, int pageNumber, int pageSize) throws InvalidPageNumberException {
        super(entries, buildListGroupMemberCommand(region, groupName, groupType), pageNumber, pageSize);
        this.region = region;
        this.groupName = groupName;
        this.groupType = groupType;
    }

    @Override
    public List<Component> buildEntries() {
        return buildRemoveGroupMemberEntries(region, pageContent, groupType, groupName);
    }

    @Override
    public Component noContentMsg() {
        return Component.translatableWithFallback("cli.msg.info.region.group." + groupType.name + ".empty",
                "No " + groupType.name + "s defined as '%s' in %s", groupName, buildRegionInfoLink(region));
    }

    @Override
    public Component header() {
        return ChatComponentBuilder.buildGroupTypeHeader(region, groupName, groupType);
    }

    @Override
    public Component emptyEntry() {
        return Messages.substitutable(" - %s", buildAddToGroupLink(region, groupName, groupType));
    }
}
