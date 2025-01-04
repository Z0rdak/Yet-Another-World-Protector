package de.z0rdak.yawp.util.text.messages.pagination;


import net.minecraft.network.chat.Component;

import java.util.List;

public class GroupPagination extends BasePaginationMessage<String> {

    protected GroupPagination(List<String> entries, String cmd, int pageNumber, int pageSize) throws InvalidPageNumberException {
        super(entries, cmd, pageNumber, pageSize);
    }

    @Override
    public List<Component> getLines() {
        return List.of();
    }

    @Override
    public List<Component> buildEntries() {
        return List.of();
    }

    @Override
    public Component noContentMsg() {
        return null;
    }

    @Override
    public Component header() {
        return null;
    }

    @Override
    public Component emptyEntry() {
        return null;
    }
}
