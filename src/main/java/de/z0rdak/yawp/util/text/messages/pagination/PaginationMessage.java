package de.z0rdak.yawp.util.text.messages.pagination;

import de.z0rdak.yawp.util.text.messages.multiline.MultiLineMessage;

import java.util.List;

public interface PaginationMessage<T> extends MultiLineMessage<List<T>> {

    static <T> List<T> getPageContentFor(List<T> entries, int pageNumber, int pageSize) {
        int from = pageNumber * pageSize;
        int to = Math.min(pageSize + (pageSize * pageNumber), entries.size());
        return entries.subList(from, to);
    }
}
