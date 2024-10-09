package de.z0rdak.yawp.util.text.messages.pagination;

import net.minecraft.network.chat.MutableComponent;

public class InvalidPageNumberException extends Exception {


    private final MutableComponent error;
    private final int pageSize;
    private final int pageNumber;
    private final int numberOfPages;

    public InvalidPageNumberException(MutableComponent error, int pageSize, int pageNumber, int numberOfPages) {
        this.error = error;
        this.pageSize = pageSize;
        this.pageNumber = pageNumber;
        this.numberOfPages = numberOfPages;
    }

    public MutableComponent getError() {
        return error;
    }

    public int getPageSize() {
        return pageSize;
    }

    public int getPageNumber() {
        return pageNumber;
    }

    public int getNumberOfPages() {
        return numberOfPages;
    }
}
