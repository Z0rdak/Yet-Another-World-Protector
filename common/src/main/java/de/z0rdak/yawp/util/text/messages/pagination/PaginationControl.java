package de.z0rdak.yawp.util.text.messages.pagination;

import de.z0rdak.yawp.util.text.Messages;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;


public class PaginationControl {

    private final String command;
    private final int numberOfPages;
    private final PaginationLinks links;
    private final int pageNumber;
    private final int pageSize;

    public PaginationControl(String cmd, int pageNumber, int pageSize, int numberOfPages) {
        this.command = cmd;
        this.pageNumber = pageNumber;
        this.pageSize = pageSize;
        this.numberOfPages = numberOfPages;
        this.links = new PaginationLinks(cmd, pageNumber, pageSize, numberOfPages);
    }

    public int pageNumber() {
        return pageNumber;
    }

    public int pageSize() {
        return pageSize;
    }

    public int numberOfPages() {
        return numberOfPages;
    }

    public MutableComponent build() {
        boolean hasMultiplePages = this.numberOfPages > 1;
        MutableComponent first = this.links.buildFirstLinkArrow(this.command, this.pageNumber, hasMultiplePages);
        MutableComponent prev = this.links.buildPrevLinkArrow(this.command, this.pageNumber, hasMultiplePages);
        MutableComponent next = this.links.buildNextLinkArrow(this.command, this.pageNumber, numberOfPages, hasMultiplePages);
        MutableComponent last = this.links.buildLastLinkArrow(this.command, this.pageNumber, numberOfPages, hasMultiplePages);
        return this.buildControl(first, prev, next, last);
    }
    
    public MutableComponent buildControl(MutableComponent front, MutableComponent back, MutableComponent forward, MutableComponent last) {
        // [<<]  [<]  x/n  [>]  [>>]
        MutableComponent pageIndicator = Component.literal((this.pageNumber + 1) + "/" + (this.numberOfPages));
        return Messages.substitutable(" %s  %s  %s  %s  %s", front, back, pageIndicator, forward, last);
    }

}
