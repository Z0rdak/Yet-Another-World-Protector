package de.z0rdak.yawp.util.text.messages.pagination;

import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;

import java.util.ArrayList;
import java.util.List;

public abstract class BasePaginationMessage<T> implements PaginationMessage<T> {

    public static int FIRST_PAGE_IDX = 0;
    protected final PaginationControl control;
    protected List<T> pageContent;
    protected List<Component> paginationLines;

    protected BasePaginationMessage(List<T> entries, String cmd, int pageNumber, int pageSize) throws InvalidPageNumberException {
        this.paginationLines = new ArrayList<>();
        int numberOfPages = entries.size() / pageSize;
        if (numberOfPages == 0 || entries.size() % pageSize != 0) {
            numberOfPages += 1;
        }
        this.control = new PaginationControl(cmd, pageNumber, pageSize, numberOfPages);
        this.pageContent = PaginationMessage.getPageContentFor(entries, pageNumber, pageSize);
        if (this.control.pageNumber() < FIRST_PAGE_IDX || this.control.pageNumber() >= this.control.numberOfPages()) {
            MutableComponent error = Component.translatableWithFallback("cli.msg.info.pagination.error.index", "Invalid page index supplied: %s (Try [0..%s])", this.control.pageNumber(), this.control.numberOfPages() - 1);
            throw new InvalidPageNumberException(error, this.control.pageNumber(), this.control.pageSize(), this.control.numberOfPages());
        }
    }

    protected void padEmptyEntries() {
        if (this.control.numberOfPages() > 1) {
            int numberOfEmptyEntries = this.control.pageSize() - this.pageContent.size();
            for (int i = 0; i < numberOfEmptyEntries; i++) {
                paginationLines.add(this.emptyEntry());
            }
        }
    }

    @Override
    public List<T> getContent() {
        return this.pageContent;
    }

    @Override
    public List<Component> getLines() {
        if (this.pageContent.isEmpty()) {
            this.paginationLines.add(noContentMsg());
            return this.paginationLines;
        }
        this.paginationLines.add(header());
        this.paginationLines.addAll(buildEntries());
        if (this.control.numberOfPages() > 1) {
            this.padEmptyEntries();
            paginationLines.add(this.control.build());
        }
        return this.paginationLines;
    }

    public abstract List<Component> buildEntries();

    public abstract Component noContentMsg();

    public abstract Component header();

    public abstract Component emptyEntry();
}
