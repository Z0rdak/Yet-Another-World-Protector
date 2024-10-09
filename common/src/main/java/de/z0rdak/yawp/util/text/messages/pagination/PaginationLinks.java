package de.z0rdak.yawp.util.text.messages.pagination;

import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.ComponentUtils;
import net.minecraft.network.chat.MutableComponent;

import static de.z0rdak.yawp.util.ChatComponentBuilder.buildExecuteCmdComponent;
import static de.z0rdak.yawp.util.text.Messages.INACTIVE_LINK_COLOR;
import static de.z0rdak.yawp.util.text.Messages.LINK_COLOR;
import static net.minecraft.network.chat.ClickEvent.Action.RUN_COMMAND;

// Note: Maybe extend with Builder-Pattern to customize lang keys, link color, page indicator, etc.
public record PaginationLinks(String command, int pageNumber, int pageSize, int numberOfPages) {


    public static int FIRST_PAGE_IDX = 0;

    public String buildPageCommand(String cmd, int page) {
        return String.join(" ", cmd, Integer.toString(page));
    }

    public MutableComponent buildLastLinkArrow(String cmd, int pageNo, int numberOfPages, boolean hasMultiplePages) {
        MutableComponent last = Component.translatableWithFallback("cli.msg.info.pagination.last.text", ">>");
        MutableComponent lastHover = Component.translatableWithFallback("cli.msg.info.pagination.last.hover", "Last page");
        return hasMultiplePages && pageNo < numberOfPages - 1
                ? buildExecuteCmdComponent(last, lastHover, buildPageCommand(cmd, numberOfPages - 1), RUN_COMMAND, LINK_COLOR)
                : ComponentUtils.wrapInSquareBrackets(last).withStyle(INACTIVE_LINK_COLOR);
    }

    public MutableComponent buildNextLinkArrow(String cmd, int pageNo, int numberOfPages, boolean hasMultiplePages) {
        MutableComponent next = Component.translatableWithFallback("cli.msg.info.pagination.next.text", ">");
        MutableComponent nextHover = Component.translatableWithFallback("cli.msg.info.pagination.next.hover", "Next page");
        return hasMultiplePages && pageNo < numberOfPages - 1
                ? buildExecuteCmdComponent(next, nextHover, buildPageCommand(cmd, Math.min(pageNo + 1, numberOfPages - 1)), RUN_COMMAND, LINK_COLOR)
                : ComponentUtils.wrapInSquareBrackets(next).withStyle(INACTIVE_LINK_COLOR);
    }

    public MutableComponent buildPrevLinkArrow(String cmd, int pageNo, boolean hasMultiplePages) {
        MutableComponent previous = Component.translatableWithFallback("cli.msg.info.pagination.previous.text", "<");
        MutableComponent previousHover = Component.translatableWithFallback("cli.msg.info.pagination.previous.hover", "Previous page");
        return hasMultiplePages && pageNo > FIRST_PAGE_IDX
                ? buildExecuteCmdComponent(previous, previousHover, buildPageCommand(cmd, Math.max(pageNo - 1, FIRST_PAGE_IDX)), RUN_COMMAND, LINK_COLOR)
                : ComponentUtils.wrapInSquareBrackets(previous).withStyle(INACTIVE_LINK_COLOR);
    }

    public MutableComponent buildFirstLinkArrow(String cmd, int pageNo, boolean hasMultiplePages) {
        MutableComponent first = Component.translatableWithFallback("cli.msg.info.pagination.first.text", "<<");
        MutableComponent firstHover = Component.translatableWithFallback("cli.msg.info.pagination.first.hover", "First page");
        return hasMultiplePages && pageNo != FIRST_PAGE_IDX
                ? buildExecuteCmdComponent(first, firstHover, buildPageCommand(cmd, FIRST_PAGE_IDX), RUN_COMMAND, LINK_COLOR)
                : ComponentUtils.wrapInSquareBrackets(first).withStyle(INACTIVE_LINK_COLOR);
    }

}
