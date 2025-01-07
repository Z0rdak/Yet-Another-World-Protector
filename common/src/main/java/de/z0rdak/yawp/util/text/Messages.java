package de.z0rdak.yawp.util.text;

import net.minecraft.ChatFormatting;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.network.chat.contents.TranslatableContents;

import static net.minecraft.ChatFormatting.*;

public final class Messages {

    public final static ChatFormatting LINK_COLOR = AQUA;
    public final static ChatFormatting TP_COLOR = GREEN;
    public final static ChatFormatting INACTIVE_LINK_COLOR = GRAY;
    public final static ChatFormatting ADD_CMD_COLOR = DARK_GREEN;
    public final static ChatFormatting REMOVE_CMD_COLOR = DARK_RED;
    private Messages() {
    }

    public static MutableComponent substitutable(String pattern, Object... args) {
        /* return MutableComponent.create(new SubstituteTextContent(pattern, args)); */
        return MutableComponent.create(new TranslatableContents(pattern, null, args));
    }
}
