package de.z0rdak.yawp.core.area.display;

import com.mojang.serialization.Codec;
import com.mojang.serialization.DataResult;
import net.minecraft.ChatFormatting;

import java.util.Locale;

public enum TextAlignment {
    LEFT, RIGHT, CENTER;

    public static final Codec<TextAlignment> CODEC = Codec.STRING.comapFlatMap(
            name -> {
                try {
                    return DataResult.success(TextAlignment.valueOf(name.toUpperCase(Locale.ROOT)));
                } catch (IllegalArgumentException e) {
                    return DataResult.error(() -> "Unknown text alignment: " + name);
                }
            }, alignment -> alignment.name().toLowerCase(Locale.ROOT));

    public static final Codec<ChatFormatting> FORMATTING_CODEC = Codec.STRING.comapFlatMap(
            name -> {
                try {
                    ChatFormatting formatting = ChatFormatting.valueOf(name.toUpperCase(Locale.ROOT));
                    return DataResult.success(formatting);
                } catch (IllegalArgumentException e) {
                    return DataResult.error(() -> "Unknown chat formatting: " + name);
                }
            }, ChatFormatting::name);
}
