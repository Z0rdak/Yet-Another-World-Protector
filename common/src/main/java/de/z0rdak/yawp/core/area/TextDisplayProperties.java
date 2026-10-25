package de.z0rdak.yawp.core.area;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import de.z0rdak.yawp.core.area.display.TextAlignment;
import net.minecraft.ChatFormatting;

public final class TextDisplayProperties {

    public static final int BG_TRANSPARENT = 0x10_00_00_00;
    public static final int DEFAULT_BACKGROUND = 0x1A_00_00_00;
    public static final TextAlignment DEFAULT_ALIGNMENT = TextAlignment.CENTER;
    public static final ChatFormatting DEFAULT_COLOR = ChatFormatting.WHITE;
    public static final int DEFAULT_LIGHT_LEVEL = 15;

    public static final Codec<TextDisplayProperties> CODEC =
            RecordCodecBuilder.create(instance -> instance.group(
                    Codec.STRING.fieldOf("text")
                            .forGetter(TextDisplayProperties::getText),
                    TextAlignment.CODEC.fieldOf("alignment")
                            .forGetter(TextDisplayProperties::getAlignment),
                    Codec.INT.fieldOf("backgroundColor")
                            .forGetter(TextDisplayProperties::getBackgroundColor),
                    TextAlignment.FORMATTING_CODEC.fieldOf("color")
                            .forGetter(TextDisplayProperties::getColor),
                    Codec.intRange(0, 15).fieldOf("lightLevel")
                            .forGetter(TextDisplayProperties::getLightLevel)
            ).apply(instance, TextDisplayProperties::new));

    private String text;
    private final TextAlignment alignment;
    /**
     * Background color in ARGB format.
     * Values with alpha below 0x14 are rendered transparent.
     */
    private final int backgroundColor;
    /**
     * Chat formatting used for the displayed text.
     */
    private final ChatFormatting color;
    /**
     * Display light level, between 0 and 15.
     */
    private final int lightLevel;

    public TextDisplayProperties(String text, TextAlignment alignment, int backgroundColor, ChatFormatting formatting, int lightLevel) {
        this.text = text;
        this.alignment = alignment;
        this.backgroundColor = backgroundColor;
        this.color = formatting;
        this.lightLevel = lightLevel;
    }

    public TextDisplayProperties(String text) {
        this(text, DEFAULT_ALIGNMENT, BG_TRANSPARENT, DEFAULT_COLOR, DEFAULT_LIGHT_LEVEL);
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }

    public TextAlignment getAlignment() {
        return alignment;
    }

    public int getBackgroundColor() {
        return backgroundColor;
    }

    public ChatFormatting getColor() {
        return color;
    }

    public int getLightLevel() {
        return lightLevel;
    }
}