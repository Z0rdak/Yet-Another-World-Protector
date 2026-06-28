package de.z0rdak.yawp.core.area.visuals;

import net.minecraft.ChatFormatting;
import net.minecraft.nbt.CompoundTag;

import java.util.Locale;


public final class TextDisplayProperties {

    public static int BG_TRANSPARENT = 0x10_00_00_00;

    private String text;
    /**
     * TODO: Enum?
     * {center, left, right} are valid values
     */
    private String alignment;
    /**
     * See: https://minecraft.wiki/w/Display#Data_values
     *  The background color, arranged by ARGB. E.g.: 0x10_00_00_00.
     *  Values of A < 0x14 will be rendered transparent
     */
    private int backgroundColor;
    /**
     * See {@link ChatFormatting} for valid values
     */
    private String color;
    private int lightLevel;

    public TextDisplayProperties(String text,  String alignment, int backgroundColor, String color, int lightLevel) {
        this.text = text;
        this.alignment = alignment;
        this.backgroundColor = backgroundColor;
        // TODO ChatFormatting Changed in 26.2
  //      var colorFormat = ChatFormatting.getByName(color);
        this.color = ChatFormatting.WHITE.name().toLowerCase(Locale.ROOT);
        this.lightLevel = lightLevel;
    }

    public TextDisplayProperties(String text) {
        this.text = text;
        this.alignment = "center";
        this.backgroundColor = BG_TRANSPARENT;
        this.color = ChatFormatting.WHITE.toString();
        this.lightLevel = 15;
    }

    public CompoundTag serializeNBT() {
        CompoundTag tag = new CompoundTag();
        tag.putString("text", this.text);
        tag.putString("alignment", this.alignment);
        tag.putInt("backgroundColor", this.backgroundColor);
        tag.putString("color", this.color);
        tag.putInt("lightLevel", this.lightLevel);
        return tag;
    }

    public static final int DEFAULT_BACKGROUND = 0x1A_00_00_00;

    public void deserializeNBT(CompoundTag nbt) {
        this.text = nbt.getString("text").get();
        this.alignment = nbt.getString("alignment").get();
        this.backgroundColor = nbt.getInt("backgroundColor").get();
      //  var colorFormat = ChatFormatting.getByName(color);
        this.color =  ChatFormatting.WHITE.name().toLowerCase(Locale.ROOT);
        this.lightLevel = nbt.getInt("lightLevel").get();
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }

    public String getAlignment() {
        return alignment;
    }

    public int getBackgroundColor() {
        return backgroundColor;
    }

    public String getColor() {
        return color;
    }

    public int getLightLevel() {
        return lightLevel;
    }
}
