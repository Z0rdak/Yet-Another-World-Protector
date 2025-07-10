package de.z0rdak.yawp.core.area;

import de.z0rdak.yawp.core.INbtSerializable;
import net.minecraft.ChatFormatting;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.resources.ResourceLocation;

public final class TextDisplayProperties implements INbtSerializable<CompoundTag> {

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
        var colorFormat = ChatFormatting.getByName(color);
        this.color = colorFormat == null ? ChatFormatting.WHITE.getName() : colorFormat.getName();
        this.lightLevel = lightLevel;
    }

    public TextDisplayProperties(String text) {
        this.text = text;
        this.alignment = "center";
        this.backgroundColor = BG_TRANSPARENT;
        this.color = ChatFormatting.WHITE.toString();
        this.lightLevel = 15;
    }

    public TextDisplayProperties(CompoundTag nbt) {
        this.deserializeNBT(nbt);
    }

    @Override
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

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        this.text = nbt.getString("text");
        this.alignment = nbt.getString("alignment");
        this.backgroundColor = nbt.getInt("backgroundColor");
        var colorFormat = ChatFormatting.getByName(color);
        this.color = colorFormat == null ? ChatFormatting.WHITE.getName() : colorFormat.getName();
        this.lightLevel = nbt.getInt("lightLevel");
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
