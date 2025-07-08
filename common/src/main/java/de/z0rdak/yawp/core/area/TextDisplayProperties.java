package de.z0rdak.yawp.core.area;

import de.z0rdak.yawp.core.INbtSerializable;
import net.minecraft.ChatFormatting;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.resources.ResourceLocation;

public final class TextDisplayProperties implements INbtSerializable<CompoundTag> {

    private String text;

    public TextDisplayProperties(String text) {
        this.text = text;
    }

    public TextDisplayProperties(CompoundTag nbt) {
        this.deserializeNBT(nbt);
    }

    @Override
    public CompoundTag serializeNBT() {
        CompoundTag tag = new CompoundTag();
        tag.putString("text", this.text);
        return tag;
    }

    public static final int DEFAULT_BACKGROUND = 0x1A_00_00_00;

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        this.text = nbt.getString("text");
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }
}
