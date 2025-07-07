package de.z0rdak.yawp.core.area;

import de.z0rdak.yawp.core.INbtSerializable;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;

public final class BlockDisplayProperties implements INbtSerializable<CompoundTag> {

    private ResourceLocation blockRl;
    private boolean hasGlow;
    private int lightLevel;
    // private boolean persistent;

    public BlockDisplayProperties(ResourceLocation blockRl, boolean hasGlow, int lightLevel) {
        this.blockRl = blockRl;
        this.hasGlow = hasGlow;
        this.lightLevel = lightLevel;
        //this.persistent = true;
    }

    public BlockDisplayProperties(CompoundTag nbt) {
        this.deserializeNBT(nbt);
    }

    @Override
    public CompoundTag serializeNBT() {
        CompoundTag tag = new CompoundTag();
        tag.putString("block", blockRl.toString());
        tag.putBoolean("hasGlow", hasGlow);
        tag.putInt("lightLevel", lightLevel);
        return tag;
    }

    public static final ResourceLocation DEFAULT_BLOCK = ResourceLocation.parse("minecraft:cyan_stained_glass");
    public static final boolean DEFAULT_GLOW = true;
    public static final int DEFAULT_LIGHT_LEVEL = 15;

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        String string = nbt.getString("block");
        try {
            this.blockRl = ResourceLocation.parse(string);
        } catch (Exception _e) {
            // TODO: Config for default
            this.blockRl = DEFAULT_BLOCK;
        }
        this.hasGlow = nbt.getBoolean("hasGlow");
        this.lightLevel = nbt.getInt("lightLevel");
    }

    public ResourceLocation blockRl() {
        return blockRl;
    }

    public boolean hasGlow() {
        return hasGlow;
    }

    public int lightLevel() {
        return lightLevel;
    }

    public void setBlockRl(ResourceLocation blockRl) {
        this.blockRl = blockRl;
    }

    public void setHasGlow(boolean hasGlow) {
        this.hasGlow = hasGlow;
    }

    public void setLightLevel(int lightLevel) {
        this.lightLevel = lightLevel;
    }
}
