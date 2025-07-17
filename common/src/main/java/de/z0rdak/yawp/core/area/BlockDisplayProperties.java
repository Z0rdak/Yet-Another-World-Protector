package de.z0rdak.yawp.core.area;

import de.z0rdak.yawp.core.INbtSerializable;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;

import java.util.*;

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

    public static final List<ResourceLocation> DEFAULT_BLOCKS = new ArrayList<>();
    static {
        DEFAULT_BLOCKS.add(ResourceLocation.tryBuild(ResourceLocation.DEFAULT_NAMESPACE, "white_stained_glass"));
        DEFAULT_BLOCKS.add(ResourceLocation.tryBuild(ResourceLocation.DEFAULT_NAMESPACE, "orange_stained_glass"));
        DEFAULT_BLOCKS.add(ResourceLocation.tryBuild(ResourceLocation.DEFAULT_NAMESPACE, "magenta_stained_glass"));
        DEFAULT_BLOCKS.add(ResourceLocation.tryBuild(ResourceLocation.DEFAULT_NAMESPACE, "light_blue_stained_glass"));
        DEFAULT_BLOCKS.add(ResourceLocation.tryBuild(ResourceLocation.DEFAULT_NAMESPACE, "yellow_stained_glass"));
        DEFAULT_BLOCKS.add(ResourceLocation.tryBuild(ResourceLocation.DEFAULT_NAMESPACE, "lime_stained_glass"));
        DEFAULT_BLOCKS.add(ResourceLocation.tryBuild(ResourceLocation.DEFAULT_NAMESPACE, "pink_stained_glass"));
        DEFAULT_BLOCKS.add(ResourceLocation.tryBuild(ResourceLocation.DEFAULT_NAMESPACE, "gray_stained_glass"));
        DEFAULT_BLOCKS.add(ResourceLocation.tryBuild(ResourceLocation.DEFAULT_NAMESPACE, "light_gray_stained_glass"));
        DEFAULT_BLOCKS.add(ResourceLocation.tryBuild(ResourceLocation.DEFAULT_NAMESPACE, "cyan_stained_glass"));
        DEFAULT_BLOCKS.add(ResourceLocation.tryBuild(ResourceLocation.DEFAULT_NAMESPACE, "purple_stained_glass"));
        DEFAULT_BLOCKS.add(ResourceLocation.tryBuild(ResourceLocation.DEFAULT_NAMESPACE, "blue_stained_glass"));
        DEFAULT_BLOCKS.add(ResourceLocation.tryBuild(ResourceLocation.DEFAULT_NAMESPACE, "brown_stained_glass"));
        DEFAULT_BLOCKS.add(ResourceLocation.tryBuild(ResourceLocation.DEFAULT_NAMESPACE, "green_stained_glass"));
        DEFAULT_BLOCKS.add(ResourceLocation.tryBuild(ResourceLocation.DEFAULT_NAMESPACE, "red_stained_glass"));
        DEFAULT_BLOCKS.add(ResourceLocation.tryBuild(ResourceLocation.DEFAULT_NAMESPACE, "black_stained_glass"));
    }

    public static ResourceLocation randomFromDefault() {
        int randomNum = new Random().nextInt(0, 16);
        return BlockDisplayProperties.DEFAULT_BLOCKS.get(randomNum);
    }

    public static final boolean DEFAULT_GLOW = true;
    public static final int DEFAULT_LIGHT_LEVEL = 15;

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        String string = nbt.getString("block");
        this.blockRl = ResourceLocation.tryParse(string);
        if  (this.blockRl == null) {
            Random rand = new Random();
            int randomNum = rand.nextInt(0, DEFAULT_BLOCKS.size());
            this.blockRl = DEFAULT_BLOCKS.get(randomNum);
        }
        this.hasGlow = nbt.getBoolean("hasGlow");
        this.lightLevel = nbt.getInt("lightLevel");
    }

    public ResourceLocation blockRl() {
        return this.blockRl;
    }

    public boolean hasGlow() {
        return this.hasGlow;
    }

    public int lightLevel() {
        return this.lightLevel;
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
