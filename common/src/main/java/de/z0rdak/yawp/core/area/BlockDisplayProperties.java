package de.z0rdak.yawp.core.area;

import com.mojang.serialization.Codec;
import com.mojang.serialization.MapCodec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import net.minecraft.resources.ResourceLocation;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public final class BlockDisplayProperties {

    public static final boolean DEFAULT_GLOW = true;
    public static final int DEFAULT_LIGHT_LEVEL = 15;

    public static final List<ResourceLocation> DEFAULT_BLOCKS = new ArrayList<>();
    static {
        DEFAULT_BLOCKS.add(new ResourceLocation(ResourceLocation.DEFAULT_NAMESPACE, "white_stained_glass"));
        DEFAULT_BLOCKS.add(new ResourceLocation(ResourceLocation.DEFAULT_NAMESPACE, "orange_stained_glass"));
        DEFAULT_BLOCKS.add(new ResourceLocation(ResourceLocation.DEFAULT_NAMESPACE, "magenta_stained_glass"));
        DEFAULT_BLOCKS.add(new ResourceLocation(ResourceLocation.DEFAULT_NAMESPACE, "light_blue_stained_glass"));
        DEFAULT_BLOCKS.add(new ResourceLocation(ResourceLocation.DEFAULT_NAMESPACE, "yellow_stained_glass"));
        DEFAULT_BLOCKS.add(new ResourceLocation(ResourceLocation.DEFAULT_NAMESPACE, "lime_stained_glass"));
        DEFAULT_BLOCKS.add(new ResourceLocation(ResourceLocation.DEFAULT_NAMESPACE, "pink_stained_glass"));
        DEFAULT_BLOCKS.add(new ResourceLocation(ResourceLocation.DEFAULT_NAMESPACE, "gray_stained_glass"));
        DEFAULT_BLOCKS.add(new ResourceLocation(ResourceLocation.DEFAULT_NAMESPACE, "light_gray_stained_glass"));
        DEFAULT_BLOCKS.add(new ResourceLocation(ResourceLocation.DEFAULT_NAMESPACE, "cyan_stained_glass"));
        DEFAULT_BLOCKS.add(new ResourceLocation(ResourceLocation.DEFAULT_NAMESPACE, "purple_stained_glass"));
        DEFAULT_BLOCKS.add(new ResourceLocation(ResourceLocation.DEFAULT_NAMESPACE, "blue_stained_glass"));
        DEFAULT_BLOCKS.add(new ResourceLocation(ResourceLocation.DEFAULT_NAMESPACE, "brown_stained_glass"));
        DEFAULT_BLOCKS.add(new ResourceLocation(ResourceLocation.DEFAULT_NAMESPACE, "green_stained_glass"));
        DEFAULT_BLOCKS.add(new ResourceLocation(ResourceLocation.DEFAULT_NAMESPACE, "red_stained_glass"));
        DEFAULT_BLOCKS.add(new ResourceLocation(ResourceLocation.DEFAULT_NAMESPACE, "black_stained_glass"));
    }

    public static ResourceLocation randomFromDefault() {
        int randomNum = new Random().nextInt(0, 16);
        return BlockDisplayProperties.DEFAULT_BLOCKS.get(randomNum);
    }

    public static BlockDisplayProperties createRndDefault() {
        return new BlockDisplayProperties(
                randomFromDefault(),
                DEFAULT_GLOW,
                DEFAULT_LIGHT_LEVEL
        );
    }

    public static MapCodec<BlockDisplayProperties> CODEC = RecordCodecBuilder.mapCodec(instance -> instance.group(
                    ResourceLocation.CODEC.fieldOf("block")
                            .orElse(randomFromDefault())
                            .forGetter(BlockDisplayProperties::blockRl),
                    Codec.BOOL.fieldOf("hasGlow")
                            .orElse(DEFAULT_GLOW)
                            .forGetter(BlockDisplayProperties::hasGlow),
                    Codec.INT.fieldOf("lightLevel")
                            .orElse(DEFAULT_LIGHT_LEVEL)
                            .forGetter(BlockDisplayProperties::lightLevel)
            ).apply(instance, BlockDisplayProperties::new)
    );

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
