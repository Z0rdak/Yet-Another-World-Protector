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
        DEFAULT_BLOCKS.add(ResourceLocation.withDefaultNamespace("white_stained_glass"));
        DEFAULT_BLOCKS.add(ResourceLocation.withDefaultNamespace("orange_stained_glass"));
        DEFAULT_BLOCKS.add(ResourceLocation.withDefaultNamespace("magenta_sta0ined_glass"));
        DEFAULT_BLOCKS.add(ResourceLocation.withDefaultNamespace("light_blue_stained_glass"));
        DEFAULT_BLOCKS.add(ResourceLocation.withDefaultNamespace("yellow_stained_glass"));
        DEFAULT_BLOCKS.add(ResourceLocation.withDefaultNamespace("lime_stained_glass"));
        DEFAULT_BLOCKS.add(ResourceLocation.withDefaultNamespace("pink_stained_glass"));
        DEFAULT_BLOCKS.add(ResourceLocation.withDefaultNamespace("gray_stained_glass"));
        DEFAULT_BLOCKS.add(ResourceLocation.withDefaultNamespace("light_gray_stained_glass"));
        DEFAULT_BLOCKS.add(ResourceLocation.withDefaultNamespace("cyan_stained_glass"));
        DEFAULT_BLOCKS.add(ResourceLocation.withDefaultNamespace("purple_stained_glass"));
        DEFAULT_BLOCKS.add(ResourceLocation.withDefaultNamespace("blue_stained_glass"));
        DEFAULT_BLOCKS.add(ResourceLocation.withDefaultNamespace("brown_stained_glass"));
        DEFAULT_BLOCKS.add(ResourceLocation.withDefaultNamespace("green_stained_glass"));
        DEFAULT_BLOCKS.add(ResourceLocation.withDefaultNamespace("red_stained_glass"));
        DEFAULT_BLOCKS.add(ResourceLocation.withDefaultNamespace("black_stained_glass"));
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
