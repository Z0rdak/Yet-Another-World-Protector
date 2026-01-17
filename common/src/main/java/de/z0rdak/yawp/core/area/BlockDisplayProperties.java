package de.z0rdak.yawp.core.area;

import com.mojang.serialization.Codec;
import com.mojang.serialization.MapCodec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.Identifier;

import java.util.*;

public final class BlockDisplayProperties {

    public static final boolean DEFAULT_GLOW = true;
    public static final int DEFAULT_LIGHT_LEVEL = 15;

    public static final List<Identifier> DEFAULT_BLOCKS = new ArrayList<>();
    static {
        DEFAULT_BLOCKS.add(Identifier.withDefaultNamespace("white_stained_glass"));
        DEFAULT_BLOCKS.add(Identifier.withDefaultNamespace("orange_stained_glass"));
        DEFAULT_BLOCKS.add(Identifier.withDefaultNamespace("magenta_stained_glass"));
        DEFAULT_BLOCKS.add(Identifier.withDefaultNamespace("light_blue_stained_glass"));
        DEFAULT_BLOCKS.add(Identifier.withDefaultNamespace("yellow_stained_glass"));
        DEFAULT_BLOCKS.add(Identifier.withDefaultNamespace("lime_stained_glass"));
        DEFAULT_BLOCKS.add(Identifier.withDefaultNamespace("pink_stained_glass"));
        DEFAULT_BLOCKS.add(Identifier.withDefaultNamespace("gray_stained_glass"));
        DEFAULT_BLOCKS.add(Identifier.withDefaultNamespace("light_gray_stained_glass"));
        DEFAULT_BLOCKS.add(Identifier.withDefaultNamespace("cyan_stained_glass"));
        DEFAULT_BLOCKS.add(Identifier.withDefaultNamespace("purple_stained_glass"));
        DEFAULT_BLOCKS.add(Identifier.withDefaultNamespace("blue_stained_glass"));
        DEFAULT_BLOCKS.add(Identifier.withDefaultNamespace("brown_stained_glass"));
        DEFAULT_BLOCKS.add(Identifier.withDefaultNamespace("green_stained_glass"));
        DEFAULT_BLOCKS.add(Identifier.withDefaultNamespace("red_stained_glass"));
        DEFAULT_BLOCKS.add(Identifier.withDefaultNamespace("black_stained_glass"));
    }

    public static Identifier randomFromDefault() {
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
                    Identifier.CODEC.fieldOf("block").orElse(randomFromDefault())
                            .forGetter(BlockDisplayProperties::blockRl),
                    Codec.BOOL.fieldOf("hasGlow").orElse(DEFAULT_GLOW)
                            .forGetter(BlockDisplayProperties::hasGlow),
                    Codec.INT.fieldOf("lightLevel").orElse(DEFAULT_LIGHT_LEVEL)
                            .forGetter(BlockDisplayProperties::lightLevel)
            ).apply(instance, BlockDisplayProperties::new)
    );

    private Identifier blockRl;
    private boolean hasGlow;
    private int lightLevel;
    // private boolean persistent;

    public BlockDisplayProperties(Identifier blockRl, boolean hasGlow, int lightLevel) {
        this.blockRl = blockRl;
        this.hasGlow = hasGlow;
        this.lightLevel = lightLevel;
        //this.persistent = true;
    }

    public Identifier blockRl() {
        return this.blockRl;
    }

    public boolean hasGlow() {
        return this.hasGlow;
    }

    public int lightLevel() {
        return this.lightLevel;
    }

    public void setBlockRl(Identifier blockRl) {
        this.blockRl = blockRl;
    }

    public void setHasGlow(boolean hasGlow) {
        this.hasGlow = hasGlow;
    }

    public void setLightLevel(int lightLevel) {
        this.lightLevel = lightLevel;
    }
}
