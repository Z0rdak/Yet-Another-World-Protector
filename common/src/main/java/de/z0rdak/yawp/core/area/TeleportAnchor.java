package de.z0rdak.yawp.core.area;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import de.z0rdak.yawp.constants.serialization.RegionNbtKeys;
import de.z0rdak.yawp.core.INbtSerializable;
import de.z0rdak.yawp.util.NbtCompatHelper;
import net.minecraft.core.BlockPos;

public class TeleportAnchor {

    public static Codec<TeleportAnchor> CODEC = RecordCodecBuilder.create(instance -> instance.group(
                    BlockPos.CODEC.fieldOf("pos")
                            .forGetter(TeleportAnchor::getPos),
                    Codec.STRING.fieldOf("name")
                            .forGetter(TeleportAnchor::getName)
            ).apply(instance, TeleportAnchor::new)
    );

    public BlockPos getPos() {
        return pos;
    }

    public void setPos(BlockPos pos) {
        this.pos = pos;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    private BlockPos pos;
    private String name;
    // private Direction facing;
    // DisplayProperties display;

    public TeleportAnchor(BlockPos pos, String name) {
        this.pos = pos;
        this.name = name;
    }
}