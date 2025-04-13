package de.z0rdak.yawp.core.area;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import de.z0rdak.yawp.constants.serialization.RegionNbtKeys;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;

import java.util.Optional;

public abstract class MarkedArea implements IMarkableArea {

    public static Codec<IMarkableArea> CODEC = RecordCodecBuilder.create(instance -> instance.group(
                            Codec.STRING.fieldOf("areaType")
                                    .forGetter(r->r.getAreaType().areaType),
                            BlockPos.CODEC.optionalFieldOf("p1")
                                    .forGetter(r -> Optional.of(((CuboidArea) r).getAreaP1())),
                            BlockPos.CODEC.optionalFieldOf("p2")
                                    .forGetter(r -> Optional.of(((CuboidArea) r).getAreaP2())),
                            BlockPos.CODEC.optionalFieldOf("center")
                                    .forGetter(r -> Optional.of(((SphereArea) r).getCenterPos())),
                            Codec.INT.optionalFieldOf("radius")
                                    .forGetter(r -> Optional.of(((SphereArea) r).getRadius()))
                    )
                    .apply(instance, (areaType, p1, p2, center, radius) -> switch (AreaType.of(areaType)) {
                        case CUBOID -> new CuboidArea(p1.orElseThrow(), p2.orElseThrow());
                        case SPHERE -> new SphereArea(center.orElseThrow(), radius.orElseThrow());
                        default -> throw new IllegalStateException("Unexpected value: " + areaType);
                    })
    );

    private AreaType areaType;

    protected MarkedArea(AreaType areaType) {
        this.areaType = areaType;
    }

    protected MarkedArea(CompoundTag nbt) {
        this.deserializeNBT(nbt);
    }

    public AreaType getAreaType() {
        return this.areaType;
    }

    @Override
    public CompoundTag serializeNBT() {
        CompoundTag nbt = new CompoundTag();
        nbt.putString(RegionNbtKeys.AREA_TYPE, this.areaType.areaType);
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        Optional<String> maybeStr = nbt.getString(RegionNbtKeys.AREA_TYPE);
        maybeStr.ifPresent(areaType -> this.areaType = AreaType.of(areaType));
    }
}
