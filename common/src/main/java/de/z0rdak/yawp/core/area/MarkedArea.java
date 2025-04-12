package de.z0rdak.yawp.core.area;

import de.z0rdak.yawp.constants.serialization.RegionNbtKeys;
import net.minecraft.nbt.CompoundTag;

import java.util.Optional;

public abstract class MarkedArea implements IMarkableArea {

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
