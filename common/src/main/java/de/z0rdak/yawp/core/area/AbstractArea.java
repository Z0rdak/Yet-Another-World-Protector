package de.z0rdak.yawp.core.area;

import de.z0rdak.yawp.constants.RegionNBT;
import net.minecraft.nbt.CompoundTag;

public abstract class AbstractArea implements IMarkableArea {

    private AreaType areaType;

    protected AbstractArea(AreaType areaType) {
        this.areaType = areaType;
    }

    protected AbstractArea(CompoundTag nbt) {
        this.deserializeNBT(nbt);
    }

    public AreaType getAreaType() {
        return this.areaType;
    }

    @Override
    public CompoundTag serializeNBT() {
        CompoundTag nbt = new CompoundTag();
        nbt.putString(RegionNBT.AREA_TYPE, this.areaType.areaType);
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        this.areaType = AreaType.of(nbt.getString(RegionNBT.AREA_TYPE));
    }
}
