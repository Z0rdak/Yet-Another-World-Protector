package de.z0rdak.yawp.core.area;

import de.z0rdak.yawp.util.constants.RegionNBT;
import net.minecraft.nbt.NbtCompound;

public abstract class AbstractArea implements IMarkableArea {

    private AreaType areaType;

    protected AbstractArea(AreaType areaType) {
        this.areaType = areaType;
    }

    protected AbstractArea(NbtCompound nbt) {
        this.deserializeNBT(nbt);
    }

    public AreaType getAreaType() {
        return this.areaType;
    }

    @Override
    public NbtCompound serializeNBT() {
        NbtCompound nbt = new NbtCompound();
        nbt.putString(RegionNBT.AREA_TYPE, this.areaType.areaType);
        return nbt;
    }

    @Override
    public void deserializeNBT(NbtCompound nbt) {
        this.areaType = AreaType.of(nbt.getString(RegionNBT.AREA_TYPE));
    }
}
