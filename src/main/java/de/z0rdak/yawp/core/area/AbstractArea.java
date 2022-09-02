package de.z0rdak.yawp.core.area;

import de.z0rdak.yawp.util.constants.NBTConstants;
import de.z0rdak.yawp.util.constants.RegionNBT;
import net.minecraft.nbt.CompoundNBT;

public abstract class AbstractArea implements IMarkableArea {

    private AreaType areaType;

    protected AbstractArea(AreaType areaType) {
        this.areaType = areaType;
    }

    protected AbstractArea(CompoundNBT nbt) {
        this.deserializeNBT(nbt);
    }

    public AreaType getAreaType() {
        return this.areaType;
    }

    @Override
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = new CompoundNBT();
        nbt.putString(RegionNBT.AREA_TYPE, this.areaType.areaType);
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        this.areaType = AreaType.of(nbt.getString(RegionNBT.AREA_TYPE));
    }
}
