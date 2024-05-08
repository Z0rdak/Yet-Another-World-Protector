package de.z0rdak.yawp.core.area;

import de.z0rdak.yawp.util.constants.RegionNBT;
import net.minecraft.core.HolderLookup;
import net.minecraft.nbt.CompoundTag;

public abstract class AbstractArea implements IMarkableArea {

    private AreaType areaType;

    protected AbstractArea(AreaType areaType) {
        this.areaType = areaType;
    }

    protected AbstractArea(CompoundTag nbt) {
        this.deserializeNBT(provider, nbt);
    }

    public AreaType getAreaType() {
        return this.areaType;
    }

    @Override
    public CompoundTag serializeNBT(HolderLookup.Provider provider) {
        CompoundTag nbt = new CompoundTag();
        nbt.putString(RegionNBT.AREA_TYPE, this.areaType.areaType);
        return nbt;
    }

    @Override
    public void deserializeNBT(HolderLookup.Provider provider, CompoundTag nbt) {
        this.areaType = AreaType.of(nbt.getString(RegionNBT.AREA_TYPE));
    }
}
