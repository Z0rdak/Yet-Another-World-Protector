package de.z0rdak.yawp.core.area;

import de.z0rdak.yawp.constants.serialization.RegionNbtKeys;
import net.minecraft.nbt.CompoundTag;

public abstract class AbstractArea implements IMarkableArea {

    private AreaType areaType;
    private BlockDisplayProperties displayProperties;

    protected AbstractArea(AreaType areaType) {
        this.areaType = areaType;
        this.displayProperties = new BlockDisplayProperties(
                BlockDisplayProperties.DEFAULT_BLOCK,
                BlockDisplayProperties.DEFAULT_GLOW,
                BlockDisplayProperties.DEFAULT_LIGHT_LEVEL
        );
    }

    protected AbstractArea(CompoundTag nbt) {
        this.deserializeNBT(nbt);
    }

    public AreaType getAreaType() {
        return this.areaType;
    }

    @Override
    public BlockDisplayProperties getDisplay() {
        return displayProperties;
    }

    @Override
    public void updateDisplay(BlockDisplayProperties properties) {
        this.displayProperties = properties;
    }

    @Override
    public CompoundTag serializeNBT() {
        CompoundTag nbt = new CompoundTag();
        nbt.putString(RegionNbtKeys.AREA_TYPE, this.areaType.areaType);
        nbt.put("display", this.displayProperties.serializeNBT());
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        this.displayProperties = new BlockDisplayProperties(nbt);
        this.areaType = AreaType.of(nbt.getString(RegionNbtKeys.AREA_TYPE));
    }
}
