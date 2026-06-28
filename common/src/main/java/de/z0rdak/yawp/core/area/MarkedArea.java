package de.z0rdak.yawp.core.area;

import de.z0rdak.yawp.core.area.visuals.BlockDisplayProperties;

public abstract class MarkedArea implements IMarkableArea {

    private final AreaType areaType;
    private BlockDisplayProperties displayProperties;

    protected MarkedArea(AreaType areaType, BlockDisplayProperties displayProperties) {
        this.areaType = areaType;
        this.displayProperties = displayProperties;
    }

    protected MarkedArea(AreaType areaType) {
        this(areaType, new BlockDisplayProperties(
                BlockDisplayProperties.randomFromDefault(),
                BlockDisplayProperties.DEFAULT_GLOW,
                BlockDisplayProperties.DEFAULT_LIGHT_LEVEL
        ));
    }


    public AreaType getAreaType() {
        return this.areaType;
    }

    @Override
    public BlockDisplayProperties getDisplay() {
        return this.displayProperties;
    }

    @Override
    public void updateDisplay(BlockDisplayProperties properties) {
        this.displayProperties = properties;
    }
}
