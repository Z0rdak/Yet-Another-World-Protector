package de.z0rdak.yawp.core.area;

public abstract class AbstractArea implements IMarkableArea {

    private final AreaType areaType;

    protected AbstractArea(AreaType areaType) {
        this.areaType = areaType;
    }

    public AreaType getAreaType() {
        return this.areaType;
    }
}
