package de.z0rdak.yawp.core.area;

public abstract class MarkedArea implements IMarkableArea {

    private final AreaType areaType;

    protected MarkedArea(AreaType areaType) {
        this.areaType = areaType;
    }

    public AreaType getAreaType() {
        return this.areaType;
    }
}
