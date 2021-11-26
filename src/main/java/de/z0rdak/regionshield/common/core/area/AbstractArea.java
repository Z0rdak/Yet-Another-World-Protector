package de.z0rdak.regionshield.common.core.area;

import net.minecraft.util.math.vector.Vector3d;

public abstract class AbstractArea implements IMarkableArea {

    private final AreaType areaType;

    protected AbstractArea(AreaType areaType) {
        this.areaType = areaType;
    }

    @Override
    public AreaType getAreaType() {
        return this.areaType;
    }

    abstract public Vector3d getCenter();

}
