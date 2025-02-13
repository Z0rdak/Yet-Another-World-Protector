package de.z0rdak.yawp.api.core.region;

import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.region.CuboidRegion;

public class CuboidRegionBuilder extends LocalRegionBuilder<CuboidRegion> {

    private CuboidArea cuboidArea;

    public CuboidRegionBuilder(String name) {
        super(name);
    }
    
    public CuboidRegionBuilder setArea(CuboidArea area) {
        this.cuboidArea = area;
        return this;
    }

    @Override
    public CuboidRegion build() {
        if (this.name == null || this.name.isEmpty()) {
            throw new IllegalArgumentException("Region name cannot be null or empty");
        }
        if (this.cuboidArea == null) {
            throw new IllegalArgumentException("Cuboid area cannot be null");
        }
        if (this.dim == null) {
            throw new IllegalArgumentException("Dimension cannot be null");
        }
        CuboidRegion region = new CuboidRegion(name, cuboidArea, dim);
        region.setPriority(priority);
        region.setIsActive(active);
        region.setIsMuted(muted);
        region.setGroups(this.groups);
        region.setFlags(this.flags);
        return region;
    }
}