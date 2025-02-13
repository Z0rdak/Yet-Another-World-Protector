package de.z0rdak.yawp.api.core.region;

import de.z0rdak.yawp.core.area.SphereArea;
import de.z0rdak.yawp.core.region.SphereRegion;

public class SphereRegionBuilder extends LocalRegionBuilder<SphereRegion> {

    private SphereArea sphereArea;

    public SphereRegionBuilder(String name) {
        super(name);
    }
    
    public SphereRegionBuilder setArea(SphereArea area) {
        this.sphereArea = area;
        return this;
    }

    @Override
    public SphereRegion build() {
        if (this.name == null || this.name.isEmpty()) {
            throw new IllegalArgumentException("Region name cannot be null or empty");
        }
        if (this.sphereArea == null) {
            throw new IllegalArgumentException("Cuboid area cannot be null");
        }
        if (this.dim == null) {
            throw new IllegalArgumentException("Dimension cannot be null");
        }
        SphereRegion region = new SphereRegion(name, sphereArea, dim);
        region.setPriority(priority);
        region.setIsActive(active);
        region.setIsMuted(muted);
        region.setGroups(this.groups);
        region.setFlags(this.flags);
        return region;
    }
}