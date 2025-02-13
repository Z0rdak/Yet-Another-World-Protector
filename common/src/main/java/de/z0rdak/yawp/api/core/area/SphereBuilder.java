package de.z0rdak.yawp.api.core.area;

import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.area.IMarkableArea;
import de.z0rdak.yawp.core.area.SphereArea;
import net.minecraft.core.BlockPos;

public class SphereBuilder implements AreaBuilder {

    private BlockPos center;
    private int radius;

    public SphereBuilder(BlockPos center) {
        this.center = center;
    }

    public SphereBuilder() {
    }

    public SphereBuilder centerAt(BlockPos pos) {
        this.center = pos;
        return this;
    }

    public SphereBuilder centerAt(int x, int y, int z) {
        this.center = new BlockPos(x, y, z);
        return this;
    }

    public SphereBuilder centerAtZero() {
        return this.centerAt(0, 0, 0);
    }

    public SphereBuilder radius(int radius) {
        this.radius = radius;
        return this;
    }

    public SphereBuilder expand(int inc) {
        long newR = (long) this.radius + (long) Math.abs(inc);
        this.radius = (int) Math.min(newR, Integer.MAX_VALUE); // prevent overflow
        return this;
    }

    public SphereBuilder shrink(int dec) {
        long newR = (long) this.radius - (long) Math.abs(dec);
        this.radius = (int) Math.max(newR, 1);  // prevent zero or negative radius
        return this;
    }

    @Override
    public SphereArea build() {
        if (this.center == null)
            throw new IllegalArgumentException("No center specified");
        return new SphereArea(this.center, this.radius);
    }
}