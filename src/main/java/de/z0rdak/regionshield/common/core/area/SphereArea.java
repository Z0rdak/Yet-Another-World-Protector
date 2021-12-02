package de.z0rdak.regionshield.common.core.area;

import de.z0rdak.regionshield.RegionShield;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.vector.Vector3d;

import static de.z0rdak.regionshield.common.core.area.AreaUtil.distance;

public class SphereArea extends CenteredArea {

    private int radius;

    private SphereArea() {
        super(AreaType.SPHERE);
    }

    public SphereArea(CompoundNBT nbt) {
        this();
        this.deserializeNBT(nbt);
    }

    public SphereArea(BlockPos centerPos, BlockPos scopePos){
        super(centerPos, AreaType.SPHERE);
        this.radius = (int) (distance(centerPos, scopePos) + 0.5);
    }

    @Override
    public Vector3d getCenter() {
        return new Vector3d(this.center.getX(), this.center.getY(), this.center.getZ());
    }

    public int getRadius() {
        return this.radius;
    }

    // TODO: MAX sphere radius
    public SphereArea(BlockPos middlePos, int radius){
        this(middlePos, new BlockPos(middlePos)
                .offset(0, radius, 0));
    }

    @Override
    public boolean contains(BlockPos pos) {
        return distance(this.center, pos) < this.radius + 0.5;
    }

    @Override
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = super.serializeNBT();
        nbt.putInt("radius", this.radius);
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        super.deserializeNBT(nbt);
        this.radius = nbt.getInt("radius");
    }

    @Override
    public String toString() {
        return "SphereArea " + AreaUtil.toString(this.center) + ", r=" + radius;
    }
}
