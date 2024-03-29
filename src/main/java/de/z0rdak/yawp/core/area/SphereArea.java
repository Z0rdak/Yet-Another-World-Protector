package de.z0rdak.yawp.core.area;

import de.z0rdak.yawp.util.AreaUtil;
import de.z0rdak.yawp.util.constants.AreaNBT;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.vector.Vector3d;

import static de.z0rdak.yawp.util.AreaUtil.distance;

public class SphereArea extends CenteredArea {

    private int radius;

    public SphereArea(CompoundNBT nbt) {
        super(nbt);
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
        nbt.putInt(AreaNBT.RADIUS, this.radius);
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        super.deserializeNBT(nbt);
        this.radius = nbt.getInt(AreaNBT.RADIUS);
    }

    @Override
    public String toString() {
        return "Sphere " + AreaUtil.blockPosStr(this.center) + ", r=" + radius;
    }
}
