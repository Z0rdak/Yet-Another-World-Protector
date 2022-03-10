package de.z0rdak.regionshield.core.area;

import de.z0rdak.regionshield.RegionShield;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.vector.Vector3d;

import static de.z0rdak.regionshield.util.AreaUtil.*;

// TODO: how to orientation?
public class CylinderArea extends CenteredArea {

    private BlockPos centerTopPos;
    private int distance;
    private int radius;

    private CylinderArea() {
        super(AreaType.CYLINDER);
    }

    public CylinderArea(BlockPos centerBottomPos, BlockPos scopePos){
        super(centerBottomPos, AreaType.CYLINDER);
        this.centerTopPos = new BlockPos(centerBottomPos.getX(), scopePos.getY(), centerBottomPos.getZ());
        this.radius = (int) (distance(centerBottomPos, new BlockPos(scopePos.getX(), centerBottomPos.getY(), scopePos.getZ())));
        this.distance = (int) (distance(centerBottomPos, this.centerTopPos) + 0.5);
        RegionShield.LOGGER.debug(this.toString());
    }

    public CylinderArea(BlockPos centerBottomPos, int radius, int distance){
        super(centerBottomPos, AreaType.CYLINDER);
        this.centerTopPos = centerBottomPos.offset(0, distance, 0);
        this.radius =radius;
        this.distance = distance;
        RegionShield.LOGGER.debug(this.toString());
    }

    public CylinderArea(CompoundNBT nbt){
        this();
        this.deserializeNBT(nbt);
    }

    public Vector3d getCenter() {
        return new Vector3d(this.center.getX(), this.center.getY(), this.center.getZ());
    }

    /***
     * https://stackoverflow.com/questions/47932955/how-to-check-if-a-3d-point-is-inside-a-cylinder
     * @param pos
     * @return
     */
    @Override
    public boolean contains(BlockPos pos) {
        BlockPos dist = centerTopPos.subtract(center);
        boolean b1 = multiply(pos.subtract(center), dist).compareTo(BlockPos.ZERO) >= 0;
        boolean b2 = multiply(pos.subtract(centerTopPos), dist).compareTo(BlockPos.ZERO) <= 0;
        boolean isBetweenPlanes = b1 && b2;
        BlockPos crossProduct = pos.subtract(center).cross(dist);
        double distance = length(crossProduct) / distance(centerTopPos, center);
        boolean isInsideSurface = distance <= (radius - 0.5);
        return isBetweenPlanes && isInsideSurface;
    }

    public int getDistance() {
        return distance;
    }

    public int getRadius() {
        return radius;
    }

    public BlockPos multiply(BlockPos p1, BlockPos p2){
        return new BlockPos(p1.getX() * p2.getX(), p1.getY() * p2.getY(), p1.getZ() * p2.getZ());
    }

    @Override
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = super.serializeNBT();
        nbt.putInt("radius", this.radius);
        nbt.putInt("height", this.distance);
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        super.deserializeNBT(nbt);
        this.distance = nbt.getInt("height");
        this.radius = nbt.getInt("radius");
    }

    @Override
    public String toString() {
        return "CylinderArea: [" +
                "bottomCenter=" + center.toShortString() +
                ", distance=" + distance +
                ", radius=" + radius +
                ']';
    }
}
