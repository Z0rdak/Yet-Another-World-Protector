package de.z0rdak.yawp.core.area;

import com.mojang.math.Vector3d;
import de.z0rdak.yawp.util.AreaUtil;
import de.z0rdak.yawp.util.constants.AreaNBT;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;

import static de.z0rdak.yawp.util.AreaUtil.distance;
import static de.z0rdak.yawp.util.AreaUtil.length;

/**
 * Vertical, cylindrical area defined by the bottom center position and a perimeter position.
 * The perimeter position defines both the height and radius of the area.
 */
public class VerticalCylinderArea extends CenteredArea {

    private BlockPos centerTopPos;
    private int distance;
    private int radius;

    public VerticalCylinderArea(BlockPos centerBottomPos, BlockPos scopePos) {
        super(centerBottomPos, AreaType.CYLINDER);
        this.centerTopPos = new BlockPos(centerBottomPos.getX(), scopePos.getY(), centerBottomPos.getZ());
        this.radius = (int) (distance(centerBottomPos, new BlockPos(scopePos.getX(), centerBottomPos.getY(), scopePos.getZ())));
        this.distance = (int) (distance(centerBottomPos, this.centerTopPos) + 0.5);
    }

    public VerticalCylinderArea(BlockPos centerBottomPos, int radius, int distance) {
        super(centerBottomPos, AreaType.CYLINDER);
        this.centerTopPos = centerBottomPos.offset(0, distance, 0);
        this.radius = radius;
        this.distance = distance;
    }

    public VerticalCylinderArea(CompoundTag nbt) {
        super(nbt);
        this.deserializeNBT(nbt);
    }

    public Vector3d getCenter() {
        return new Vector3d(this.center.getX(), this.center.getY(), this.center.getZ());
    }

    /***
     * <a href="https://stackoverflow.com/questions/47932955/how-to-check-if-a-3d-point-is-inside-a-cylinder">...</a>
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
    public CompoundTag serializeNBT() {
        CompoundTag nbt = super.serializeNBT();
        nbt.putInt(AreaNBT.RADIUS, this.radius);
        nbt.putInt(AreaNBT.HEIGHT, this.distance);
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        super.deserializeNBT(nbt);
        this.distance = nbt.getInt(AreaNBT.RADIUS);
        this.radius = nbt.getInt(AreaNBT.HEIGHT);
    }

    // Cylinder [x,y,z] with radius r and height h
    @Override
    public String toString() {
        return "Cylinder " + AreaUtil.blockPosStr(this.center) + " with radius " + this.radius + " and height " + this.distance + ".";
    }
}
