package de.z0rdak.yawp.core.area;

import de.z0rdak.yawp.constants.serialization.RegionNbtKeys;
import de.z0rdak.yawp.util.AreaUtil;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Vec3i;
import net.minecraft.nbt.CompoundTag;
import org.apache.commons.lang3.NotImplementedException;

import java.util.Set;

import static de.z0rdak.yawp.util.AreaUtil.distanceManhattan;
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
        this.radius = distanceManhattan(centerBottomPos, new BlockPos(scopePos.getX(), centerBottomPos.getY(), scopePos.getZ()));
        this.distance = distanceManhattan(centerBottomPos, this.centerTopPos);
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

    public Vec3i getCenter() {
        return new Vec3i(this.center.getX(), this.center.getY(), this.center.getZ());
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
        double distance = length(crossProduct) / distanceManhattan(centerTopPos, center);
        boolean isInsideSurface = distance <= radius;
        return isBetweenPlanes && isInsideSurface;
    }

    @Override
    public Set<BlockPos> getHull() {
        throw new NotImplementedException("ChunkArea.getHull() not implemented yet");
    }

    public int getDistance() {
        return distance;
    }

    public int getRadius() {
        return radius;
    }

    public BlockPos multiply(BlockPos p1, BlockPos p2) {
        return new BlockPos(p1.getX() * p2.getX(), p1.getY() * p2.getY(), p1.getZ() * p2.getZ());
    }

    @Override
    public CompoundTag serializeNBT() {
        CompoundTag nbt = super.serializeNBT();
        nbt.putInt(RegionNbtKeys.RADIUS, this.radius);
        nbt.putInt(RegionNbtKeys.HEIGHT, this.distance);
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        super.deserializeNBT(nbt);
        this.distance = nbt.getInt(RegionNbtKeys.RADIUS);
        this.radius = nbt.getInt(RegionNbtKeys.HEIGHT);
    }


    @Override
    public boolean containsOther(IMarkableArea other) {
        throw new NotImplementedException("Not yet implemented");
    }

    @Override
    public boolean intersects(IMarkableArea other) {
        throw new NotImplementedException("Not yet implemented");
    }

    // Cylinder [x,y,z] with radius r and height h
    @Override
    public String toString() {
        return "Cylinder " + AreaUtil.blockPosStr(this.center) + " with radius " + this.radius + " and height " + this.distance + ".";
    }
}
