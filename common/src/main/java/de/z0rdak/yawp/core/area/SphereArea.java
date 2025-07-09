package de.z0rdak.yawp.core.area;

import com.mojang.serialization.Codec;
import com.mojang.serialization.MapCodec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import de.z0rdak.yawp.constants.serialization.RegionNbtKeys;
import de.z0rdak.yawp.util.AreaUtil;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.level.levelgen.structure.BoundingBox;
import org.apache.commons.lang3.NotImplementedException;

import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.util.AreaUtil.distance;
import static de.z0rdak.yawp.util.AreaUtil.distanceManhattan;

public class SphereArea extends CenteredArea {

    public static MapCodec<SphereArea> CODEC = RecordCodecBuilder.mapCodec(instance -> instance.group(
                    BlockPos.CODEC.fieldOf("center")
                            .forGetter(SphereArea::getCenterPos),
                    Codec.INT.fieldOf("radius")
                            .forGetter(SphereArea::getRadius),
                    Codec.STRING.fieldOf("areaType")
                            .forGetter(r -> MarkedAreaTypes.areaIdentifier(r.getAreaType()).toString())
            ).apply(instance, (center, radius, area) -> new SphereArea(center, radius))
    );

    private final int radius;

    public SphereArea(BlockPos centerPos, BlockPos scopePos) {
        super(centerPos, AreaType.SPHERE);
        this.radius = (int) (distance(centerPos, scopePos) + 0.5);
    }

    public SphereArea(BlockPos middlePos, int radius) {
        this(middlePos, new BlockPos(middlePos).offset(0, radius, 0));
    }

    public static SphereArea expand(SphereArea area, int expansion) {
        return new SphereArea(area.center, Math.max(area.radius + expansion, 0));
    }

    public int getRadius() {
        return this.radius;
    }

    @Override
    public boolean contains(BlockPos pos) {
        return distance(this.center, pos) < this.radius + 0.5;
    }

    public boolean isHullBlock(BlockPos pos) {
        var d = distance(this.center, pos);
        return d > this.radius - 0.5 && d < this.radius + 0.5;
    }

    @Override
    public Set<BlockPos> getHull() {
        BlockPos p1 = this.center.offset(-this.radius, -this.radius, -this.radius);
        BlockPos p2 = new BlockPos(this.center).offset(this.radius, this.radius, this.radius);
        BoundingBox cube = BoundingBox.fromCorners(p1, p2);
        Set<BlockPos> cubeBlocks = AreaUtil.blocksIn(cube);
        return cubeBlocks.stream().filter(this::isHullBlock).collect(Collectors.toSet());
    }

    @Override
    public Set<BlockPos> getFrame() {
        Set<BlockPos> frameBlocks = this.getMinimalOutline();

        int halfRadius = this.radius / 2;
        frameBlocks.addAll(AreaUtil.getSliceBlocks(this.center, this.radius, halfRadius, Direction.Axis.X, this::isHullBlock));
        frameBlocks.addAll(AreaUtil.getSliceBlocks(this.center, this.radius, -halfRadius, Direction.Axis.X, this::isHullBlock));
        frameBlocks.addAll(AreaUtil.getSliceBlocks(this.center, this.radius, halfRadius, Direction.Axis.Y, this::isHullBlock));
        frameBlocks.addAll(AreaUtil.getSliceBlocks(this.center, this.radius, -halfRadius, Direction.Axis.Y, this::isHullBlock));
        frameBlocks.addAll(AreaUtil.getSliceBlocks(this.center, this.radius, halfRadius, Direction.Axis.Z, this::isHullBlock));
        frameBlocks.addAll(AreaUtil.getSliceBlocks(this.center, this.radius, -halfRadius, Direction.Axis.Z, this::isHullBlock));
        return frameBlocks;
    }

    @Override
    public Set<BlockPos> getMinimalOutline() {
        Set<BlockPos> frameBlocks = new HashSet<>();
        frameBlocks.addAll(AreaUtil.getSliceBlocks(this.center, this.radius, 0, Direction.Axis.X, this::isHullBlock));
        frameBlocks.addAll(AreaUtil.getSliceBlocks(this.center, this.radius, 0, Direction.Axis.Y, this::isHullBlock));
        frameBlocks.addAll(AreaUtil.getSliceBlocks(this.center, this.radius, 0, Direction.Axis.Z, this::isHullBlock));
        return frameBlocks;
    }

    public boolean contains(CuboidArea inner) {
        double maxDistance = Double.NEGATIVE_INFINITY;
        // Calculate the maximum distance from the sphere center to cuboid vertices
        for (BlockPos vertex : inner.getVertices()) {
            double distance = distanceManhattan(this.center, vertex);
            if (distance > maxDistance) {
                maxDistance = distance;
            }
        }
        // Check if the maximum distance is less than or equal to the sphere's radius
        return maxDistance <= this.getRadius();
    }

    public boolean contains(SphereArea inner) {
        return distanceManhattan(this.center, inner.center) + inner.radius <= this.radius;
    }

    public boolean intersects(CuboidArea other) {
        // Check if sphere's center is inside the cuboid
        if (other.contains(this.center)) {
            return true;
        }
        // Compute the closest point on the cuboid to the sphere
        int closestX = Math.max(other.getArea().minX(), Math.min(this.center.getX(), other.getArea().maxX()));
        int closestY = Math.max(other.getArea().minY(), Math.min(this.center.getY(), other.getArea().maxY()));
        int closestZ = Math.max(other.getArea().minZ(), Math.min(this.center.getZ(), other.getArea().maxZ()));
        // Calculate the distance between the sphere center and the closest point
        return distanceManhattan(this.center, new BlockPos(closestX, closestY, closestZ)) <= this.radius;
    }

    public boolean intersects(SphereArea other) {
        return distanceManhattan(this.center, other.center) <= this.radius + other.radius;
    }

    @Override
    public boolean containsOther(IMarkableArea inner) {
        switch (inner.getAreaType()) {
            case CUBOID:
                return this.contains((CuboidArea) inner);
            case SPHERE:
                return this.contains((SphereArea) inner);
            default:
                throw new NotImplementedException("Area type not implemented yet");
        }
    }

    @Override
    public boolean intersects(IMarkableArea other) {
        switch (other.getAreaType()) {
            case CUBOID:
                return this.intersects((CuboidArea) other);
            case SPHERE:
                return this.intersects((SphereArea) other);
            default:
                throw new NotImplementedException("Area type not implemented yet");
        }
    }

    @Override
    public MarkedAreaType<?> getType() {
        return MarkedAreaTypes.SPHERE_AREA;
    }

    @Override
    public String toString() {
        return "Sphere " + AreaUtil.blockPosStr(this.center) + ", r=" + radius;
    }
}
