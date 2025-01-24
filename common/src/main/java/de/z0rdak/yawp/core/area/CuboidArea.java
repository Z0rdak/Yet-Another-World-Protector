package de.z0rdak.yawp.core.area;

import de.z0rdak.yawp.constants.serialization.RegionNbtKeys;
import de.z0rdak.yawp.util.AreaUtil;
import de.z0rdak.yawp.util.NbtCompatHelper;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtUtils;
import net.minecraft.world.level.levelgen.structure.BoundingBox;
import org.apache.commons.lang3.NotImplementedException;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static de.z0rdak.yawp.util.AreaUtil.blocksBetweenOnAxis;
import static de.z0rdak.yawp.util.AreaUtil.distanceManhattan;

/**
 * Represents and wraps a simple AxisAlignedBB.
 * This area is marked by two positions and thus spans a cuboid shape
 */
public class CuboidArea extends AbstractArea {

    private BoundingBox area;
    private BlockPos p1;
    private BlockPos p2;

    public CuboidArea(BoundingBox area) {
        super(AreaType.CUBOID);
        this.area = area;
    }

    public CuboidArea(BlockPos p1, BlockPos p2) {
        this(BoundingBox.fromCorners(p1, p2));
        this.p1 = AreaUtil.getLowerPos(p1, p2);
        this.p2 = AreaUtil.getHigherPos(p1, p2);
    }

    public CuboidArea(CompoundTag nbt) {
        super(nbt);
        this.deserializeNBT(nbt);
    }

    public static CuboidArea expand(CuboidArea area, int min, int max) {
        BlockPos p1 = area.getAreaP1();
        BlockPos p2 = area.getAreaP2();
        return new CuboidArea(new BlockPos(p1.getX(), min, p1.getZ()),
                new BlockPos(p2.getX(), max, p2.getZ()));
    }

    private static boolean isInFacePlane(BlockPos point, BlockPos corner1, BlockPos corner2, BlockPos corner3, BlockPos corner4) {
        // Check if the point is within the plane defined by the four corners
        // This can be done by checking if the point is within the bounding box of the face
        return point.getX() >= corner1.getX() && point.getX() <= corner2.getX()
                && point.getY() >= corner1.getY() && point.getY() <= corner3.getY()
                && point.getZ() >= corner1.getZ() && point.getZ() <= corner4.getZ();
    }

    private static List<BlockPos> getBlocksInFace(BlockPos corner1, BlockPos corner2, BlockPos corner3, BlockPos corner4) {
        List<BlockPos> blocksInFace = new ArrayList<>();
        // Determine the min and max coordinates along each axis
        int minX = Math.min(Math.min(corner1.getX(), corner2.getX()), Math.min(corner3.getX(), corner4.getX()));
        int minY = Math.min(Math.min(corner1.getY(), corner2.getY()), Math.min(corner3.getY(), corner4.getY()));
        int minZ = Math.min(Math.min(corner1.getZ(), corner2.getZ()), Math.min(corner3.getZ(), corner4.getZ()));
        int maxX = Math.max(Math.max(corner1.getX(), corner2.getX()), Math.max(corner3.getX(), corner4.getX()));
        int maxY = Math.max(Math.max(corner1.getY(), corner2.getY()), Math.max(corner3.getY(), corner4.getY()));
        int maxZ = Math.max(Math.max(corner1.getZ(), corner2.getZ()), Math.max(corner3.getZ(), corner4.getZ()));
        // Iterate through the grid defined by the min and max coordinates
        for (int x = minX; x <= maxX; x++) {
            for (int y = minY; y <= maxY; y++) {
                for (int z = minZ; z <= maxZ; z++) {
                    BlockPos currentPos = new BlockPos(x, y, z);
                    if (isInFacePlane(currentPos, corner1, corner2, corner3, corner4)) {
                        blocksInFace.add(currentPos);
                    }
                }
            }
        }
        return blocksInFace;
    }

    @Override
    public boolean contains(BlockPos pos) {
        // INFO: this.area.contains(x,y,z); does not work, because the max checks are exclusive by default.
        // TODO: Maybe replace with net.minecraft.util.math.MutableBoundingBox::intersectsWith which has inclusive checks
        return pos.getX() >= area.minX() && pos.getX() <= area.maxX()
                && pos.getY() >= this.area.minY() && pos.getY() <= this.area.maxY()
                && pos.getZ() >= this.area.minZ() && pos.getZ() <= this.area.maxZ();
    }

    public boolean contains(CuboidArea inner) {
        return this.area.minX() <= inner.area.minX() && this.area.maxX() >= inner.area.maxX()
                && this.area.minY() <= inner.area.minY() && this.area.maxY() >= inner.area.maxY()
                && this.area.minZ() <= inner.area.minZ() && this.area.maxZ() >= inner.area.maxZ();
    }

    public boolean contains(SphereArea inner) {
        int sphereRadius = inner.getRadius();
        BlockPos center = inner.center;
        // Bounding box check
        if (!this.intersects(inner)) {
            return false; // Cuboid and sphere do not intersect, sphere cannot be contained
        }

        // Bounding sphere check
        int maxDistanceToCorner = maxDistanceToCorners(center);
        if (maxDistanceToCorner > sphereRadius) {
            return false; // Maximum distance to cuboid corner is greater than sphere's radius
        }

        // Iterate over a cube around the sphere to generate points
        for (int x = center.getX() - sphereRadius; x <= center.getX() + sphereRadius; x++) {
            for (int y = center.getY() - sphereRadius; y <= center.getY() + sphereRadius; y++) {
                for (int z = center.getZ() - sphereRadius; z <= center.getZ() + sphereRadius; z++) {
                    BlockPos currentPos = new BlockPos(x, y, z);
                    int distance = distanceManhattan(center, currentPos);
                    // Check if the point is within or on the surface of the sphere
                    if (distance <= sphereRadius) {
                        // Check if the point is within the cuboid
                        if (!this.contains(currentPos)) {
                            return false; // Point is outside cuboid, sphere is not contained
                        }
                    }
                }
            }
        }
        return true; // All points within or on the sphere are contained in the cuboid
    }

    private int maxDistanceToCorners(BlockPos center) {
        List<BlockPos> corners = getVertices();
        int maxDistance = Integer.MIN_VALUE;
        for (BlockPos corner : corners) {
            int distance = distanceManhattan(center, corner);
            if (distance > maxDistance) {
                maxDistance = distance;
            }
        }
        return maxDistance;
    }

    /**
     * Returns the vertices of the cuboid area as a list of BlockPos.
     * Z+
     * p7-----p8
     * /|      /|
     * Y+  p5------p6|
     * | |     | |
     * | p3----|-p4
     * |/      |/
     * p1------p2  X+
     *
     * @return [p1, p2, p3, p4, p5, p6, p7, p8] as list of BlockPos
     */
    public List<BlockPos> getVertices() {
        BlockPos p1 = new BlockPos(this.area.minX(), this.area.minY(), this.area.minZ());
        BlockPos p2 = new BlockPos(this.area.maxX(), this.area.minY(), this.area.minZ());
        BlockPos p3 = new BlockPos(this.area.minX(), this.area.minY(), this.area.maxZ());
        BlockPos p4 = new BlockPos(this.area.maxX(), this.area.minY(), this.area.maxZ());
        BlockPos p5 = new BlockPos(this.area.minX(), this.area.maxY(), this.area.minZ());
        BlockPos p6 = new BlockPos(this.area.maxX(), this.area.maxY(), this.area.minZ());
        BlockPos p7 = new BlockPos(this.area.minX(), this.area.maxY(), this.area.maxZ());
        BlockPos p8 = new BlockPos(this.area.maxX(), this.area.maxY(), this.area.maxZ());
        return Arrays.asList(p1, p2, p3, p4, p5, p6, p7, p8);
    }

    /**
     * Returns the hull of the cuboid area as a set of BlockPos.
     * The hull is the outermost layer of blocks of the cuboid area.
     * The hull is calculated by iterating through the faces of the cuboid area and collecting the blocks in each face.
     * @return hull as set of BlockPos of cuboid area
     */
    @Override
    public Set<BlockPos> getHull() {
        List<BlockPos> vertices = getVertices();
        List<BlockPos> face1 = getBlocksInFace(vertices.get(0), vertices.get(1), vertices.get(2), vertices.get(3));
        List<BlockPos> face2 = getBlocksInFace(vertices.get(4), vertices.get(5), vertices.get(6), vertices.get(7));
        List<BlockPos> face3 = getBlocksInFace(vertices.get(0), vertices.get(2), vertices.get(4), vertices.get(6));
        List<BlockPos> face4 = getBlocksInFace(vertices.get(1), vertices.get(3), vertices.get(5), vertices.get(7));
        List<BlockPos> face5 = getBlocksInFace(vertices.get(0), vertices.get(1), vertices.get(4), vertices.get(5));
        List<BlockPos> face6 = getBlocksInFace(vertices.get(2), vertices.get(3), vertices.get(6), vertices.get(7));
        return Stream.of(face1, face2, face3, face4, face5, face6)
                .flatMap(List::stream)
                .collect(Collectors.toSet());
    }

    private boolean intersects(CuboidArea other) {
        return this.area.intersects(other.area);
    }

    public boolean intersects(SphereArea other) {
        return other.intersects(this);
    }

    public BoundingBox getArea() {
        return area;
    }

    public int getXsize() {
        return Math.max(this.area.getXSpan(), 1);
    }

    public int getZsize() {
        return Math.max(this.area.getZSpan(), 1);
    }

    public int getYsize() {
        return Math.max(this.area.getYSpan(), 1);
    }

    public BlockPos getAreaP1() {
        return this.p1;
    }

    public BlockPos getAreaP2() {
        return this.p2;
    }

    @Override
    public CompoundTag serializeNBT() {
        CompoundTag nbt = super.serializeNBT();
        nbt.put(RegionNbtKeys.P1, NbtUtils.writeBlockPos(this.p1));
        nbt.put(RegionNbtKeys.P2, NbtUtils.writeBlockPos(this.p2));
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        super.deserializeNBT(nbt);
        this.p1 = NbtCompatHelper.toBlockPos(nbt, RegionNbtKeys.P1).orElseThrow();
        this.p2 = NbtCompatHelper.toBlockPos(nbt, RegionNbtKeys.P2).orElseThrow();
        this.area = BoundingBox.fromCorners(p1, p2);
    }

    @Override
    public String toString() {
        return getAreaType().areaType + " " + AreaUtil.blockPosStr(this.p1) + " <-> " + AreaUtil.blockPosStr(this.p2) +
                "\n" + "Size: " + "X=" + this.getXsize() + ", Y=" + this.getYsize() + ", Z=" + this.getZsize() +
                "\n" + "Blocks: " + AreaUtil.blockPosStr(this.p1) + ", " + AreaUtil.blockPosStr(this.p2);
    }

    @Override
    public List<BlockPos> markedBlocks() {
        return Arrays.asList(this.p1, this.p2);
    }

    /**
     * Returns the outer frame of the cuboid area as a set of BlockPos.
     *
     * @return outer frame as set of BlockPos of cuboid area
     */
    public Set<BlockPos> getFrame() {
        List<BlockPos> vertices = getVertices();
        Set<BlockPos> p12 = blocksBetweenOnAxis(vertices.get(0), vertices.get(1), Direction.Axis.X);
        Set<BlockPos> p34 = blocksBetweenOnAxis(vertices.get(2), vertices.get(3), Direction.Axis.X);
        Set<BlockPos> p56 = blocksBetweenOnAxis(vertices.get(4), vertices.get(5), Direction.Axis.X);
        Set<BlockPos> p78 = blocksBetweenOnAxis(vertices.get(6), vertices.get(7), Direction.Axis.X);
        Set<BlockPos> p15 = blocksBetweenOnAxis(vertices.get(0), vertices.get(4), Direction.Axis.Y);
        Set<BlockPos> p26 = blocksBetweenOnAxis(vertices.get(1), vertices.get(5), Direction.Axis.Y);
        Set<BlockPos> p37 = blocksBetweenOnAxis(vertices.get(2), vertices.get(6), Direction.Axis.Y);
        Set<BlockPos> p48 = blocksBetweenOnAxis(vertices.get(3), vertices.get(7), Direction.Axis.Y);
        Set<BlockPos> p13 = blocksBetweenOnAxis(vertices.get(0), vertices.get(2), Direction.Axis.Z);
        Set<BlockPos> p24 = blocksBetweenOnAxis(vertices.get(1), vertices.get(3), Direction.Axis.Z);
        Set<BlockPos> p57 = blocksBetweenOnAxis(vertices.get(4), vertices.get(6), Direction.Axis.Z);
        Set<BlockPos> p68 = blocksBetweenOnAxis(vertices.get(5), vertices.get(7), Direction.Axis.Z);
        return Stream.of(p12, p34, p56, p78, p15, p26, p37, p48, p13, p24, p57, p68)
                .flatMap(Set::stream)
                .collect(Collectors.toSet());
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
}
