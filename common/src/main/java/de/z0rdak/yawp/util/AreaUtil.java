package de.z0rdak.yawp.util;

import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.level.levelgen.structure.BoundingBox;

import java.util.HashSet;
import java.util.Set;
import java.util.StringJoiner;

public final class AreaUtil {

    private AreaUtil() {
    }

    public static double distance(BlockPos a, BlockPos b) {
        return Math.sqrt(Math.pow(b.getX() - a.getX(), 2)
                + Math.pow(b.getY() - a.getY(), 2)
                + Math.pow(b.getZ() - a.getZ(), 2));
    }

    public static int distanceManhattan(BlockPos a, BlockPos b) {
        return Math.abs(b.getX() - a.getX())
                + Math.abs(b.getY() - a.getY())
                + Math.abs(b.getZ() - a.getZ());
    }

    public static double length(BlockPos a) {
        return Math.sqrt(Math.pow(a.getX(), 2)
                + Math.pow(a.getY(), 2)
                + Math.pow(a.getZ(), 2));
    }

    public static String blockPosStr(BlockPos pos) {
        return new StringJoiner(", ", "[", "]")
                .add(String.valueOf(pos.getX()))
                .add(String.valueOf(pos.getY()))
                .add(String.valueOf(pos.getZ()))
                .toString();
    }

    public static BlockPos getLowerPos(BlockPos pos1, BlockPos pos2) {
        return pos1.getZ() < pos2.getZ() ? pos1 : pos2;
    }

    public static BlockPos getHigherPos(BlockPos pos1, BlockPos pos2) {
        return pos1.getZ() > pos2.getZ() ? pos1 : pos2;
    }

    public static Set<BlockPos> blocksBetweenOnAxis(BlockPos p1, BlockPos p2, Direction.Axis axis) {
        BoundingBox blockLine = BoundingBox.fromCorners(p1, p2);
        Set<BlockPos> blocks = new HashSet<>();
        switch (axis) {
            case X:
                for (int x = blockLine.minX(); x <= blockLine.maxX(); x++) {
                    blocks.add(new BlockPos(x, p1.getY(), p1.getZ()));
                }
                break;
            case Y:
                for (int y = blockLine.minY(); y <= blockLine.maxY(); y++) {
                    blocks.add(new BlockPos(p1.getX(), y, p1.getZ()));
                }
                break;
            case Z:
                for (int z = blockLine.minZ(); z <= blockLine.maxZ(); z++) {
                    blocks.add(new BlockPos(p1.getX(), p1.getY(), z));
                }
                break;
        }
        return blocks;
    }

    public static Set<BlockPos> blocksBetween(BoundingBox cube) {
        Set<BlockPos> blocks = new HashSet<>();
        for (int x = cube.minX(); x <= cube.maxX(); x++) {
            for (int y = cube.minY(); y <= cube.maxY(); y++) {
                for (int z = cube.minZ(); z <= cube.maxZ(); z++) {
                    blocks.add(new BlockPos(x, y, z));
                }
            }
        }
        return blocks;
    }

    public static int blocksOnAxis(BoundingBox box, Direction.Axis axis) {
        switch (axis) {
            case X:
                return box.getXSpan();
            case Y:
                return box.getYSpan();
            case Z:
                return box.getZSpan();
            default:
                throw new IllegalArgumentException();
        }
    }
}
