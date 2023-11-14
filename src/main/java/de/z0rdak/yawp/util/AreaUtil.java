package de.z0rdak.yawp.util;

import net.minecraft.core.BlockPos;

import java.util.StringJoiner;

public final class AreaUtil {

    private AreaUtil(){}

    public static double distance(BlockPos a, BlockPos b) {
        return Math.sqrt(Math.pow(b.getX() - a.getX(), 2)
                + Math.pow(b.getY() - a.getY(), 2)
                + Math.pow(b.getZ() - a.getZ(), 2));
    }

    public static double length(BlockPos a) {
        return Math.sqrt(Math.pow(a.getX(), 2)
                + Math.pow(a.getY(), 2)
                + Math.pow(a.getZ(), 2));
    }

    public static String blockPosStr(BlockPos pos) {
        return new StringJoiner(", ", "[", "]")
                .add(pos.getX() + "")
                .add(pos.getY() + "")
                .add(pos.getZ() + "")
                .toString();
    }

    public static BlockPos getLowerPos(BlockPos pos1, BlockPos pos2) {
        int z1 = pos1.getZ();
        int z2 = pos2.getZ();

        if (z1 < z2) {
            return pos1;
        } else {
            return pos2;
        }
    }

    public static BlockPos getHigherPos(BlockPos pos1, BlockPos pos2) {
        int z1 = pos1.getZ();
        int z2 = pos2.getZ();

        if (z1 > z2) {
            return pos1;
        } else {
            return pos2;
        }
    }
}
