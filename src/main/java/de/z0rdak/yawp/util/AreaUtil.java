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

    public static String blockPosStr(BlockPos pos){
        return new StringJoiner(", ", "[", "]")
                .add(pos.getX() + "")
                .add(pos.getY() + "")
                .add(pos.getZ() + "")
                .toString();
    }
}
