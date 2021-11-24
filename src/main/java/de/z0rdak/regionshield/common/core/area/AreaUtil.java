package de.z0rdak.regionshield.common.core.area;

import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.math.BlockPos;

import java.util.StringJoiner;

public final class AreaUtil {

    private AreaUtil(){}

    public static String toString(BlockPos pos){
        String posStr = new StringJoiner(", ", "[", "]")
                .add(pos.getX() + "")
                .add(pos.getY() + "")
                .add(pos.getZ() + "")
                .toString();
        return posStr;
    }
}
