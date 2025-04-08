package de.z0rdak.yawp.util;

import net.minecraft.core.BlockPos;
import net.minecraft.nbt.*;

import java.util.Optional;

public class NbtCompatHelper {

	public static Optional<BlockPos> asBlockPos(CompoundTag nbt, String key) {
        Optional<int[]> intArray = nbt.getIntArray(key);
        if (intArray.isPresent() && intArray.get().length == 3) {
            int[] blockPosInts = intArray.get();
            BlockPos blockPos = new BlockPos(blockPosInts[0], blockPosInts[1], blockPosInts[2]);
            return Optional.of(blockPos);
        }
        return Optional.empty();
    }

    public static Optional<BlockPos> asBlockPos(IntArrayTag nbt) {
        return asBlockPos(nbt.getAsIntArray());
    }

    public static Optional<BlockPos> asBlockPos(int[] blockPosInts) {
        if (blockPosInts.length == 3) {
            return Optional.of(new BlockPos(blockPosInts[0], blockPosInts[1], blockPosInts[2]));
        }
        return Optional.empty();
    }

    public static IntArrayTag asInts(BlockPos pos) {
        var coords = new int[]{pos.getX(), pos.getY(), pos.getZ()};
        return new IntArrayTag(coords);
    }
}
