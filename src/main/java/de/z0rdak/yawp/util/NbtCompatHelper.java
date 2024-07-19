package de.z0rdak.yawp.util;

import static de.z0rdak.yawp.util.constants.RegionNBT.TP_POS;

import java.util.Optional;

import net.minecraft.nbt.NbtCompound;
import net.minecraft.nbt.NbtElement;
import net.minecraft.nbt.NbtHelper;
import net.minecraft.nbt.NbtIntArray;
import net.minecraft.util.math.BlockPos;

public class NbtCompatHelper {
	public static Optional<BlockPos> toBlockPosFromCompound(NbtCompound nbt, String key) {
        if (nbt.getType(key) == NbtElement.COMPOUND_TYPE) {
    		NbtCompound c = nbt.getCompound(key);
    		return Optional.of(new BlockPos(c.getInt("X"), c.getInt("Y"), c.getInt("Z")));
        }
		return Optional.empty();
	}
	
	public static Optional<BlockPos> toBlockPos(NbtCompound nbt, String key) {
        Optional <BlockPos> blockPos = NbtHelper.toBlockPos(nbt, key);
        if (blockPos.isEmpty()) {
        	blockPos = toBlockPosFromCompound(nbt, key);
        }
        return blockPos;
    }

    public static Optional<BlockPos> toBlockPos(NbtIntArray nbt) {
        int[] is = nbt.getIntArray();
        if (is.length == 3) {
            return Optional.of(new BlockPos(is[0], is[1], is[2]));
        }
        return Optional.empty();

    }
	
}
