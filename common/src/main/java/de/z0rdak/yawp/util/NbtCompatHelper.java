package de.z0rdak.yawp.util;

import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.IntArrayTag;
import net.minecraft.nbt.NbtUtils;
import net.minecraft.nbt.Tag;

import java.util.Optional;

public class NbtCompatHelper {
	public static Optional<BlockPos> toBlockPosFromCompound(CompoundTag nbt, String key) {
        if (nbt.getTagType(key) == Tag.TAG_COMPOUND) {
    		return readBlockPosFromCompound(nbt.getCompound(key));
        }
		return Optional.empty();
	}

    private static Optional<BlockPos> readBlockPosFromCompound(CompoundTag nbt) {
        return Optional.of(new BlockPos(nbt.getInt("X"), nbt.getInt("Y"), nbt.getInt("Z")));
    }
	
	public static Optional<BlockPos> toBlockPos(CompoundTag nbt, String key) {
        Optional <BlockPos> blockPos = NbtUtils.readBlockPos(nbt, key);
        if (blockPos.isEmpty()) {
        	blockPos = toBlockPosFromCompound(nbt, key);
        }
        return blockPos;
    }

    public static Optional<BlockPos> toBlockPos(IntArrayTag nbt) {
        int[] is = nbt.getAsIntArray();
        if (is.length == 3) {
            return Optional.of(new BlockPos(is[0], is[1], is[2]));
        }
        return Optional.empty();

    }
	
}
