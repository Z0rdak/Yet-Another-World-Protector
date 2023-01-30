package de.z0rdak.yawp.core.area;

import de.z0rdak.yawp.core.INbtSerializable;
import net.minecraft.nbt.NbtCompound;
import net.minecraft.util.math.BlockPos;

import java.util.List;

/**
 * IMarkableArea provides an interface for different types of areas.
 * The most basic area would be a simple AxisAlignedBB.
 * This type of mark-able area is already implemented in the class CuboidArea.
 */
public interface IMarkableArea extends INbtSerializable<NbtCompound> {

    boolean contains(BlockPos pos);

    AreaType getAreaType();

    List<BlockPos> getMarkedBlocks();
}
