package de.z0rdak.yawp.core.area;

import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.math.BlockPos;
import net.minecraftforge.common.util.INBTSerializable;

/**
 * IMarkableArea provides an interface for different types of areas.
 * The most basic area would be a simple AxisAlignedBB.
 * This type of mark-able area is already implemented in the class CuboidArea.
 */
public interface IMarkableArea extends INBTSerializable<CompoundNBT> {

    boolean contains(BlockPos pos);

    AreaType getAreaType();
}
