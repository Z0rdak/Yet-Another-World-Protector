package de.z0rdak.yawp.core.area;

import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.neoforged.neoforge.common.util.INBTSerializable;

import java.util.List;
import java.util.Set;

/**
 * IMarkableArea provides an interface for different types of areas.
 * The most basic area would be a simple AxisAlignedBB.
 * This type of mark-able area is already implemented in the class CuboidArea.
 */
public interface IMarkableArea extends INBTSerializable<CompoundTag> {

    boolean contains(BlockPos pos);

    AreaType getAreaType();

    List<BlockPos> getMarkedBlocks();

    Set<BlockPos> getHull();

    boolean containsOther(IMarkableArea other);

    boolean intersects(IMarkableArea other);
}
