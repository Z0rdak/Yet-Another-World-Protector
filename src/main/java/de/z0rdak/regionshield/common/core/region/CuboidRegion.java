package de.z0rdak.regionshield.common.core.region;

import de.z0rdak.regionshield.common.core.area.CuboidArea;
import de.z0rdak.regionshield.common.util.constants.RegionNBT;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.math.AxisAlignedBB;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

/**
 * A cuboid regions represents it's area as a simple rectangular cuboid (a BlockBox).
 * The region is marked with two blocks representing the bounding box of the area.
 */
public final class CuboidRegion extends AbstractMarkableRegion {

	public CuboidRegion(CompoundNBT nbt) {
		this.deserializeNBT(nbt);
	}

	public CuboidRegion(String name, AxisAlignedBB area, RegistryKey<World> dimension) {
		super(name, new CuboidArea(area), dimension);
		this.tpTarget = new BlockPos((int) area.getCenter().x,
				(int) area.getCenter().y,
				(int) area.getCenter().z);
	}

	public CuboidRegion(String name, AxisAlignedBB area, BlockPos tpPos, RegistryKey<World> dimension) {
		super(name, new CuboidArea(area), dimension);
		this.tpTarget = tpPos;
	}

	@Override
	public boolean containsPosition(BlockPos position) {
		return this.area.contains(position);
	}

	@Override
	public CompoundNBT serializeNBT() {
		CompoundNBT nbt = super.serializeNBT();
		nbt.put(RegionNBT.AREA, this.area.serializeNBT());
		return nbt;
	}

	@Override
	public void deserializeNBT(CompoundNBT nbt) {
		super.deserializeNBT(nbt);
		this.area = new CuboidArea(nbt.getCompound(RegionNBT.AREA));
	}
}
