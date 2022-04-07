package de.z0rdak.regionshield.core.region;

import de.z0rdak.regionshield.core.area.CuboidArea;
import de.z0rdak.regionshield.util.constants.RegionNBT;
import net.minecraft.entity.player.PlayerEntity;
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

	public CuboidRegion(String name, CuboidArea area, PlayerEntity owner, RegistryKey<World> dimension) {
		this(name, area, new BlockPos(area.getArea().getCenter()), owner, dimension);
	}

	public CuboidRegion(String name, CuboidArea area, BlockPos tpPos, PlayerEntity owner, RegistryKey<World> dimension) {
		super(name, area, owner, dimension);
		this.tpTarget = tpPos;
	}

	@Override
	public boolean contains(BlockPos position) {
		return this.area.contains(position);
	}

	// TODO: maybe this is not needed and the base class method could be used
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
