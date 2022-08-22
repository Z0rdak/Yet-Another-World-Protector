package de.z0rdak.yawp.core.region;

import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.util.constants.RegionNBT;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.RegistryKey;
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
		super(name, area, new BlockPos(area.getArea().getCenter()), owner, dimension);
	}

	public CuboidRegion(String name, CuboidArea area, BlockPos tpPos, PlayerEntity owner, RegistryKey<World> dimension) {
		super(name, area, tpPos, owner, dimension);
	}

	@Override
	public void deserializeNBT(CompoundNBT nbt) {
		super.deserializeNBT(nbt);
		this.area = new CuboidArea(nbt.getCompound(RegionNBT.AREA));
	}
}
