package de.z0rdak.yawp.core.region;

import de.z0rdak.yawp.constants.serialization.RegionNbtKeys;
import de.z0rdak.yawp.core.area.CuboidArea;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;

/**
 * A cuboid regions represents it's area as a simple rectangular cuboid (a BlockBox).
 * The region is marked with two blocks representing the bounding box of the area.
 */
public final class CuboidRegion extends AbstractMarkableRegion {

    public CuboidRegion(CompoundTag nbt) {
        super(nbt);
        this.deserializeNBT(nbt);
    }

    public CuboidRegion(String name, CuboidArea area, ResourceKey<Level> dim) {
        super(name, area, area.getArea().getCenter(), null, dim);
    }

    public CuboidRegion(String name, CuboidArea area, Player owner, ResourceKey<Level> dimension) {
        super(name, area, area.getArea().getCenter(), owner, dimension);
    }

    public CuboidRegion(String name, CuboidArea area, BlockPos tpPos, Player owner, ResourceKey<Level> dimension) {
        super(name, area, tpPos, owner, dimension);
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        super.deserializeNBT(nbt);
        this.area = new CuboidArea(nbt.getCompound(RegionNbtKeys.AREA));
    }
}
