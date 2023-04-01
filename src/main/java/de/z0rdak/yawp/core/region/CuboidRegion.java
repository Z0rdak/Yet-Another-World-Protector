package de.z0rdak.yawp.core.region;

import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.util.constants.RegionNBT;
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

    public CuboidRegion(String name, CuboidArea area, Player owner, ResourceKey<Level> dimension) {
        super(name, area, new BlockPos((int) area.getArea().getCenter().x,
                (int) area.getArea().getCenter().y, (int) area.getArea().getCenter().z), owner, dimension);
    }

    public CuboidRegion(String name, CuboidArea area, BlockPos tpPos, Player owner, ResourceKey<Level> dimension) {
        super(name, area, tpPos, owner, dimension);
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        super.deserializeNBT(nbt);
        this.area = new CuboidArea(nbt.getCompound(RegionNBT.AREA));
    }
}
