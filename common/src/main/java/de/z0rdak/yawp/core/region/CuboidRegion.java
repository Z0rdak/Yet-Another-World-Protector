package de.z0rdak.yawp.core.region;

import de.z0rdak.yawp.constants.serialization.RegionNbtKeys;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.area.IMarkableArea;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.group.PlayerContainer;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;

import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * A cuboid regions represents it's area as a simple rectangular cuboid (a BlockBox).
 * The region is marked with two blocks representing the bounding box of the area.
 */
public final class CuboidRegion extends MarkedRegion {

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

    public CuboidRegion(String name, ResourceKey<Level> dim, String parentName, Map<String, IFlag> flags,
                        boolean isActive, boolean isMuted, int priority, IMarkableArea area, BlockPos blockPos,
                        Map<String, PlayerContainer> groups, List<String> childrenNames){
        super(name, dim, parentName, flags, isActive, isMuted, priority, AreaType.CUBOID.areaType, area, blockPos, groups, childrenNames);
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        super.deserializeNBT(nbt);
        Optional<CompoundTag> compound = nbt.getCompound(RegionNbtKeys.AREA);
        compound.ifPresent(compoundTag -> this.area = new CuboidArea(compoundTag));
    }
}
