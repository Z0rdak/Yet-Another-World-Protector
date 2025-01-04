package de.z0rdak.yawp.core.region;

import de.z0rdak.yawp.constants.serialization.RegionNbtKeys;
import de.z0rdak.yawp.core.area.VerticalCylinderArea;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;

public final class CylinderRegion extends AbstractMarkableRegion {

    public CylinderRegion(CompoundTag nbt) {
        super(nbt);
        this.deserializeNBT(nbt);
    }

    public CylinderRegion(String name, VerticalCylinderArea area, Player owner, ResourceKey<Level> dimension) {
        this(name, area, new BlockPos(area.getCenter().getX(), area.getCenter().getY(), area.getCenter().getZ()), owner, dimension);
    }

    public CylinderRegion(String name, VerticalCylinderArea area, BlockPos tpPos, Player owner, ResourceKey<Level> dimension) {
        super(name, area, tpPos, owner, dimension);
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        super.deserializeNBT(nbt);
        this.area = new VerticalCylinderArea(nbt.getCompound(RegionNbtKeys.AREA));
    }
}
