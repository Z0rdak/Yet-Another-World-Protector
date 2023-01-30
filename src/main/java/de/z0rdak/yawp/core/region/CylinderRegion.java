package de.z0rdak.yawp.core.region;

import de.z0rdak.yawp.core.area.VerticalCylinderArea;
import de.z0rdak.yawp.util.constants.RegionNBT;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.NbtCompound;
import net.minecraft.registry.RegistryKey;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

public final class CylinderRegion extends AbstractMarkableRegion {

    public CylinderRegion(NbtCompound nbt) {
        super(nbt);
        this.deserializeNBT(nbt);
    }

    public CylinderRegion(String name, VerticalCylinderArea area, PlayerEntity owner, RegistryKey<World> dimension) {
        this(name, area, new BlockPos(area.getCenter().getX(), area.getCenter().getY(), area.getCenter().getZ()), owner, dimension);
    }

    public CylinderRegion(String name, VerticalCylinderArea area, BlockPos tpPos, PlayerEntity owner, RegistryKey<World> dimension) {
        super(name, area, tpPos, owner, dimension);
    }

    @Override
    public void deserializeNBT(NbtCompound nbt) {
        super.deserializeNBT(nbt);
        this.area = new VerticalCylinderArea(nbt.getCompound(RegionNBT.AREA));
    }
}
