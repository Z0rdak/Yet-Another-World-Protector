package de.z0rdak.yawp.core.region;

import de.z0rdak.yawp.core.area.CylinderArea;
import de.z0rdak.yawp.util.constants.RegionNBT;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

public final class CylinderRegion extends AbstractMarkableRegion {

    public CylinderRegion(CompoundNBT nbt) {
        super(nbt);
        this.deserializeNBT(nbt);
    }

    public CylinderRegion(String name, CylinderArea area, PlayerEntity owner, RegistryKey<World> dimension) {
        this(name, area, new BlockPos(area.getCenter()), owner, dimension);
    }

    public CylinderRegion(String name, CylinderArea area, BlockPos tpPos, PlayerEntity owner, RegistryKey<World> dimension) {
        super(name, area, tpPos, owner, dimension);
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        super.deserializeNBT(nbt);
        this.area = new CylinderArea(nbt.getCompound(RegionNBT.AREA));
    }
}
