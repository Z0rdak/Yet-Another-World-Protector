package de.z0rdak.yawp.core.region;

import de.z0rdak.yawp.core.area.Polygon3DArea;
import de.z0rdak.yawp.util.constants.RegionNBT;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

public final class PolygonRegion extends AbstractMarkableRegion {

    public PolygonRegion(CompoundNBT nbt) {
        super(nbt);
        this.deserializeNBT(nbt);
    }

    public PolygonRegion(String name, Polygon3DArea area, PlayerEntity owner, RegistryKey<World> dimension) {
        super(name, area, owner, dimension);
    }

    public PolygonRegion(String name, Polygon3DArea area, BlockPos tpTarget, PlayerEntity owner, RegistryKey<World> dimension) {
        super(name, area, tpTarget, owner, dimension);
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        super.deserializeNBT(nbt);
        this.area = new Polygon3DArea(nbt.getCompound(RegionNBT.AREA));
    }
}
