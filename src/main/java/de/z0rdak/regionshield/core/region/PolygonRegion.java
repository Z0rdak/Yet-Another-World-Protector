package de.z0rdak.regionshield.core.region;

import de.z0rdak.regionshield.core.area.IMarkableArea;
import de.z0rdak.regionshield.core.area.Polygon3DArea;
import de.z0rdak.regionshield.core.area.SphereArea;
import de.z0rdak.regionshield.util.constants.RegionNBT;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

public class PolygonRegion extends AbstractMarkableRegion {

    public PolygonRegion(CompoundNBT nbt) {
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
