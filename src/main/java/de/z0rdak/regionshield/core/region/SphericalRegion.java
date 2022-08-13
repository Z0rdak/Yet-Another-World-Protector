package de.z0rdak.regionshield.core.region;

import de.z0rdak.regionshield.core.area.SphereArea;
import de.z0rdak.regionshield.util.constants.RegionNBT;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

public class SphericalRegion extends AbstractMarkableRegion {

    public SphericalRegion(CompoundNBT nbt) {
        this.deserializeNBT(nbt);
    }

    public SphericalRegion(String name, SphereArea area, PlayerEntity player, RegistryKey<World> dimension) {
        super(name, area, player, dimension);
    }

    public SphericalRegion(String name, SphereArea area, BlockPos tpPos, PlayerEntity player, RegistryKey<World> dimension) {
        super(name, area, tpPos, player, dimension);
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        super.deserializeNBT(nbt);
        this.area = new SphereArea(nbt.getCompound(RegionNBT.AREA));
    }
}
