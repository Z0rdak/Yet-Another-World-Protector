package de.z0rdak.regionshield.common.core.region;

import de.z0rdak.regionshield.common.core.area.SphereArea;
import de.z0rdak.regionshield.common.util.constants.RegionNBT;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

public class SphericalRegion extends AbstractMarkableRegion {

    public SphericalRegion(CompoundNBT nbt) {
        this.deserializeNBT(nbt);
    }

    public SphericalRegion(String name, SphereArea area, RegistryKey<World> dimension) {
        this(name, area, area.getCenterP(), dimension);
    }

    public SphericalRegion(String name, SphereArea area, BlockPos tpPos, RegistryKey<World> dimension) {
        super(name, area, dimension);
        this.tpTarget = tpPos;
    }

    @Override
    public boolean containsPosition(BlockPos position) {
        return this.area.contains(position);
    }

    @Override
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = super.serializeNBT();
        nbt.put(RegionNBT.AREA, this.area.serializeNBT());
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        super.deserializeNBT(nbt);
        this.area = new SphereArea(nbt.getCompound(RegionNBT.AREA));
    }
}
