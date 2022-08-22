package de.z0rdak.yawp.core.region;

import de.z0rdak.yawp.util.constants.RegionNBT;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.registry.Registry;
import net.minecraft.world.World;

/**
 * The DimensionalRegion represents the only direct implementation of an Abstract region.
 * It is intended to be used to protect dimensions (vanilla and modded).
 */
public final class DimensionalRegion extends AbstractRegion {

    public static final int DEFAULT_PRIORITY = Integer.MIN_VALUE;

    private RegistryKey<World> dimensionKey;

    public DimensionalRegion(RegistryKey<World> dimensionKey) {
        super(dimensionKey.location().toString());
        this.dimensionKey = dimensionKey;
    }

    public DimensionalRegion(CompoundNBT nbt) {
        super("");
        this.deserializeNBT(nbt);
    }

    public DimensionalRegion(String dimensionKey) {
        this(RegistryKey.create(Registry.DIMENSION_REGISTRY, new ResourceLocation(dimensionKey)));
    }

    public RegistryKey<World> getDimensionKey() {
        return dimensionKey;
    }

    @Override
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = super.serializeNBT();
        nbt.putString(RegionNBT.DIM, this.dimensionKey.location().toString());
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        super.deserializeNBT(nbt);
        String dim = nbt.getString(RegionNBT.DIM);
        this.dimensionKey = RegistryKey.create(Registry.DIMENSION_REGISTRY, new ResourceLocation(dim));
    }

    @Override
    public String getName() {
        return this.dimensionKey.location().toString();
    }
}
