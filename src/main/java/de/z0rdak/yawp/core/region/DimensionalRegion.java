package de.z0rdak.yawp.core.region;

import de.z0rdak.yawp.util.constants.RegionNBT;
import net.minecraft.core.Registry;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.Level;

/**
 * The DimensionalRegion represents the only direct implementation of an Abstract region.
 * It is intended to be used to protect dimensions (vanilla and modded).
 */
public final class DimensionalRegion extends AbstractRegion {

    public static final int DEFAULT_PRIORITY = Integer.MIN_VALUE;

    private ResourceKey<Level> dimensionKey;

    public DimensionalRegion(ResourceKey<Level> dimensionKey) {
        super(dimensionKey.location().toString(), RegionType.DIMENSION);
        this.dimensionKey = dimensionKey;
    }

    public DimensionalRegion(CompoundTag nbt) {
        super(nbt);
        this.deserializeNBT(nbt);
    }

    public DimensionalRegion(String dimensionKey) {
        this(ResourceKey.create(Registry.DIMENSION_REGISTRY, new ResourceLocation(dimensionKey)));
    }

    public ResourceKey<Level> getDimensionKey() {
        return dimensionKey;
    }

    @Override
    public CompoundTag serializeNBT() {
        CompoundTag nbt = super.serializeNBT();
        nbt.putString(RegionNBT.DIM, this.dimensionKey.location().toString());
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        super.deserializeNBT(nbt);
        String dim = nbt.getString(RegionNBT.DIM);
        this.dimensionKey = ResourceKey.create(Registry.DIMENSION_REGISTRY, new ResourceLocation(dim));
    }

    @Override
    public String getName() {
        return this.dimensionKey.location().toString();
    }
}
