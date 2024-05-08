package de.z0rdak.yawp.core.region;

import net.minecraft.core.HolderLookup;
import net.minecraft.core.registries.Registries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.Level;

/**
 * The DimensionalRegion represents the only direct implementation of an Abstract region.
 * It is intended to be used to protect dimensions (vanilla and modded).
 */
public final class DimensionalRegion extends AbstractRegion {

    public DimensionalRegion(ResourceKey<Level> dimensionKey) {
        super(dimensionKey.location().toString(), RegionType.DIMENSION);
        this.dimension = dimensionKey;
    }

    public DimensionalRegion(ResourceKey<Level> dimensionKey, IProtectedRegion parent) {
        super(dimensionKey.location().toString(), RegionType.DIMENSION);
        this.dimension = dimensionKey;
        if (!(parent instanceof GlobalRegion)) {
            throw new IllegalArgumentException("Illegal parent region for dimensional region");
        }
        this.setParent(parent);
    }

    public DimensionalRegion(CompoundTag nbt) {
        super(nbt);
        // TODO: Set global region parent
        this.deserializeNBT(provider, nbt);
    }

    public DimensionalRegion(String dimensionKey) {
        this(ResourceKey.create(Registries.DIMENSION, new ResourceLocation(dimensionKey)));
    }

    /**
     * A DimensionalRegion can by design only have a global region as its parent.
     *
     * @param parent the parent to set for this region.
     */
    @Override
    public boolean setParent(IProtectedRegion parent) {
        if (super.setParent(parent)) {
            return true;
        }
        if (!(parent instanceof GlobalRegion)) {
            throw new IllegalRegionStateException("Cannot set parent for dimensional region");
        }
        return true;
    }

    @Override
    public CompoundTag serializeNBT(HolderLookup.Provider provider) {
        return super.serializeNBT(provider);
    }

    @Override
    public void deserializeNBT(HolderLookup.Provider provider, CompoundTag nbt) {
        super.deserializeNBT(provider, nbt);
    }

    @Override
    public String getName() {
        return this.dimension.location().toString();
    }
}
