package de.z0rdak.yawp.core.region;

import net.minecraft.nbt.NbtCompound;
import net.minecraft.registry.RegistryKey;
import net.minecraft.registry.RegistryKeys;
import net.minecraft.util.Identifier;
import net.minecraft.world.World;

/**
 * The DimensionalRegion represents the only direct implementation of an Abstract region.
 * It is intended to be used to protect dimensions (vanilla and modded).
 */
public final class DimensionalRegion extends AbstractRegion {

    public DimensionalRegion(RegistryKey<World> dimensionKey) {
        super(dimensionKey.getValue().toString(), RegionType.DIMENSION);
        this.dimension = dimensionKey;
    }

    public DimensionalRegion(RegistryKey<World> dimensionKey, IProtectedRegion parent) {
        super(dimensionKey.getValue().toString(), RegionType.DIMENSION);
        this.dimension = dimensionKey;
        if (!(parent instanceof GlobalRegion)) {
            throw new IllegalArgumentException("Illegal parent region for dimensional region");
        }
        this.setParent(parent);
    }

    public DimensionalRegion(NbtCompound nbt) {
        super(nbt);
        // TODO: Set global region parent
        this.deserializeNBT(nbt);
    }

    public DimensionalRegion(String dimensionKey) {
        this(RegistryKey.of(RegistryKeys.WORLD, Identifier.of(dimensionKey)));
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
    public NbtCompound serializeNBT() {
        return super.serializeNBT();
    }

    @Override
    public void deserializeNBT(NbtCompound nbt) {
        super.deserializeNBT(nbt);
    }

    @Override
    public String getName() {
        return this.dimension.getValue().toString();
    }
}
