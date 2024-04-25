package de.z0rdak.yawp.core.region;

import de.z0rdak.yawp.managers.data.region.RegionDataManager;
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

    public DimensionalRegion(RegistryKey<World> dimensionKey) {
        super(dimensionKey.location().toString(), dimensionKey, RegionType.DIMENSION);
        this.dimension = dimensionKey;
    }

    public DimensionalRegion(RegistryKey<World> dimensionKey, IProtectedRegion parent) {
        super(dimensionKey.location().toString(), dimensionKey, RegionType.DIMENSION);
        this.dimension = dimensionKey;
        if (! (parent instanceof GlobalRegion)) {
            throw new IllegalArgumentException("Illegal parent region for dimensional region");
        }
        this.setParent(parent);
    }

    public DimensionalRegion(CompoundNBT nbt) {
        super(nbt);
        this.parent = RegionDataManager.get().getGlobalRegion();
        this.deserializeNBT(nbt);
    }

    public DimensionalRegion(String dimensionKey) {
        this(RegistryKey.create(Registry.DIMENSION_REGISTRY, new ResourceLocation(dimensionKey)));
        this.parent = RegionDataManager.get().getGlobalRegion();
    }

    @Override
    public boolean setParent(IProtectedRegion parent) {
        if (parent.getRegionType() == RegionType.GLOBAL) {
            return super.setParent(parent);
        }
        return false;
    }

    @Override
    public boolean addChild(IProtectedRegion child) {
        if (child.getRegionType() == RegionType.LOCAL && child.getParent() == null) {
            String parentName = child.getParentName();
            if (parentName != null && !parentName.equals(this.getName())) {
                super.addChild(child);
                ((AbstractRegion) child).parentName = parentName;
                return true;
            }
            return super.addChild(child);
        }
        if (child.getRegionType() == RegionType.LOCAL && child.getParent().getRegionType() == RegionType.DIMENSION) {
            return super.addChild(child);
        }
        if (child.getRegionType() == RegionType.LOCAL && !child.getParent().hasChild(child)) {
            return super.addChild(child);
        }
        return false;
    }

    @Override
    public CompoundNBT serializeNBT() {
        return super.serializeNBT();
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        super.deserializeNBT(nbt);
    }

    @Override
    public String getName() {
        return this.dimension.location().toString();
    }
}
