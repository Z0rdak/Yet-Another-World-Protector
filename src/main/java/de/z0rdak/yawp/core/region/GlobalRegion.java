package de.z0rdak.yawp.core.region;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.registry.Registry;
import net.minecraft.world.World;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

public class GlobalRegion extends AbstractRegion {

    public static final ResourceLocation GLOBAL = new ResourceLocation("yawp", "global");
    public static final RegistryKey<World> GLOBAL_DIMENSION = RegistryKey.create(Registry.DIMENSION_REGISTRY, GLOBAL);

    protected GlobalRegion(CompoundNBT nbt) {
        super(nbt);
    }

    public GlobalRegion() {
        this("global", RegionType.GLOBAL);
    }

    protected GlobalRegion(String name, RegionType type) {
        super(name, GLOBAL_DIMENSION, type);
    }

    protected GlobalRegion(String name, RegionType regionType, PlayerEntity owner) {
        super(name, GLOBAL_DIMENSION, regionType, owner);
    }

    @Override
    public boolean setParent(@Nonnull IProtectedRegion parent) {
        throw new IllegalRegionStateException("Attempt to set parent for global region");
    }

    @Nullable
    @Override
    public IProtectedRegion getParent() {
        return null;
    }

    @Override
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = super.serializeNBT();
        nbt.putBoolean("global", true);
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        super.deserializeNBT(nbt);
        boolean isGlobal = nbt.getBoolean("global");
    }
}
