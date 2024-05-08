package de.z0rdak.yawp.core.region;

import net.minecraft.core.HolderLookup;
import net.minecraft.core.registries.Registries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

public class GlobalRegion extends AbstractRegion {

    public static final ResourceLocation GLOBAL = new ResourceLocation("yawp", "global");
    public static final ResourceKey<Level> GLOBAL_DIMENSION = ResourceKey.create(Registries.DIMENSION, GLOBAL);

    protected GlobalRegion(CompoundTag nbt) {
        super(nbt);
    }

    public GlobalRegion() {
        this("global", RegionType.GLOBAL);
    }

    protected GlobalRegion(String name, RegionType type) {
        super(name, GLOBAL_DIMENSION, type);
    }

    protected GlobalRegion(String name, RegionType regionType, Player owner) {
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
    public CompoundTag serializeNBT(HolderLookup.Provider provider) {
        CompoundTag nbt = super.serializeNBT(provider);
        nbt.putBoolean("global", true);
        return nbt;
    }

    @Override
    public void deserializeNBT(HolderLookup.Provider provider, CompoundTag nbt) {
        super.deserializeNBT(provider, nbt);
        boolean isGlobal = nbt.getBoolean("global");
    }
}
