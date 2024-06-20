package de.z0rdak.yawp.core.region;


import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.NbtCompound;
import net.minecraft.registry.RegistryKey;
import net.minecraft.registry.RegistryKeys;
import net.minecraft.util.Identifier;
import net.minecraft.world.World;


public class GlobalRegion extends AbstractRegion {

    public static final Identifier GLOBAL = Identifier.of("yawp", "global");
    public static final RegistryKey<World> GLOBAL_DIMENSION = RegistryKey.of(RegistryKeys.WORLD, GLOBAL);

    protected GlobalRegion(NbtCompound nbt) {
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
    public boolean setParent(IProtectedRegion parent) {
        throw new IllegalRegionStateException("Attempt to set parent for global region");
    }

    @Override
    public IProtectedRegion getParent() {
        return null;
    }

    @Override
    public NbtCompound serializeNBT() {
        NbtCompound nbt = super.serializeNBT();
        nbt.putBoolean("global", true);
        return nbt;
    }

    @Override
    public void deserializeNBT(NbtCompound nbt) {
        super.deserializeNBT(nbt);
        boolean isGlobal = nbt.getBoolean("global");
    }
}
