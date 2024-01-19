package de.z0rdak.yawp.core.region;

import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.registry.Registry;
import net.minecraft.world.World;

public class GlobalRegion extends AbstractRegion {

    public static final ResourceLocation GLOBAL = new ResourceLocation("yawp", "global");
    public static final RegistryKey<World> GLOBAL_DIMENSION = RegistryKey.create(Registry.DIMENSION_REGISTRY, GLOBAL);

    public GlobalRegion(CompoundNBT nbt) {
        super(nbt);
    }

    public GlobalRegion() {
        this(GLOBAL.toString(), RegionType.GLOBAL);
    }

    protected GlobalRegion(String name, RegionType type) {
        super(name, GLOBAL_DIMENSION, type);
        this.parent = this;
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
