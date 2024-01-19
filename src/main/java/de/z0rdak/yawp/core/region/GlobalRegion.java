package de.z0rdak.yawp.core.region;

import net.minecraft.core.Registry;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;

public class GlobalRegion extends AbstractRegion {

    public static final ResourceLocation GLOBAL = new ResourceLocation("yawp", "global");
    public static final ResourceKey<Level> GLOBAL_DIMENSION = ResourceKey.create(Registry.DIMENSION_REGISTRY, GLOBAL);

    public GlobalRegion(CompoundTag nbt) {
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
    public CompoundTag serializeNBT() {
        CompoundTag nbt = super.serializeNBT();
        nbt.putBoolean("global", true);
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        super.deserializeNBT(nbt);
        boolean isGlobal = nbt.getBoolean("global");
    }
}
