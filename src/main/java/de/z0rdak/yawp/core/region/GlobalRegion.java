package de.z0rdak.yawp.core.region;

import net.minecraft.core.Registry;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;

import java.util.Collections;
import java.util.Map;
import java.util.stream.Collectors;

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
        this.parentName = GLOBAL.toString();
    }

    @Override
    public Map<String, IProtectedRegion> getChildren() {
        Map<String, IProtectedRegion> childrenWithoutGlobal = this.children.entrySet().stream()
                .filter(e -> e.getValue().getRegionType() != RegionType.GLOBAL)
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
        return Collections.unmodifiableMap(childrenWithoutGlobal);
    }

    @Override
    public CompoundTag serializeNBT() {
        CompoundTag nbt = super.serializeNBT();
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        super.deserializeNBT(nbt);
    }
}
