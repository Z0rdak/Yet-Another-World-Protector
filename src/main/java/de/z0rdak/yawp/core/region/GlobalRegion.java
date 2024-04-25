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
        super.setParent(this);
    }

    @Override
    public Map<String, IProtectedRegion> getChildren() {
        Map<String, IProtectedRegion> childrenWithoutGlobal = super.getChildren().entrySet().stream()
                .filter(e -> e.getValue().getRegionType() != RegionType.GLOBAL)
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
        return Collections.unmodifiableMap(childrenWithoutGlobal);
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
        if (child.getRegionType() == RegionType.DIMENSION) {
            return super.addChild(child);
        }
        return false;
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
